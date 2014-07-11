{-# LANGUAGE OverloadedStrings #-}  -- For FilePath
{-# LANGUAGE TemplateHaskell #-}

module Plugins.Filewatch where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Data.Monoid (mempty)
import Data.String (fromString)
import Filesystem.Path (FilePath, dirname, filename)
import GHC
import GhcMonad                   (liftIO) -- from ghc7.7 and up you can use the usual
import Language.Haskell.TH.Syntax as TH (Name(Name))
import System.FSNotify
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (FilePath)
import Language.Haskell.TH          (ExpQ, appE, varE)
import Language.Haskell.TH.Lift     (lift)

import Plugins.Base

-- | predicate: event caused by file being added
-- filewatch
isAdded :: Event -> Bool
isAdded (Added {}) = True
isAdded _          = False

-- | predicate: event caused by file being modified
-- filewatch
isModified :: Event -> Bool
isModified (Modified {}) = True
isModified _             = False

-- | watch a single file for changes
-- filewatch
watchFile :: WatchManager -> (FilePath, FilePath) -> IO () -> IO ()
watchFile wm (dir, file) action =
    watchDir wm dir
                 (\e -> filename (eventPath e) == file)
                 (\e -> if (isAdded e || isModified e)
                          then action
                          else return ())

-- | watch a bunch of files for changes
-- filewatch
watchFiles :: WatchManager -> [FilePath] -> IO () -> IO ()
watchFiles wm fps action =
    do let pairs = Map.toList . Map.fromListWith (++) . map (\(x,y) -> (if x == mempty then "." else x,[y])) $ map splitFileName fps
       print pairs
       mapM_ watchFiles' pairs
    where
      splitFileName fp = (dirname fp, filename fp)
      watchFiles' :: (FilePath, [FilePath]) -> IO ()
      watchFiles' (dir, files) =
          watchDir wm dir
                   (\e -> filename (eventPath e) `elem` files)
                   (\e -> if (isAdded e || isModified e)
                          then action
                          else return ())

------------------------------------------------------------------------------
-- PluginsHandle
------------------------------------------------------------------------------

-- file-watch
data PluginsHandle = PluginsHandle
    { phWatchManager :: TMVar WatchManager
    , phSymMap         :: TMVar (Map TH.Name HValue)
    }

-- | create a new, empty 'PluginsHandle'
-- file-watch
newPluginsHandle :: IO PluginsHandle
newPluginsHandle =
    PluginsHandle <$> (newTMVarIO =<< startManager) <*> newTMVarIO Map.empty

-- | recompile and reload modified modules currently in the watch
-- list. Also update the watch list based on the new dependency graph.
--
-- FIXME: we probably need some form of semaphore here to protect use against multiple simultaneous calls
-- file-watch
reload :: PluginsHandle
       -> [TH.Name]
       -> IO ()
reload ph newSyms =
    do m <- atomically $ takeTMVar (phSymMap ph)
       m' <- withSession' $ do
         let names = (Map.keys m) ++ newSyms
             syms = map nameToModFunc names
         setTargets' syms
         vals <- loadSyms syms
         updateWatches ph
         return $ Map.fromList $ zip names vals
       atomically $ putTMVar (phSymMap ph) m'

-- | look at the current module graph and update the list of watched
-- files accordingly.
-- file-watch
updateWatches :: PluginsHandle
              -> Ghc ()
updateWatches ph =
    do wm <- liftIO $ do
         newWM <- startManager
         oldWM <- atomically $ do old <- takeTMVar (phWatchManager ph)
                                  putTMVar (phWatchManager ph) newWM
                                  return old
         stopManager oldWM
         return newWM
       modGraph <- getModuleGraph
       let files = catMaybes $ map (ml_hs_file . ms_location) modGraph
       liftIO $ do putStr "Now watching: "
                   print files
                   watchFiles wm (map fromString files) (reload ph [])

-- | look up the symbol refered to by 'TH.Name' and call
-- 'unsafeCoerce' to cast it to type 'a'.
--
-- see also: 'lookupName'
-- 
-- file-watch
unsafeLookupName :: TH.Name
               -> a
               -> PluginsHandle
               -> IO a
unsafeLookupName n _ ph =
    do sym <- atomically $ do m <- readTMVar (phSymMap ph)
                              case Map.lookup n m of
                                Nothing  -> error "Invalid name"
                                (Just s) -> return s
       return $ unsafeCoerce sym

-- | TH to safely lookup a symbol
--
-- generates a function like:
--
-- lookupName :: TH.Name -> PluginsHandle -> IO a
--
-- where the type 'a' is derived from the actual type of the symbol
-- refered to by 'Name' which must be in scope at compile time.
-- file-watch
lookupName :: TH.Name -> ExpQ
lookupName name =
    appE (appE [| unsafeLookupName |] (lift name)) (varE name)
