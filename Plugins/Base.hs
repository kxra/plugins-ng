	{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-

This module provides a system which can watch one or more symbols
which are loaded from local modules. If the files containing those
modules are changed, then the code is automatically recompiled and
loaded into the running process.

-}


module Plugins.Base where

import DynFlags (defaultFatalMessager, defaultFlushOut)
import GHC
import GHC.Paths (libdir)
import GhcMonad                   (liftIO) -- from ghc7.7 and up you can use the usual
import Language.Haskell.TH.Syntax as TH (Name(Name),NameFlavour(NameG), NameSpace(VarName), OccName(..), ModName(..))
import Module (moduleNameSlashes)

{-

We need to watch a bunch of files and reload.

We need a way to map from a specific symbol to it's loaded value.

What happens when there is an error?

What happens when there are multiple symbols from the same file?

When a module is reloaded how do we ensure all the symbols get reloaded?

There are two phases:

 1. reloading all the modules

 2. evaluating the symbols

We can start with the brute force variant -- when any module is touched, we just reload everything.

-}

------------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------------

-- | extract the module name and occurance name of a symbol
--
-- can fail if the 'Name' is not for the right type of thing.
-- BASE
nameToModFunc :: TH.Name -> (ModuleName, String)
nameToModFunc (Name (OccName occName) (NameG VarName _ (ModName mn))) =
    (mkModuleName mn, occName)
nameToModFunc n = error $ "nameToModFunc failed because Name was not the right kind. " ++ show n

-- | wrapper for calling a Ghc action
--
-- defaults to 'HscAsm' and 'LinkInMemory'
-- BASE
withSession' :: Ghc a -> IO a
withSession' action =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscAsm
                                    , ghcLink   = LinkInMemory
                                    }
        action



------------------------------------------------------------------------------
-- PluginsHandle
------------------------------------------------------------------------------

-- | set the list of modules that GHC should load
-- BASE
setTargets' :: [(ModuleName, String)] -> Ghc ()
setTargets' syms =
    do targets <- mapM (\(mod,_) -> (liftIO $ print $ moduleNameString mod) >> guessTarget (moduleNameSlashes mod) Nothing) syms
       setTargets targets
       return ()

-- | load the modules+symbols
-- BASE
loadSyms :: [(ModuleName, String)] -> Ghc [HValue]
loadSyms syms =
    do res <- load LoadAllTargets

       -- Bringing the module into the context
       setContext (map (IIDecl . simpleImportDecl . fst) syms)

       let symNames = map (\(modName, symName) -> moduleNameString modName ++ "." ++ symName) syms
       liftIO $ print symNames
       mapM compileExpr symNames
