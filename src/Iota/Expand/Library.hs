-- | import, define-library and cond-expand. This lives inside the
-- expander because a library body has to run while the program that
-- imports it is still being expanded. The body goes through the Runner
-- carried in the context.
module Iota.Expand.Library
  ( importInto
  , environmentFrom
  , loadLibrary
  , defineLibrary
  , condExpand
  , featureRequirement
  ) where

import Control.Monad (foldM, forM_)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Iota.Expand.Common
import Iota.Library
import Iota.Syntax
import Iota.Types
import Iota.Value (identBase)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

-- | Copy the bindings of some import sets into an environment. Importing
-- the same denotation twice is fine. A different one under the same name
-- overwrites the earlier one.
importInto :: Ex -> GlobalEnv -> [Datum] -> IO ()
importInto ex genv specs = forM_ specs $ \spec -> do
  r <- resolveImportSet (loadLibrary ex) (stripSyntax spec)
  case r of
    Left err -> throwScheme (ImportError err)
    Right bindings -> forM_ (Map.toList bindings) $ \(name, den) ->
      globalLookup genv name >>= \case
        Just existing | sameDenotation existing den -> pure ()
        _ -> globalBind genv name den

-- | (environment spec ...).
environmentFrom :: Ex -> [Datum] -> IO GlobalEnv
environmentFrom ex specs = do
  genv <- newGlobalEnv "environment"
  importInto ex genv specs
  pure genv

-- | Look in the registry first. Failing that, look for foo/bar.iota next
-- to the importing file and along the library path. One extension for
-- everything. What makes the file a library is that it holds nothing but
-- define-library forms.
loadLibrary :: Ex -> LibName -> IO (Either Text Library)
loadLibrary ex name@(LibName parts) =
  lookupLibrary rt name >>= \case
    Just lib -> pure (Right lib)
    Nothing -> do
      let rel = foldr1 (</>) (map T.unpack parts)
          candidates =
            [ dir </> rel <> ext
            | dir <- exDir ex : rtLibraryPath rt
            , ext <- [".iota"]
            ]
      found <- firstExisting candidates
      case found of
        Nothing -> pure (Left ("library not found: " <> libNameText name))
        Just path -> do
          ds <- readSourceDatums path
          let ex' = ex {exDir = takeDirectory path, exLoc = Nothing}
          forM_ ds $ \d -> case d of
            DList (DSymbol "define-library" : _) Nothing -> defineLibrary ex' d
            _ -> throwScheme (ImportError ("library file " <> T.pack path <> " must contain only define-library forms"))
          lookupLibrary rt name >>= \case
            Just lib -> pure (Right lib)
            Nothing -> pure (Left ("file " <> T.pack path <> " does not define " <> libNameText name))
  where
    rt = exRt ex
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
      e <- doesFileExist p
      if e then pure (Just p) else firstExisting ps

-- | A fresh environment, the declarations in order, then register the
-- exports.
--
-- The body runs to completion right here. A continuation captured
-- inside it and called after the library is done re-enters a
-- computation nobody is waiting on. The report leaves that case to the
-- implementation and I have not done anything about it.
defineLibrary :: Ex -> Datum -> IO ()
defineLibrary ex form = case form of
  DList (_ : nameDatum : decls) Nothing -> do
    name <- either (throwScheme . ImportError) pure (libNameFromDatum (stripSyntax nameDatum))
    genv <- newGlobalEnv (libNameText name)
    exports <- newIORef []
    let decl d = case d of
          DList (kw : args) Nothing | isIdent kw -> case identBase kw of
            "export" -> modifyIORef' exports (<> args)
            "import" -> importInto ex genv args
            "begin" -> exRun ex ex genv args
            "include" -> includeFiles ex False args >>= exRun ex ex genv
            "include-ci" -> includeFiles ex True args >>= exRun ex ex genv
            "include-library-declarations" -> includeFiles ex False args >>= mapM_ decl
            "cond-expand" -> condExpand ex (topEnv genv) args >>= mapM_ decl
            _ -> syntaxError "library declaration" d
          _ -> syntaxError "library declaration" d
    mapM_ decl decls
    specs <- readIORef exports
    exported <- foldM (exportOne genv) Map.empty specs
    registerLibrary (exRt ex) Library {libName = name, libExports = exported}
  _ -> syntaxError "define-library" form
  where
    exportOne genv acc spec = do
      (internal, external) <- case spec of
        d | isIdent d -> pure (identBase d, identBase d)
        DList [r, a, b] Nothing | isIdent r, identBase r == "rename", isIdent a, isIdent b -> pure (identBase a, identBase b)
        _ -> syntaxError "export spec" spec
      globalLookup genv internal >>= \case
        Just den -> pure (Map.insert external den acc)
        Nothing -> throwScheme (ImportError ("exported name is not defined: " <> symbolName internal))

-- | The body of the first clause whose requirement holds. Empty if none
-- does.
condExpand :: Ex -> SynEnv -> [Datum] -> IO [Datum]
condExpand _ _ [] = pure []
condExpand ex env (clause : rest) = case clause of
  DList (req : body) Nothing -> do
    isElse <- isSpecial env req "else"
    ok <- if isElse then pure True else featureRequirement ex env req
    if ok then pure body else condExpand ex env rest
  _ -> syntaxError "cond-expand clause" clause

featureRequirement :: Ex -> SynEnv -> Datum -> IO Bool
featureRequirement ex env req = case req of
  _ | isIdent req -> pure (identBase req `elem` featureSymbols)
  DList (op : args) Nothing | isIdent op -> case identBase op of
    "and" -> and <$> mapM (featureRequirement ex env) args
    "or" -> or <$> mapM (featureRequirement ex env) args
    "not" | [r] <- args -> not <$> featureRequirement ex env r
    -- (library (foo)) tries to load the library. Heavy for a feature
    -- test, but anything else would be a guess.
    "library" | [name] <- args -> case libNameFromDatum (stripSyntax name) of
      Left _ -> pure False
      Right ln -> either (const False) (const True) <$> loadLibrary ex ln
    _ -> syntaxError "feature requirement" req
  _ -> syntaxError "feature requirement" req
