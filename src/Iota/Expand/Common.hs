-- | What the expander modules share. The context record, the syntax
-- error helper, aliases into the implementation environment, shape
-- checks for formals and bindings, and reading source files.
module Iota.Expand.Common
  ( -- * Contexts
    Ex (..)
  , Runner
  , withLoc

    -- * Errors
  , syntaxError

    -- * Helpers
  , implRef
  , Head (..)
  , headOf
  , useMacro
  , beginOf
  , listOf
  , formArgs
  , parseFormals
  , bindingPairs
  , nameLambda

    -- * Special forms
  , specialForms
  , installSpecialForms
  , featureSymbols

    -- * Files
  , readSourceFile
  , readSourceDatums
  , includeFiles
  , foldSymbols
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Iota.Reader (readDatums)
import Iota.Syntax
import Iota.SyntaxRules (expandMacro)
import Iota.Types
import Iota.Value
import System.FilePath ((</>))

-- | Runs top level forms in an environment. The library code needs this
-- for begin declarations, but the real function lives in "Iota.Expand",
-- which imports the library module. Carrying it in the context avoids an
-- hs-boot file.
type Runner = Ex -> GlobalEnv -> [Datum] -> IO ()

-- | Expander context. Everything except the syntactic environment.
data Ex = Ex
  { exRt :: Runtime
  , -- | Where include resolves relative paths.
    exDir :: FilePath
  , -- | Position of the top level form being expanded, for error messages.
    exLoc :: Maybe SrcLoc
  , exRun :: Runner
  }

withLoc :: SrcLoc -> Ex -> Ex
withLoc loc ex = ex {exLoc = Just loc}

syntaxError :: Text -> Datum -> IO a
syntaxError what form = do
  v <- datumToValue (stripSyntax form)
  throwScheme (BadSyntax what v)

-- | A fresh alias for a name in the implementation environment. The
-- derived form rewrites are built out of these. A user who rebinds if or
-- memv locally does not change what case expands into.
implRef :: Ex -> Text -> IO Datum
implRef ex name = freshAlias (topEnv (rtImpl (exRt ex))) (DSymbol (Symbol name))

data Head
  = HSpecial Symbol
  | HMacro Macro
  | HOther

headOf :: SynEnv -> Datum -> IO Head
headOf env form = case form of
  DList (h : _) _ | isIdent h ->
    resolve env h >>= \case
      Just (DenSpecial s) -> pure (HSpecial s)
      Just (DenMacro m) -> pure (HMacro m)
      _ -> pure HOther
  _ -> pure HOther

useMacro :: SynEnv -> Macro -> Datum -> IO Datum
useMacro env m form =
  expandMacro m env form >>= \case
    Right d -> pure d
    Left err -> throwScheme (MacroError err (stripSyntax form))

-- | Wrap forms in a begin that means begin whatever the user has bound.
beginOf :: Ex -> [Datum] -> IO Datum
beginOf ex ds = do
  b <- implRef ex "begin"
  pure (listOf (b : ds))

listOf :: [Datum] -> Datum
listOf xs = DList xs Nothing

formArgs :: Datum -> [Datum]
formArgs (DList (_ : args) Nothing) = args
formArgs _ = []

-- | Lambda formals. The required parameters and the rest parameter, if any.
parseFormals :: Datum -> IO ([Datum], Maybe Datum)
parseFormals formals = do
  (ps, rest) <- case formals of
    DList ps tl -> pure (ps, tl)
    d | isIdent d -> pure ([], Just d)
    _ -> syntaxError "lambda formals" formals
  unless (all isIdent ps && maybe True isIdent rest) $ syntaxError "lambda formals" formals
  let keys = map identKey (ps <> maybe [] pure rest)
  when (hasDuplicates keys) $ syntaxError "lambda formals (duplicate parameter)" formals
  pure (ps, rest)
  where
    hasDuplicates xs = length xs /= Map.size (Map.fromList (zip xs (repeat ())))

-- | ((name expr) ...) as pairs.
bindingPairs :: [Datum] -> IO [(Datum, Datum)]
bindingPairs = mapM pair
  where
    pair b = case b of
      DList [n, e] Nothing | isIdent n -> pure (n, e)
      _ -> syntaxError "binding" b

-- | An anonymous lambda bound to a variable takes the variable's name.
-- Only affects what #<procedure foo> prints.
nameLambda :: Symbol -> Core -> Core
nameLambda n c@CLambda {lamName = "lambda"} = c {lamName = n}
nameLambda _ c = c

-- | Every keyword the expander recognises. They are bound as DenSpecial
-- in the implementation environment and re-exported by the standard
-- libraries.
specialForms :: [Symbol]
specialForms =
  [ "quote", "quasiquote", "unquote", "unquote-splicing"
  , "lambda", "named-lambda", "if", "set!", "define", "begin"
  , "let", "let*", "letrec", "letrec*", "let-values", "let*-values", "define-values"
  , "define-syntax", "let-syntax", "letrec-syntax", "syntax-rules", "syntax-error"
  , "and", "or", "when", "unless", "cond", "case", "do", "else", "=>", "...", "_"
  , "delay", "delay-force", "guard", "parameterize", "case-lambda"
  , "define-record-type", "cond-expand", "include", "include-ci"
  , "import", "define-library", "export", "include-library-declarations"
  ]

installSpecialForms :: GlobalEnv -> IO ()
installSpecialForms env = forM_ specialForms $ \s -> globalBind env s (DenSpecial s)

-- | Feature identifiers. cond-expand and (features) both use this list.
featureSymbols :: [Symbol]
featureSymbols = ["r7rs", "exact-closed", "ratios", "ieee-float", "full-unicode", "iota"]

-- | Source files are always UTF-8. The locale does not get a say.
readSourceFile :: FilePath -> IO Text
readSourceFile path = do
  r <- try (BS.readFile path)
  case r of
    Left (e :: IOException) -> throwScheme (FileError (T.pack (show e)))
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _ -> throwScheme (FileError (T.pack path <> ": not valid UTF-8"))
      Right t -> pure t

readSourceDatums :: FilePath -> IO [Datum]
readSourceDatums path = do
  src <- readSourceFile path
  case readDatums path src of
    Left err -> throwScheme (ParseFailure err)
    Right ds -> pure ds

-- | include. Paths are relative to the including file, not the working
-- directory.
includeFiles :: Ex -> Bool -> [Datum] -> IO [Datum]
includeFiles ex foldCase files = concat <$> mapM one files
  where
    one = \case
      DString name -> do
        let path = exDir ex </> T.unpack name
        ds <- readSourceDatums path
        pure (if foldCase then map foldSymbols ds else ds)
      d -> syntaxError "include (expected a file name)" d

-- | include-ci folds identifiers only. Strings and characters stay as
-- they are.
foldSymbols :: Datum -> Datum
foldSymbols = \case
  DSymbol (Symbol t) -> DSymbol (Symbol (T.toCaseFold t))
  DList ds tl -> DList (map foldSymbols ds) (fmap foldSymbols tl)
  DVector ds -> DVector (map foldSymbols ds)
  DLabel n d -> DLabel n (foldSymbols d)
  d -> d
