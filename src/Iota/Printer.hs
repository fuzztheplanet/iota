-- | Values back to text. write, write-shared, write-simple and display
-- are one traversal with two settings. One says whether strings and
-- characters get quoted. The other says which objects get a @#n=@ label,
-- none, cycles only, or everything that is shared.
module Iota.Printer
  ( -- * Values
    writeText
  , writeSharedText
  , writeSimpleText
  , displayText
  , Style (..)
  , renderValue

    -- * Data
  , writeDatum

    -- * Errors
  , formatError
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isAlphaNum, isPrint, ord)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as UMV
import Data.Word (Word8)
import Iota.Number qualified as N
import Iota.Reader (numberFromText)
import Iota.Types
import Iota.Value (identBase, stringText)
import Numeric (showHex)

data Style = Style
  { -- | write quotes, display does not.
    styleMachine :: !Bool
  , -- | Nothing means no labels. Just False labels cycles. Just True
    -- labels everything reachable twice.
    styleLabels :: !(Maybe Bool)
  }

writeText :: Value -> Eval Text
writeText = renderValue (Style True (Just False))

writeSharedText :: Value -> Eval Text
writeSharedText = renderValue (Style True (Just True))

-- | No labels at all, so this loops on cyclic data. The report allows
-- that.
writeSimpleText :: Value -> Eval Text
writeSimpleText = renderValue (Style True Nothing)

displayText :: Value -> Eval Text
displayText = renderValue (Style False (Just False))

-- The shared traversal

renderValue :: Style -> Value -> Eval Text
renderValue style value = do
  toLabel <- case styleLabels style of
    Nothing -> pure IntSet.empty
    Just shared -> findLabels shared value
  counter <- newIORef (0 :: Int)
  assigned <- newIORef (IntMap.empty :: IntMap Int)
  out <- newIORef ([] :: [Text])
  let emit t = modifyIORef' out (t :)
      machine = styleMachine style

      -- Prints "#n=" or "#n#" if the object needs one. True means a back
      -- reference went out and the caller should stop.
      labelled :: Int -> Eval Bool
      labelled i
        | not (IntSet.member i toLabel) = pure False
        | otherwise = do
            seen <- readIORef assigned
            case IntMap.lookup i seen of
              Just n -> emit ("#" <> tshow n <> "#") >> pure True
              Nothing -> do
                n <- readIORef counter
                writeIORef counter (n + 1)
                modifyIORef' assigned (IntMap.insert i n)
                emit ("#" <> tshow n <> "=")
                pure False

      go :: Value -> Eval ()
      go v = case v of
        Null -> emit "()"
        Boolean True -> emit "#t"
        Boolean False -> emit "#f"
        Num n -> emit (N.numberToText10 n)
        Character c -> emit (if machine then writeChar c else T.singleton c)
        Sym s -> emit (if machine then writeSymbol s else symbolName s)
        Str {} -> do
          t <- stringText v
          emit (if machine then writeString t else t)
        Pair i _ _ -> do
          done <- labelled i
          unless done (renderPair v)
        Vec i mv -> do
          done <- labelled i
          unless done $ do
            emit "#("
            forM_ [0 .. MV.length mv - 1] $ \k -> do
              when (k > 0) (emit " ")
              MV.read mv k >>= go
            emit ")"
        Bytes _ mv -> do
          ws <- mapM (UMV.read mv) [0 .. UMV.length mv - 1]
          emit ("#u8(" <> T.unwords (map (T.pack . show) ws) <> ")")
        Proc p -> emit (renderProcedure p)
        PortVal p -> emit ("#<port " <> portName p <> ">")
        RecordVal r -> do
          done <- labelled (recordId r)
          unless done $ do
            emit ("#<" <> symbolName (rtdName (recordType r)))
            forM_ [0 .. MV.length (recordFields r) - 1] $ \k -> do
              emit " "
              MV.read (recordFields r) k >>= go
            emit ">"
        RecordTypeVal t -> emit ("#<record-type " <> symbolName (rtdName t) <> ">")
        PromiseVal _ -> emit "#<promise>"
        EnvVal e -> emit ("#<environment " <> globalName e <> ">")
        -- One value per line. Zero values print nothing.
        MultipleValues vs -> forM_ (zip [0 :: Int ..] vs) $ \(k, x) -> do
          when (k > 0) (emit "\n")
          go x
        ErrObj kind msg irritants -> do
          emit ("#<" <> errorKindName kind <> " " <> writeString msg)
          forM_ irritants $ \x -> emit " " >> go x
          emit ">"
        EofObject -> emit "#<eof>"
        Unspecified -> emit "#<unspecified>"
        Undefined -> emit "#<undefined>"

      -- (quote x) prints as 'x.
      renderPair v = do
        shorthand <- quoteAbbreviation v
        case shorthand of
          Just (prefix, inner) -> emit prefix >> go inner
          Nothing -> do
            emit "("
            elements v True
            emit ")"

      -- List elements. A cdr that needs a label has to go after a dot.
      -- "#1=" can only prefix a whole datum, not the middle of a list.
      elements v first = case v of
        Pair _ a d -> do
          unless first (emit " ")
          readIORef a >>= go
          rest <- readIORef d
          case rest of
            Null -> pure ()
            Pair j _ _
              | IntSet.member j toLabel -> emit " . " >> go rest
              | otherwise -> elements rest False
            other -> emit " . " >> go other
        _ -> pure ()

  go value
  T.concat . reverse <$> readIORef out
  where
    tshow :: Int -> Text
    tshow = T.pack . show

-- | Which objects need a label. A depth first walk that remembers the
-- current path. An object met again while still on the path closes a
-- cycle. With @shared@ on, any object met twice counts.
findLabels :: Bool -> Value -> Eval IntSet
findLabels shared root = do
  visited <- newIORef IntSet.empty
  labels <- newIORef IntSet.empty
  let visit onPath v = case v of
        Pair i a d -> node i onPath $ do
          readIORef a >>= visit (IntSet.insert i onPath)
          readIORef d >>= visit (IntSet.insert i onPath)
        Vec i mv -> node i onPath $
          forM_ [0 .. MV.length mv - 1] $ \k -> MV.read mv k >>= visit (IntSet.insert i onPath)
        RecordVal r -> node (recordId r) onPath $
          forM_ [0 .. MV.length (recordFields r) - 1] $ \k ->
            MV.read (recordFields r) k >>= visit (IntSet.insert (recordId r) onPath)
        MultipleValues vs -> mapM_ (visit onPath) vs
        ErrObj _ _ irritants -> mapM_ (visit onPath) irritants
        _ -> pure ()
      node i onPath descend = do
        seen <- readIORef visited
        if IntSet.member i seen
          then when (shared || IntSet.member i onPath) (modifyIORef' labels (IntSet.insert i))
          else do
            modifyIORef' visited (IntSet.insert i)
            descend
  visit IntSet.empty root
  readIORef labels

renderProcedure :: Procedure -> Text
renderProcedure = \case
  Primitive {..} -> "#<procedure " <> primName <> ">"
  PrimitiveK {..} -> "#<procedure " <> primName <> ">"
  Closure {..} -> "#<procedure " <> closureName <> ">"
  Continuation {} -> "#<continuation>"
  Parameter {} -> "#<parameter>"

errorKindName :: ErrorKind -> Text
errorKindName = \case
  ErrorGeneral -> "error"
  ErrorFile -> "file-error"
  ErrorRead -> "read-error"

-- | Only a two element @(quote x)@ qualifies. @(quote x y)@ prints as it
-- is.
quoteAbbreviation :: Value -> Eval (Maybe (Text, Value))
quoteAbbreviation (Pair _ a d) = do
  hd <- readIORef a
  tl <- readIORef d
  case (hd, tl) of
    (Sym s, Pair _ b rest) -> do
      rest' <- readIORef rest
      case rest' of
        Null -> do
          inner <- readIORef b
          pure (fmap (\prefix -> (prefix, inner)) (lookup s abbreviations))
        _ -> pure Nothing
    _ -> pure Nothing
quoteAbbreviation _ = pure Nothing

abbreviations :: [(Symbol, Text)]
abbreviations =
  [ ("quote", "'")
  , ("quasiquote", "`")
  , ("unquote", ",")
  , ("unquote-splicing", ",@")
  ]

-- Lexical syntax

writeString :: Text -> Text
writeString t = "\"" <> T.concatMap escapeChar t <> "\""
  where
    escapeChar = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\a' -> "\\a"
      '\b' -> "\\b"
      c
        | isPrint c -> T.singleton c
        | otherwise -> "\\x" <> T.pack (showHex (ord c) "") <> ";"

writeChar :: Char -> Text
writeChar c = case lookup c namedCharacters of
  Just name -> "#\\" <> name
  Nothing
    | isPrint c -> "#\\" <> T.singleton c
    | otherwise -> "#\\x" <> T.pack (showHex (ord c) "")

namedCharacters :: [(Char, Text)]
namedCharacters =
  [ ('\a', "alarm")
  , ('\b', "backspace")
  , ('\DEL', "delete")
  , ('\ESC', "escape")
  , ('\n', "newline")
  , ('\0', "null")
  , ('\r', "return")
  , (' ', "space")
  , ('\t', "tab")
  ]

-- | A symbol that would not read back as itself goes between bars. That
-- covers odd characters, the empty symbol, and names like @1@ or @.@
-- that the reader would take for something else.
writeSymbol :: Symbol -> Text
writeSymbol (Symbol t)
  | T.null t = "||"
  | T.all plain t && not looksLikeOtherDatum = t
  | otherwise = "|" <> T.concatMap barEscape t <> "|"
  where
    plain c = (isAlphaNum c && isPrint c) || c `elem` ("!$%&*+-./:<=>?@^_~" :: String)
    looksLikeOtherDatum = t == "." || maybe False (const True) (numberFromText 10 t)
    barEscape = \case
      '|' -> "\\|"
      '\\' -> "\\\\"
      c
        | isPrint c -> T.singleton c
        | otherwise -> "\\x" <> T.pack (showHex (ord c) "") <> ";"

-- Data

-- | The same for a 'Datum'. Used by the :ast command and by macro errors,
-- where there is no value yet. Aliases print as the name underneath.
writeDatum :: Datum -> Text
writeDatum = \case
  DBool True -> "#t"
  DBool False -> "#f"
  DNumber n -> N.numberToText10 n
  DChar c -> writeChar c
  DString t -> writeString t
  DSymbol s -> writeSymbol s
  d@(DAlias _) -> writeSymbol (identBase d)
  DList [DSymbol s, d] Nothing
    | Just prefix <- lookup s abbreviations -> prefix <> writeDatum d
  DList xs Nothing -> "(" <> T.unwords (map writeDatum xs) <> ")"
  DList xs (Just tl) -> "(" <> T.unwords (map writeDatum xs) <> " . " <> writeDatum tl <> ")"
  DVector xs -> "#(" <> T.unwords (map writeDatum xs) <> ")"
  DBytevector ws -> "#u8(" <> T.unwords (map showByte ws) <> ")"
  DLabel n d -> "#" <> T.pack (show n) <> "=" <> writeDatum d
  DLabelRef n -> "#" <> T.pack (show n) <> "#"
  where
    showByte :: Word8 -> Text
    showByte = T.pack . show

-- Errors

-- | Error messages for the REPL and the CLI. This runs in IO so it can
-- print the offending value in full. The Show instance on 'SchemeError'
-- cannot follow references and prints @#<pair>@ instead.
formatError :: SchemeError -> Eval Text
formatError = \case
  ParseFailure t -> pure ("read error:\n" <> T.stripEnd t)
  UnboundVariable s -> pure ("unbound variable: " <> symbolName s)
  NotApplicable v -> withValue "not applicable: " v
  ArityMismatch who ar n ->
    pure (who <> ": expected " <> describeArity ar <> ", got " <> T.pack (show n))
  TypeError expected v -> do
    t <- writeText v
    pure ("expected " <> expected <> ", got: " <> t)
  BadSyntax what form -> do
    t <- writeText form
    pure ("bad syntax in " <> what <> ": " <> t)
  DivideByZero who -> pure (who <> ": division by zero")
  IndexOutOfRange who i -> pure (who <> ": index out of range: " <> T.pack (show i))
  FileError t -> pure ("file error: " <> t)
  PortError t -> pure ("port error: " <> t)
  MacroError t d -> pure ("syntax error: " <> t <> ": " <> writeDatum d)
  ImportError t -> pure ("import error: " <> t)
  UserError msg irritants -> withIrritants msg irritants
  RaisedObject (ErrObj _ msg irritants) -> withIrritants msg irritants
  RaisedObject v -> withValue "uncaught exception: " v
  where
    withValue prefix v = (prefix <>) <$> writeText v
    withIrritants msg irritants = do
      parts <- mapM writeText irritants
      pure (msg <> T.concat (map (" " <>) parts))
