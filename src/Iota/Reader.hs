-- | The datum reader, written with megaparsec. Text in, 'Datum' out.
-- The only parser state is the fold-case flag, so the tests can compare
-- results with plain equality.
--
-- Identifiers and numbers are not told apart up front. An atom is a
-- maximal run of non-delimiter characters. If it parses as a number it
-- is a number, otherwise it is a symbol. That handles @1+@ and @...@
-- without special cases.
module Iota.Reader
  ( -- * Reading
    readDatums
  , readDatumsLocated
  , readDatum
  , readDatumFromText
  , ReadFailure (..)

    -- * Number syntax
  , numberFromText

    -- * Character classes
  , isDelimiter
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict qualified as St
import Data.Char (chr, digitToInt, isHexDigit, isSpace, toLower)
import Data.Functor (void)
import Data.List (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word8)
import Iota.Number (makePolar, makeRectangular)
import Iota.Types
import Text.Megaparsec
import Text.Megaparsec.Char

-- The Bool is the fold-case flag.
type Parser = ParsecT Void Text (St.State Bool)

runP :: Parser a -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
runP p name input = St.evalState (runParserT p name input) False

-- Entry points

-- | Every datum in the input. The path is only used in error messages.
readDatums :: FilePath -> Text -> Either Text [Datum]
readDatums name input =
  case runP (skipSpace *> many (lexeme datum) <* eof) name input of
    Left bundle -> Left (T.pack (errorBundlePretty bundle))
    Right ds -> Right ds

-- | The same, with the start position of each top level form.
readDatumsLocated :: FilePath -> Text -> Either Text [Located]
readDatumsLocated name input =
  case runP (skipSpace *> many (lexeme located) <* eof) name input of
    Left bundle -> Left (T.pack (errorBundlePretty bundle))
    Right ds -> Right ds
  where
    located = do
      pos <- getSourcePos
      d <- datum
      pure Located {locatedLoc = SrcLoc name (unPos (sourceLine pos)) (unPos (sourceColumn pos)), locatedDatum = d}

-- | Exactly one datum, nothing after it.
readDatum :: FilePath -> Text -> Either Text Datum
readDatum name input =
  case runP (skipSpace *> lexeme datum <* eof) name input of
    Left bundle -> Left (T.pack (errorBundlePretty bundle))
    Right d -> Right d

-- | 'Incomplete' means the input stopped in the middle of a datum and the
-- caller should fetch more.
data ReadFailure
  = Incomplete
  | Malformed Text
  deriving stock (Eq, Show)

-- | One datum off the front of the text, and what is left. This is what
-- the read procedure is built on. @Right Nothing@ means only whitespace
-- and comments remained, so the caller returns an eof object.
readDatumFromText :: Text -> Either ReadFailure (Maybe (Datum, Text))
readDatumFromText input =
  case St.runState (runParserT' (skipSpace *> oneOrEnd) (startState input)) False of
    ((st, Right md), _) -> Right (fmap (\d -> (d, stateInput st)) md)
    ((_, Left bundle), _)
      | errorAtEnd bundle -> Left Incomplete
      | otherwise -> Left (Malformed (T.pack (errorBundlePretty bundle)))
  where
    -- Either the input is exhausted or a datum starts here. A stray ")"
    -- is an error, not an eof. Trailing whitespace is left alone on
    -- purpose. read stops right after the datum, so a newline that
    -- follows it is still there for read-char.
    oneOrEnd = (Nothing <$ eof) <|> (Just <$> datum)
    -- An error at the very end of the text means we ran out of input,
    -- not that the input was wrong.
    errorAtEnd bundle = errorOffset (NE.head (bundleErrors bundle)) >= T.length input
    startState s =
      State
        { stateInput = s
        , stateOffset = 0
        , statePosState =
            PosState
              { pstateInput = s
              , pstateOffset = 0
              , pstateSourcePos = initialPos "<read>"
              , pstateTabWidth = defaultTabWidth
              , pstateLinePrefix = ""
              }
        , stateParseErrors = []
        }

-- | string->number. 'Nothing' unless the whole string is a number.
numberFromText :: Int -> Text -> Maybe Number
numberFromText radix t = case runP (numberP radix <* eof) "" t of
  Right n -> Just n
  Left _ -> Nothing

-- Whitespace and comments

-- | The report's delimiters plus square brackets and the quote
-- characters. Counting the quote characters as delimiters is not
-- strictly standard, but it makes @(a .,b)@ read the way people expect,
-- and nobody puts them in identifiers.
isDelimiter :: Char -> Bool
isDelimiter c = isSpace c || c `elem` ("()[]\";|'`," :: String)

-- | Whitespace, the three kinds of comment, and the @#!@ directives.
skipSpace :: Parser ()
skipSpace = skipMany (choice [void space1, lineComment, blockComment, datumComment, directive])

lineComment :: Parser ()
lineComment = char ';' *> void (takeWhileP Nothing (/= '\n'))

blockComment :: Parser ()
blockComment = void (try (string "#|")) *> go
  where
    go =
      choice
        [ void (try (string "|#"))
        , void (try (string "#|")) *> go *> go
        , anySingle *> go
        ]

-- | @#;@ discards the next datum, however big.
datumComment :: Parser ()
datumComment = void (try (string "#;")) *> skipSpace *> void datum

-- | @#!fold-case@ and @#!no-fold-case@. Each stays in effect until the
-- next one.
directive :: Parser ()
directive = do
  _ <- try (string "#!")
  name <- takeWhile1P (Just "directive") (not . isDelimiter)
  case name of
    "fold-case" -> lift (St.put True)
    "no-fold-case" -> lift (St.put False)
    _ -> fail ("unknown directive: #!" <> T.unpack name)

foldCaseOn :: Parser Bool
foldCaseOn = lift St.get

lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace

-- Data

datum :: Parser Datum
datum =
  choice
    [ stringLit
    , -- No try around charLit. Once we have seen #\ a bad name is an
      -- error, not an odd looking symbol.
      charLit
    , try boolLit
    , try vectorLit
    , try bytevectorLit
    , try labelled
    , listLit
    , abbreviation
    , pipedSymbol
    , atom
    ]
    <?> "datum"

-- | A number if it parses as one, otherwise a symbol. Anything starting
-- with @#@ that gets this far is syntax the reader does not know.
atom :: Parser Datum
atom = do
  tok <- takeWhile1P (Just "symbol or number") (not . isDelimiter)
  fold <- foldCaseOn
  case numberFromText 10 tok of
    Just n -> pure (DNumber n)
    Nothing
      | T.head tok == '#' -> fail ("unknown # syntax: " <> T.unpack tok)
      | otherwise -> pure (DSymbol (Symbol (if fold then T.toCaseFold tok else tok)))

boolLit :: Parser Datum
boolLit =
  choice
    [ DBool True <$ try (string' "#true")
    , DBool False <$ try (string' "#false")
    , DBool True <$ try (string' "#t")
    , DBool False <$ try (string' "#f")
    ]
    <* notFollowedBy (satisfy (not . isDelimiter))

charLit :: Parser Datum
charLit = do
  _ <- try (string "#\\")
  name <- optional (takeWhile1P Nothing (not . isDelimiter))
  fold <- foldCaseOn
  case name of
    Nothing -> DChar <$> anySingle
    Just t
      | T.length t == 1 -> pure (DChar (T.head t))
      | Just hexits <- T.stripPrefix "x" t
      , not (T.null hexits)
      , T.all isHexDigit hexits ->
          DChar <$> codepoint (readRadix 16 (T.unpack hexits))
      | otherwise -> case lookup (if fold then T.toLower t else t) characterNames of
          Just c -> pure (DChar c)
          Nothing -> fail ("unknown character name: " <> T.unpack t)

characterNames :: [(Text, Char)]
characterNames =
  [ ("alarm", '\a')
  , ("backspace", '\b')
  , ("delete", '\DEL')
  , ("escape", '\ESC')
  , ("newline", '\n')
  , ("null", '\0')
  , ("return", '\r')
  , ("space", ' ')
  , ("tab", '\t')
  ]

stringLit :: Parser Datum
stringLit = do
  _ <- char '"'
  parts <- many stringPart
  _ <- char '"'
  pure (DString (T.concat parts))

stringPart :: Parser Text
stringPart =
  choice
    [ char '\\' *> escape
    , takeWhile1P Nothing (\c -> c /= '"' && c /= '\\')
    ]

escape :: Parser Text
escape =
  choice
    [ "\a" <$ char 'a'
    , "\b" <$ char 'b'
    , "\t" <$ char 't'
    , "\n" <$ char 'n'
    , "\r" <$ char 'r'
    , "\"" <$ char '"'
    , "\\" <$ char '\\'
    , "|" <$ char '|'
    , hexEscape
    , -- Backslash before a newline drops the newline and the indentation
      -- around it.
      lineContinuation
    ]

hexEscape :: Parser Text
hexEscape = do
  _ <- char 'x'
  hexits <- some hexDigitChar
  _ <- char ';'
  T.singleton <$> codepoint (readRadix 16 hexits)

-- | Range check for hex escapes. Surrogates are not scalar values, and
-- 'chr' would happily produce one.
codepoint :: Integer -> Parser Char
codepoint n
  | n > 0x10FFFF = fail ("code point out of range: " <> show n)
  | n >= 0xD800 && n <= 0xDFFF = fail ("surrogate code point: " <> show n)
  | otherwise = pure (chr (fromInteger n))

lineContinuation :: Parser Text
lineContinuation =
  "" <$ (skipMany intraline *> eol *> skipMany intraline)
  where
    intraline = oneOf (" \t" :: String)

-- | @|like this|@. Fold-case does not apply inside the bars.
pipedSymbol :: Parser Datum
pipedSymbol = do
  _ <- char '|'
  parts <- many (choice [char '\\' *> escape, takeWhile1P Nothing (\c -> c /= '|' && c /= '\\')])
  _ <- char '|'
  pure (DSymbol (Symbol (T.concat parts)))

-- | The quote abbreviations become @(quote x)@ and so on right here.
-- Nothing downstream knows the short forms exist.
abbreviation :: Parser Datum
abbreviation =
  choice
    [ wrap "quote" <$> (char '\'' *> skipSpace *> datum)
    , wrap "quasiquote" <$> (char '`' *> skipSpace *> datum)
    , wrap "unquote-splicing" <$> (try (string ",@") *> skipSpace *> datum)
    , wrap "unquote" <$> (char ',' *> skipSpace *> datum)
    ]
  where
    wrap s d = DList [DSymbol s, d] Nothing

listLit :: Parser Datum
listLit = do
  open <- oneOf ("([" :: String)
  let close = if open == '(' then ')' else ']'
  skipSpace
  -- Without the notFollowedBy the dot would be read as a one character
  -- symbol.
  items <- many (notFollowedBy dotMarker *> lexeme datum)
  tl <- optional (dotMarker *> skipSpace *> lexeme datum)
  _ <- char close
  -- ( . x) is not a datum.
  if null items && maybe False (const True) tl
    then fail "expected an element before the dot"
    else pure (DList items tl)

-- | A dot followed by a delimiter. @.5@ and @...@ do not count.
dotMarker :: Parser ()
dotMarker = void (try (char '.' <* lookAhead (satisfy isDelimiter)))

vectorLit :: Parser Datum
vectorLit = do
  _ <- try (string "#(")
  skipSpace
  items <- many (notFollowedBy dotMarker *> lexeme datum)
  notFollowedBy dotMarker <?> "no dotted tail in a vector"
  _ <- char ')'
  pure (DVector items)

bytevectorLit :: Parser Datum
bytevectorLit = do
  _ <- try (string' "#u8(")
  skipSpace
  bytes <- many (lexeme byte)
  _ <- char ')'
  pure (DBytevector bytes)
  where
    byte = do
      n <- uinteger 10
      if n < 0 || n > 255
        then fail ("byte out of range: " <> show n)
        else pure (fromInteger n :: Word8)

-- | Datum labels, @#n=@ and @#n#@.
labelled :: Parser Datum
labelled = do
  _ <- char '#'
  n <- some digitChar
  let lbl = fromInteger (readRadix 10 n) :: Int
  choice
    [ DLabelRef lbl <$ char '#'
    , DLabel lbl <$> (char '=' *> skipSpace *> datum)
    ]

-- Number syntax

-- | A real before the exactness prefix is applied. Decimals stay exact
-- rationals until then, so @#e1.5@ is @3\/2@ and @#e1e400@ never goes
-- through a double.
data RealSyntax
  = RealExact Rational
  | RealDecimal Rational
  | -- | @-0.0@ is zero as a rational but the sign has to survive.
    RealNegativeZero
  | RealSpecial Double

-- | Radix and exactness prefixes in either order, then a complex
-- number, which is usually one real.
numberP :: Int -> Parser Number
numberP defaultRadix = do
  prefixes <- many (try radixOrExactness)
  let radices = [r | Left r <- prefixes]
      exactnesses = [e | Right e <- prefixes]
  when (length radices > 1 || length exactnesses > 1) $
    fail "duplicate number prefix"
  let radix = case radices of
        [r] -> r
        _ -> defaultRadix
  complexP radix >>= \case
    CReal r -> realNumber exactnesses r
    CRect re im -> makeRectangular <$> realNumber exactnesses re <*> realNumber exactnesses im
    CPolar mag ang -> makePolar <$> realNumber exactnesses mag <*> realNumber exactnesses ang

-- | Apply the exactness prefix, if any, to one real.
realNumber :: [Bool] -> RealSyntax -> Parser Number
realNumber exactnesses real =
  case (exactnesses, real) of
    ([], RealNegativeZero) -> pure (Inexact (-0.0))
    ([False], RealNegativeZero) -> pure (Inexact (-0.0))
    ([True], RealNegativeZero) -> pure (Exact 0)
    ([], RealExact r) -> pure (Exact r)
    ([], RealDecimal r) -> pure (Inexact (fromRational r))
    ([], RealSpecial d) -> pure (Inexact d)
    ([False], RealSpecial d) -> pure (Inexact d)
    ([False], RealExact r) -> pure (Inexact (fromRational r))
    ([False], RealDecimal r) -> pure (Inexact (fromRational r))
    ([True], RealExact r) -> pure (Exact r)
    ([True], RealDecimal r) -> pure (Exact r)
    ([True], RealSpecial _) -> fail "#e cannot be applied to an infinity or NaN"
    _ -> fail "malformed number"

-- | The shapes of the complex grammar. A plain real, real plus
-- imaginary, or magnitude at angle.
data ComplexSyntax
  = CReal RealSyntax
  | CRect RealSyntax RealSyntax
  | CPolar RealSyntax RealSyntax

-- | Polar is tried first because a real followed by @ is unambiguous.
-- Then a real followed by a signed imaginary part, then an imaginary
-- part on its own, then a plain real.
complexP :: Int -> Parser ComplexSyntax
complexP radix =
  choice
    [ try (CPolar <$> signedReal radix <* char '@' <*> signedReal radix)
    , try (CRect <$> signedReal radix <*> imaginary radix)
    , try (CRect (RealExact 0) <$> imaginary radix)
    , CReal <$> signedReal radix
    ]

-- | A signed imaginary part ending in i. The sign is required, and the
-- magnitude may be missing, so +i and -i work.
imaginary :: Int -> Parser RealSyntax
imaginary radix = do
  sign <- oneOf ("+-" :: String)
  let negative = sign == '-'
  mag <-
    choice
      [ RealSpecial (if negative then -1 / 0 else 1 / 0) <$ try (string' "inf.0")
      , RealSpecial (0 / 0) <$ try (string' "nan.0")
      , try (ureal radix)
      , pure (RealExact 1)
      ]
  _ <- char' 'i'
  pure (applySign negative mag)

radixOrExactness :: Parser (Either Int Bool)
radixOrExactness = do
  _ <- char '#'
  c <- oneOf ("bodxeiBODXEI" :: String)
  pure $ case toLower c of
    'b' -> Left 2
    'o' -> Left 8
    'd' -> Left 10
    'x' -> Left 16
    'e' -> Right True
    _ -> Right False

signedReal :: Int -> Parser RealSyntax
signedReal radix = do
  sign <- optional (oneOf ("+-" :: String))
  let negative = sign == Just '-'
  case sign of
    -- inf.0 and nan.0 always carry a sign, so they are only tried once
    -- one has been read.
    Just _ ->
      choice
        [ RealSpecial (if negative then -1 / 0 else 1 / 0) <$ try (string' "inf.0")
        , RealSpecial (0 / 0) <$ try (string' "nan.0")
        , applySign negative <$> ureal radix
        ]
    Nothing -> ureal radix

applySign :: Bool -> RealSyntax -> RealSyntax
applySign True (RealExact r) = RealExact (negate r)
applySign True (RealDecimal 0) = RealNegativeZero
applySign True (RealDecimal r) = RealDecimal (negate r)
applySign True RealNegativeZero = RealNegativeZero
applySign True (RealSpecial d) = RealSpecial (negate d)
applySign False r = r

ureal :: Int -> Parser RealSyntax
ureal radix
  | radix == 10 = try ratio <|> decimal
  | otherwise = try ratio <|> (RealExact . fromInteger <$> uinteger radix)
  where
    ratio = do
      n <- uinteger radix
      _ <- char '/'
      d <- uinteger radix
      if d == 0 then fail "zero denominator" else pure (RealExact (n % d))

-- | Decimal notation. A point or an exponent makes the number inexact.
-- The value is built as a rational and only converted at the end. That
-- gets the rounding right and lets @#e@ keep it exact. The exponent cap
-- is there because @1e999999999@ would otherwise try to build a huge
-- integer.
decimal :: Parser RealSyntax
decimal = do
  ipart <- many digitChar
  fpart <- optional (char '.' *> many digitChar)
  ex <- optional exponentP
  if null ipart && maybe True null fpart
    then fail "not a number"
    else case (fpart, ex) of
      (Nothing, Nothing) -> pure (RealExact (fromInteger (readRadix 10 ipart)))
      _ -> do
        let frac = maybe "" id fpart
            mantissa = readRadix 10 (ipart <> frac)
            e = maybe 0 id ex - fromIntegral (length frac)
        when (abs e > 100_000) $ fail "exponent out of range"
        pure . RealDecimal $
          if e >= 0
            then fromInteger (mantissa * 10 ^ e)
            else mantissa % (10 ^ negate e)

exponentP :: Parser Integer
exponentP = do
  _ <- oneOf ("eE" :: String)
  sign <- optional (oneOf ("+-" :: String))
  ds <- some digitChar
  let v = readRadix 10 ds
  pure (if sign == Just '-' then negate v else v)

uinteger :: Int -> Parser Integer
uinteger radix = readRadix radix <$> some (digitOf radix)
  where
    digitOf r = satisfy (\c -> isHexDigit c && digitToInt c < r) <?> ("digit in radix " <> show r)

readRadix :: Int -> String -> Integer
readRadix radix = foldl' step 0
  where
    step acc c = acc * fromIntegral radix + fromIntegral (digitToInt c)
