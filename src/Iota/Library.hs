-- | The library registry, import set resolution and the export lists of
-- the standard libraries.
--
-- Everything is defined in one implementation environment. The
-- @(scheme ...)@ libraries are named subsets of it, transcribed from
-- Appendix A of the report. A user define-library gets an environment
-- of its own and lands in the same registry.
module Iota.Library
  ( -- * Names
    libNameFromDatum
  , libNameText

    -- * Registry
  , registerLibrary
  , lookupLibrary

    -- * Import sets
  , resolveImportSet

    -- * Standard libraries
  , standardLibraryExports
  , iotaExtrasExports
  ) where

import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as T
import Iota.Types
import Iota.Value (identBase)

-- Names

-- | A library name is a list of identifiers and non-negative integers,
-- like @(srfi 1)@.
libNameFromDatum :: Datum -> Either Text LibName
libNameFromDatum = \case
  DList parts Nothing | not (null parts) -> LibName <$> mapM part parts
  d -> Left ("malformed library name: " <> T.pack (show d))
  where
    part d
      | DSymbol s <- d = Right (symbolName s)
      | DAlias _ <- d = Right (symbolName (identBase d))
      | DNumber (Exact r) <- d, denominator r == 1, numerator r >= 0 = Right (T.pack (show (numerator r)))
      | otherwise = Left ("malformed library name part: " <> T.pack (show d))

libNameText :: LibName -> Text
libNameText (LibName parts) = "(" <> T.unwords parts <> ")"

-- Registry

registerLibrary :: Runtime -> Library -> IO ()
registerLibrary rt lib = modifyIORef' (rtLibraries rt) (Map.insert (libName lib) lib)

lookupLibrary :: Runtime -> LibName -> IO (Maybe Library)
lookupLibrary rt name = Map.lookup name <$> readIORef (rtLibraries rt)

-- Import sets

-- | The bindings an import set brings in. The @load@ argument finds a
-- library by name, reading it from disk if it has to. It is passed in
-- because the loader lives above this module.
resolveImportSet ::
  (LibName -> IO (Either Text Library)) ->
  Datum ->
  IO (Either Text (Map Symbol Denotation))
resolveImportSet load spec = case spec of
  DList (kw : rest) Nothing
    | isKeyword kw "only", (inner : ids) <- rest -> do
        r <- resolveImportSet load inner
        pure $ do
          m <- r
          names <- mapM identOf ids
          mapM_ (present m) names
          Right (Map.restrictKeys m (Set.fromList names))
    | isKeyword kw "except", (inner : ids) <- rest -> do
        r <- resolveImportSet load inner
        pure $ do
          m <- r
          names <- mapM identOf ids
          mapM_ (present m) names
          Right (foldr Map.delete m names)
    | isKeyword kw "prefix", [inner, p] <- rest -> do
        r <- resolveImportSet load inner
        pure $ do
          m <- r
          prefix <- identOf p
          Right (Map.mapKeys (\s -> Symbol (symbolName prefix <> symbolName s)) m)
    | isKeyword kw "rename", (inner : pairs) <- rest -> do
        r <- resolveImportSet load inner
        pure $ do
          m <- r
          renames <- mapM renamePair pairs
          mapM_ (present m . fst) renames
          let dropped = foldr (Map.delete . fst) m renames
              added = Map.fromList [(to, d) | (from, to) <- renames, Just d <- [Map.lookup from m]]
          Right (Map.union added dropped)
  _ -> case libNameFromDatum spec of
    Left err -> pure (Left err)
    Right name -> fmap libExports <$> load name
  where
    isKeyword d name = case d of
      DSymbol s -> s == name
      DAlias _ -> identBase d == name
      _ -> False
    identOf d = case d of
      DSymbol s -> Right s
      DAlias _ -> Right (identBase d)
      _ -> Left ("expected an identifier in import set, got " <> T.pack (show d))
    renamePair = \case
      DList [a, b] Nothing -> (,) <$> identOf a <*> identOf b
      d -> Left ("malformed rename in import set: " <> T.pack (show d))
    present m s =
      if Map.member s m
        then Right ()
        else Left ("import set refers to a name the library does not export: " <> symbolName s)

-- Standard libraries

-- | Appendix A of the report. Startup checks that every name listed here
-- is defined, so a typo fails at boot.
standardLibraryExports :: [(LibName, [Text])]
standardLibraryExports =
  [ (LibName ["scheme", "base"], T.words schemeBase)
  , (LibName ["scheme", "case-lambda"], ["case-lambda"])
  , (LibName ["scheme", "char"], T.words schemeChar)
  , (LibName ["scheme", "complex"], ["angle", "imag-part", "magnitude", "make-polar", "make-rectangular", "real-part"])
  , (LibName ["scheme", "cxr"], T.words schemeCxr)
  , (LibName ["scheme", "eval"], ["environment", "eval"])
  , (LibName ["scheme", "file"], T.words schemeFile)
  , (LibName ["scheme", "inexact"], T.words "acos asin atan cos exp finite? infinite? log nan? sin sqrt tan")
  , (LibName ["scheme", "lazy"], T.words "delay delay-force force make-promise promise?")
  , (LibName ["scheme", "load"], ["load"])
  , (LibName ["scheme", "process-context"], T.words "command-line emergency-exit exit get-environment-variable get-environment-variables")
  , (LibName ["scheme", "read"], ["read"])
  , (LibName ["scheme", "repl"], ["interaction-environment"])
  , (LibName ["scheme", "time"], T.words "current-jiffy current-second jiffies-per-second")
  , (LibName ["scheme", "write"], T.words "display write write-shared write-simple")
  , (LibName ["scheme", "r5rs"], T.words schemeR5rs)
  ]

schemeBase :: Text
schemeBase =
  T.unwords
    [ "* + - ... / < <= = => > >= _ abs and append apply assoc assq assv begin"
    , "binary-port? boolean=? boolean? bytevector bytevector-append bytevector-copy"
    , "bytevector-copy! bytevector-length bytevector-u8-ref bytevector-u8-set!"
    , "bytevector? caar cadr call-with-current-continuation call-with-port"
    , "call-with-values call/cc car case cdar cddr cdr ceiling char->integer"
    , "char-ready? char<=? char<? char=? char>=? char>? char? close-input-port"
    , "close-output-port close-port complex? cond cond-expand cons"
    , "current-error-port current-input-port current-output-port define"
    , "define-record-type define-syntax define-values denominator do dynamic-wind"
    , "else eof-object eof-object? eq? equal? eqv? error error-object-irritants"
    , "error-object-message error-object? even? exact exact-integer-sqrt"
    , "exact-integer? exact? expt features file-error? floor floor-quotient"
    , "floor-remainder floor/ flush-output-port for-each gcd get-output-bytevector"
    , "get-output-string guard if include include-ci inexact inexact?"
    , "input-port-open? input-port? integer->char integer? lambda lcm length let"
    , "let* let*-values let-syntax let-values letrec letrec* letrec-syntax list"
    , "list->string list->vector list-copy list-ref list-set! list-tail list?"
    , "make-bytevector make-list make-parameter make-string make-vector map max"
    , "member memq memv min modulo negative? newline not null? number->string"
    , "number? numerator odd? open-input-bytevector open-input-string"
    , "open-output-bytevector open-output-string or output-port-open? output-port?"
    , "pair? parameterize peek-char peek-u8 positive? procedure? quasiquote quote"
    , "quotient raise raise-continuable rational? rationalize read-bytevector"
    , "read-bytevector! read-char read-error? read-line read-string read-u8 real?"
    , "remainder reverse round set! set-car! set-cdr! square string string->list"
    , "string->number string->symbol string->utf8 string->vector string-append"
    , "string-copy string-copy! string-fill! string-for-each string-length"
    , "string-map string-ref string-set! string<=? string<? string=? string>=?"
    , "string>? string? substring symbol->string symbol=? symbol? syntax-error"
    , "syntax-rules textual-port? truncate truncate-quotient truncate-remainder"
    , "truncate/ u8-ready? unless unquote unquote-splicing utf8->string values"
    , "vector vector->list vector->string vector-append vector-copy vector-copy!"
    , "vector-fill! vector-for-each vector-length vector-map vector-ref vector-set!"
    , "vector? when with-exception-handler write-bytevector write-char"
    , "write-string write-u8 zero?"
    ]

schemeChar :: Text
schemeChar =
  T.unwords
    [ "char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?"
    , "char-downcase char-foldcase char-lower-case? char-numeric? char-upcase"
    , "char-upper-case? char-whitespace? digit-value string-ci<=? string-ci<?"
    , "string-ci=? string-ci>=? string-ci>? string-downcase string-foldcase"
    , "string-upcase"
    ]

schemeCxr :: Text
schemeCxr =
  T.unwords
    [ "caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar"
    , "caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar"
    , "cddadr cdddar cddddr"
    ]

schemeFile :: Text
schemeFile =
  T.unwords
    [ "call-with-input-file call-with-output-file delete-file file-exists?"
    , "open-binary-input-file open-binary-output-file open-input-file"
    , "open-output-file with-input-from-file with-output-to-file"
    ]

schemeR5rs :: Text
schemeR5rs =
  T.unwords
    [ "* + - / < <= = > >= abs acos and angle append apply asin assoc assq assv"
    , "atan begin boolean? caaaar caaadr caaar caadar caaddr caadr caar cadaar"
    , "cadadr cadar caddar cadddr caddr cadr call-with-current-continuation"
    , "call-with-input-file call-with-output-file call-with-values car case cdaaar"
    , "cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr"
    , "cdddr cddr cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<?"
    , "char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric?"
    , "char-ready? char-upcase char-upper-case? char-whitespace? char<=? char<?"
    , "char=? char>=? char>? char? close-input-port close-output-port complex?"
    , "cond cons cos current-input-port current-output-port define define-syntax"
    , "delay denominator display do dynamic-wind else eof-object? eq? equal? eqv?"
    , "eval even? exact->inexact exact? exp expt floor for-each force gcd if"
    , "imag-part inexact->exact inexact? input-port? integer->char integer?"
    , "interaction-environment lambda lcm length let let* let-syntax letrec"
    , "letrec-syntax list list->string list->vector list-ref list-tail list? load"
    , "log magnitude make-polar make-rectangular make-string make-vector map max"
    , "member memq memv min modulo negative? newline not null-environment null?"
    , "number->string number? numerator odd? open-input-file open-output-file or"
    , "output-port? pair? peek-char positive? procedure? quasiquote quote quotient"
    , "rational? rationalize read read-char real-part real? remainder reverse"
    , "round scheme-report-environment set! set-car! set-cdr! sin sqrt string"
    , "string->list string->number string->symbol string-append string-ci<=?"
    , "string-ci<? string-ci=? string-ci>=? string-ci>? string-copy string-fill!"
    , "string-length string-ref string-set! string<=? string<? string=? string>=?"
    , "string>? string? substring symbol->string symbol? tan truncate values"
    , "vector vector->list vector-fill! vector-length vector-ref vector-set!"
    , "vector? with-input-from-file with-output-to-file write write-char zero?"
    , "=> ... _ unquote unquote-splicing"
    ]

-- | The non-standard extras, exported as @(iota extras)@. The REPL
-- imports them. Portable programs should not.
iotaExtrasExports :: [Text]
iotaExtrasExports =
  T.words
    "id flip curry compose foldl foldr fold reduce unfold last-pair filter remove \
    \partition iota-range list-index any every append-map add1 sub1 1+ 1- \
    \string-null? string-join call-with-output-string port->string displayln \
    \writeln print fact fib first second third named-lambda"
