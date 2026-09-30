-- | Characters and strings. A string is a mutable unboxed vector of
-- Char, so indexing is O(1). The read-only operations copy into a Text
-- first through 'stringText'. That copy is linear, and so were the
-- operations, so it has not been worth optimising.
module Iota.Primitives.Strings
  ( stringPrimitives
  ) where

import Data.Char (GeneralCategory (..), chr, generalCategory, isAlpha, isLower, isUpper, ord, toLower, toUpper)
import Data.Text qualified as T
import Iota.Types
import Iota.Value

stringPrimitives :: [(Symbol, Value)]
stringPrimitives =
  [ -- characters
    prim1 "char->integer" $ \v -> integer . fromIntegral . ord <$> wantChar v
  , prim1 "integer->char" $ \v -> do
      i <- wantIndex v
      if i > 0x10FFFF || (i >= 0xD800 && i <= 0xDFFF)
        then throwScheme (IndexOutOfRange "integer->char" i)
        else pure (Character (chr i))
  , charCmp "char=?" (==)
  , charCmp "char<?" (<)
  , charCmp "char>?" (>)
  , charCmp "char<=?" (<=)
  , charCmp "char>=?" (>=)
  , charCmpBy "char-ci=?" foldChar (==)
  , charCmpBy "char-ci<?" foldChar (<)
  , charCmpBy "char-ci>?" foldChar (>)
  , charCmpBy "char-ci<=?" foldChar (<=)
  , charCmpBy "char-ci>=?" foldChar (>=)
  , charPred "char-alphabetic?" isAlpha
  , charPred "char-numeric?" isNumericChar
  , charPred "char-whitespace?" isWhitespaceChar
  , charPred "char-upper-case?" isUpper
  , charPred "char-lower-case?" isLower
  , charMap "char-upcase" toUpper
  , charMap "char-downcase" toLower
  , charMap "char-foldcase" foldChar
  , -- decimal digits come in runs of ten in Unicode, so the value is the
    -- distance from the start of the run
    prim1 "digit-value" $ \v -> do
      c <- wantChar v
      pure $
        if isNumericChar c
          then integer (fromIntegral ((ord c - runStart c) `mod` 10))
          else Boolean False
  , -- strings
    prim "make-string" (Between 1 2) $ \args -> do
      (n, fill) <- case args of
        [nv] -> (,) <$> wantIndex nv <*> pure ' '
        [nv, cv] -> (,) <$> wantIndex nv <*> wantChar cv
        _ -> impossible
      makeString (T.replicate n (T.singleton fill))
  , prim "string" (AtLeast 0) $ \args ->
      mapM wantChar args >>= makeString . T.pack
  , prim1 "string-length" $ \v -> integer . fromIntegral <$> stringLength v
  , prim2 "string-ref" $ \sv kv -> do
      k <- wantIndex kv
      Character <$> stringRefAt "string-ref" sv k
  , prim3 "string-set!" $ \sv kv cv -> do
      k <- wantIndex kv
      c <- wantChar cv
      stringSetAt "string-set!" sv k c
      pure Unspecified
  , strCmp "string=?" (==)
  , strCmp "string<?" (<)
  , strCmp "string>?" (>)
  , strCmp "string<=?" (<=)
  , strCmp "string>=?" (>=)
  , strCmpBy "string-ci=?" T.toCaseFold (==)
  , strCmpBy "string-ci<?" T.toCaseFold (<)
  , strCmpBy "string-ci>?" T.toCaseFold (>)
  , strCmpBy "string-ci<=?" T.toCaseFold (<=)
  , strCmpBy "string-ci>=?" T.toCaseFold (>=)
  , prim1 "string-foldcase" $ \v -> wantString v >>= makeString . T.toCaseFold
  , prim3 "substring" $ \sv startv endv -> do
      t <- wantString sv
      start <- wantIndex startv
      end <- wantIndex endv
      checkRange "substring" (T.length t) start end
      makeString (T.take (end - start) (T.drop start t))
  , prim "string-append" (AtLeast 0) $ \args ->
      mapM wantString args >>= makeString . T.concat
  , prim "string-copy" (Between 1 3) $ \args -> do
      (t, start, end) <- sliceArgs "string-copy" args
      makeString (T.take (end - start) (T.drop start t))
  , prim "string-copy!" (Between 3 5) $ \case
      (to : atv : from : rest) -> do
        at <- wantIndex atv
        n <- stringLength from
        (start, end) <- case rest of
          [] -> pure (0, n)
          [startv] -> (\s -> (s, n)) <$> wantIndex startv
          [startv, endv] -> (,) <$> wantIndex startv <*> wantIndex endv
          _ -> impossible
        checkRange "string-copy!" n start end
        stringCopyInto to at from start end
        pure Unspecified
      _ -> impossible
  , prim "string-fill!" (Between 2 4) $ \case
      (sv : cv : rest) -> do
        n <- stringLength sv
        c <- wantChar cv
        (start, end) <- case rest of
          [] -> pure (0, n)
          [startv] -> (\s -> (s, n)) <$> wantIndex startv
          [startv, endv] -> (,) <$> wantIndex startv <*> wantIndex endv
          _ -> impossible
        checkRange "string-fill!" n start end
        stringFill sv c start end
        pure Unspecified
      _ -> impossible
  , prim "string->list" (Between 1 3) $ \args -> do
      (t, start, end) <- sliceArgs "string->list" args
      list (map Character (T.unpack (T.take (end - start) (T.drop start t))))
  , prim1 "list->string" $ \v -> do
      xs <- requireList "list->string" v
      mapM wantChar xs >>= makeString . T.pack
  , prim1 "string-upcase" $ \v -> wantString v >>= makeString . T.toUpper
  , prim1 "string-downcase" $ \v -> wantString v >>= makeString . T.toLower
  ]
  where
    prim name arity fn = (name, primitive (symbolName name) arity fn)
    prim1 name fn = prim name (Exactly 1) $ \case
      [a] -> fn a
      _ -> impossible
    prim2 name fn = prim name (Exactly 2) $ \case
      [a, b] -> fn a b
      _ -> impossible
    prim3 name fn = prim name (Exactly 3) $ \case
      [a, b, c] -> fn a b c
      _ -> impossible

    impossible :: Eval a
    impossible = throwScheme (UserError "internal arity error" [])

    -- Single character case folding. Going through upper case first
    -- handles final sigma and long s, whose lowercase is not their fold.
    foldChar c = toLower (toUpper c)

    isNumericChar c = generalCategory c == DecimalNumber

    -- Unicode White_Space, which Data.Char doesn't expose directly.
    isWhitespaceChar c =
      c `elem` ("\t\n\v\f\r\x85" :: String)
        || generalCategory c `elem` [Space, LineSeparator, ParagraphSeparator]

    runStart c = go (ord c)
      where
        go n
          | n > 0 && generalCategory (chr (n - 1)) == DecimalNumber = go (n - 1)
          | otherwise = n

    charCmp name rel = charCmpBy name id rel
    charCmpBy name f rel =
      prim name (AtLeast 2) $ \args -> do
        cs <- map f <$> mapM wantChar args
        pure (Boolean (and (zipWith rel cs (drop 1 cs))))

    charPred name p = prim1 name $ \v -> Boolean . p <$> wantChar v

    charMap name f = prim1 name $ \v -> Character . f <$> wantChar v

    strCmp name rel = strCmpBy name id rel
    strCmpBy name f rel =
      prim name (AtLeast 2) $ \args -> do
        ts <- map f <$> mapM wantString args
        pure (Boolean (and (zipWith rel ts (drop 1 ts))))

    checkRange who len start end =
      if start < 0 || end > len || start > end
        then throwScheme (IndexOutOfRange who (if start > len then start else end))
        else pure ()

    sliceArgs who args = case args of
      [sv] -> do
        t <- wantString sv
        pure (t, 0, T.length t)
      [sv, startv] -> do
        t <- wantString sv
        start <- wantIndex startv
        checkRange who (T.length t) start (T.length t)
        pure (t, start, T.length t)
      [sv, startv, endv] -> do
        t <- wantString sv
        start <- wantIndex startv
        end <- wantIndex endv
        checkRange who (T.length t) start end
        pure (t, start, end)
      _ -> impossible
