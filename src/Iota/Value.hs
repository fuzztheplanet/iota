-- | Constructors and accessors for the objects declared in
-- "Iota.Types". Nearly everything runs in IO because the objects are
-- mutable. Even 'cons' allocates two IORefs and an id.
--
-- An accessor that is handed the wrong kind of object throws a
-- 'TypeError' naming what it wanted. The @want@ functions at the end
-- are that check on its own, for the primitives to unpack arguments
-- with.
module Iota.Value
  ( -- * Booleans
    isTruthy
  , boolean

    -- * Numbers
  , number
  , integer

    -- * Strings
  , makeString
  , makeLiteralString
  , stringText
  , stringChars
  , stringLength
  , stringRefAt
  , stringSetAt
  , stringFill
  , stringCopyInto

    -- * Pairs and lists
  , cons
  , car
  , cdr
  , setCar
  , setCdr
  , list
  , listWithTail
  , properList
  , requireList
  , improperList
  , listLength
  , isList

    -- * Vectors
  , makeVector
  , vectorFromList
  , vectorToList

    -- * Bytevectors
  , bytevectorFromList
  , bytevectorToList

    -- * Procedures
  , primitive
  , primitiveK
  , procedureName

    -- * Multiple values
  , valuesToList
  , listToValues

    -- * Equivalence
  , eqv
  , eqp
  , equal

    -- * Datum conversion
  , datumToValue
  , literalToValue
  , valueToDatum
  , identBase

    -- * Mutability
  , requireMutable

    -- * Type errors
  , wantSymbol
  , wantNumber
  , wantReal
  , wantInteger
  , wantIndex
  , wantChar
  , wantString
  , wantPair
  , wantVector
  , wantProcedure
  , wantPort
  , wantList
  ) where

import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as UMV
import Data.Word (Word8)
import Iota.Counter (freshId)
import Iota.Number qualified as N
import Iota.Types

-- Booleans

-- | Only #f is false.
isTruthy :: Value -> Bool
isTruthy (Boolean False) = False
isTruthy _ = True

-- | A Scheme boolean.
boolean :: Bool -> Value
boolean = Boolean

-- Numbers

-- | A Scheme number. Numbers are immutable and have no identity.
number :: Number -> Value
number = Num

-- | An exact integer.
integer :: Integer -> Value
integer = Num . N.fromInteger'

-- Strings

-- | A fresh mutable string with the given contents. Every call
-- allocates, so two strings made from the same text are @equal?@ but
-- not @eqv?@.
makeString :: Text -> Eval Value
makeString t = do
  i <- freshId
  Str i <$> charsFromText t

-- | The same, marked constant so string-set! refuses it.
makeLiteralString :: Text -> Eval Value
makeLiteralString t = do
  i <- freshId
  Str (literalId i) <$> charsFromText t

charsFromText :: Text -> Eval (UMV.IOVector Char)
charsFromText t = do
  let n = T.length t
  v <- UMV.new n
  mapM_ (uncurry (UMV.write v)) (zip [0 ..] (T.unpack t))
  pure v

-- | The character vector inside a string. It is the string's own
-- storage, not a copy, so a write through it skips the 'requireMutable'
-- check. A 'TypeError' for anything that is not a string.
stringChars :: Value -> Eval (UMV.IOVector Char)
stringChars (Str _ v) = pure v
stringChars v = throwScheme (TypeError "a string" v)

-- | Copy the characters out to Text. O(n), but the read-only string
-- primitives walk the whole string anyway.
stringText :: Value -> Eval Text
stringText v = do
  cs <- stringChars v
  T.pack <$> mapM (UMV.read cs) [0 .. UMV.length cs - 1]

-- | @string-length@, in characters. O(1).
stringLength :: Value -> Eval Int
stringLength v = UMV.length <$> stringChars v

-- | @string-ref@. The first argument names the calling procedure for
-- the 'IndexOutOfRange' error. Only the upper bound is checked. The
-- callers get the index from 'wantIndex', which rules out a negative
-- one.
stringRefAt :: Text -> Value -> Int -> Eval Char
stringRefAt who v k = do
  cs <- stringChars v
  if k < UMV.length cs then UMV.read cs k else throwScheme (IndexOutOfRange who k)

-- | @string-set!@. A literal constant is refused before the index is
-- looked at. Otherwise as 'stringRefAt'.
stringSetAt :: Text -> Value -> Int -> Char -> Eval ()
stringSetAt who v k c = do
  requireMutable who v
  cs <- stringChars v
  if k < UMV.length cs then UMV.write cs k c else throwScheme (IndexOutOfRange who k)

-- | @string-fill!@ over the half-open range from start to end. The
-- range is not checked here. The primitive validates it against the
-- length first. A literal constant is refused.
stringFill :: Value -> Char -> Int -> Int -> Eval ()
stringFill v c start end = do
  requireMutable "string-fill!" v
  cs <- stringChars v
  mapM_ (\k -> UMV.write cs k c) [start .. end - 1]

-- | @string-copy!@: destination, index in the destination, source,
-- then the start and end of the source range. The source chunk is read
-- first, so overlapping ranges are fine. Throws 'IndexOutOfRange' when
-- the chunk does not fit in the destination. The source range itself is
-- validated by the primitive, not here. A literal destination is
-- refused.
stringCopyInto :: Value -> Int -> Value -> Int -> Int -> Eval ()
stringCopyInto to at from start end = do
  requireMutable "string-copy!" to
  dst <- stringChars to
  src <- stringChars from
  let n = end - start
  if at + n > UMV.length dst
    then throwScheme (IndexOutOfRange "string-copy!" at)
    else do
      chunk <- mapM (UMV.read src) [start .. end - 1]
      mapM_ (\(k, ch) -> UMV.write dst (at + k) ch) (zip [0 ..] chunk)

-- Pairs and lists

-- | A fresh mutable pair, with a new identity.
cons :: Value -> Value -> Eval Value
cons a b = do
  i <- freshId
  Pair i <$> newIORef a <*> newIORef b

-- | The current contents of the car field. A 'TypeError' for anything
-- that is not a pair, the empty list included.
car :: Value -> Eval Value
car (Pair _ a _) = readIORef a
car v = throwScheme (TypeError "a pair" v)

-- | The current contents of the cdr field. Fails like 'car'.
cdr :: Value -> Eval Value
cdr (Pair _ _ d) = readIORef d
cdr v = throwScheme (TypeError "a pair" v)

-- | @set-car!@. A pair that came from a literal is refused through
-- 'requireMutable'.
setCar :: Value -> Value -> Eval ()
setCar p@(Pair _ a _) v = requireMutable "set-car!" p >> writeIORef a v
setCar v _ = throwScheme (TypeError "a pair" v)

-- | @set-cdr!@. Fails like 'setCar'.
setCdr :: Value -> Value -> Eval ()
setCdr p@(Pair _ _ d) v = requireMutable "set-cdr!" p >> writeIORef d v
setCdr v _ = throwScheme (TypeError "a pair" v)

-- | Altering a constant is an error in R7RS. This is where that is
-- enforced. The first argument names the mutator for the message. A
-- pair, string, vector or bytevector with a literal id (see
-- 'isMutableId') throws a 'TypeError'. Every other value passes,
-- whatever its type, so the caller still has to check that.
requireMutable :: Text -> Value -> Eval ()
requireMutable who v = case v of
  Pair i _ _ | not (isMutableId i) -> constant
  Vec i _ | not (isMutableId i) -> constant
  Bytes i _ | not (isMutableId i) -> constant
  Str i _ | not (isMutableId i) -> constant
  _ -> pure ()
  where
    constant = throwScheme (TypeError ("a mutable object in " <> who <> ", not a literal constant") v)

-- | A fresh proper list. Every pair in it is newly allocated and
-- mutable.
list :: [Value] -> Eval Value
list xs = listWithTail xs Null

-- | @(x1 x2 . tl)@
listWithTail :: [Value] -> Value -> Eval Value
listWithTail xs tl = foldr (\x acc -> acc >>= cons x) (pure tl) xs

-- | A proper list as a Haskell list. Nothing if the list is improper
-- or circular. Tortoise and hare for the cycle check.
properList :: Value -> Eval (Maybe [Value])
properList v0 = go [] v0 v0
  where
    go acc slow fast = case slow of
      Null -> pure (Just (reverse acc))
      Pair _ a d -> do
        x <- readIORef a
        rest <- readIORef d
        fast' <- step2 fast
        case fast' of
          Just (Pair fid _ _) | Pair sid _ _ <- rest, fid == sid -> pure Nothing
          _ -> go (x : acc) rest (maybe Null id fast')
      _ -> pure Nothing
    -- Two steps for the hare. If it falls off the end the list is
    -- finite and the cycle check above cannot fire.
    step2 f = do
      f1 <- step f
      case f1 of
        Just p -> step p
        Nothing -> pure Nothing
    step (Pair _ _ d) = Just <$> readIORef d
    step _ = pure Nothing

-- | 'properList', but with a 'TypeError' instead of Nothing. The first
-- argument names the calling procedure for the message.
requireList :: Text -> Value -> Eval [Value]
requireList who v =
  properList v >>= \case
    Just xs -> pure xs
    Nothing -> throwScheme (TypeError ("a proper list in " <> who) v)

-- | Elements and tail. @(a b . c)@ gives @([a, b], c)@ and a proper
-- list gives a Null tail. Circular input is an error, not a hang.
improperList :: Value -> Eval ([Value], Value)
improperList v0 = go [] v0 v0
  where
    go acc slow fast = case slow of
      Pair _ a d -> do
        x <- readIORef a
        rest <- readIORef d
        fast' <- step2 fast
        case fast' of
          Just (Pair fid _ _) | Pair sid _ _ <- rest, fid == sid -> throwScheme (TypeError "a finite list" v0)
          _ -> go (x : acc) rest (maybe Null id fast')
      other -> pure (reverse acc, other)
    step2 f = do
      f1 <- step f
      case f1 of
        Just p -> step p
        Nothing -> pure Nothing
    step (Pair _ _ d) = Just <$> readIORef d
    step _ = pure Nothing

-- | The length of a proper list. Nothing for an improper or circular
-- one, so @length@ can report an error instead of looping.
listLength :: Value -> Eval (Maybe Int)
listLength v = fmap length <$> properList v

-- | @list?@. False for an improper list and for a circular one.
isList :: Value -> Eval Bool
isList v = maybe False (const True) <$> properList v

-- Vectors

-- | A fresh vector of the given length with every slot holding the
-- fill value. A negative length throws 'IndexOutOfRange'.
makeVector :: Int -> Value -> Eval Value
makeVector n fill
  | n < 0 = throwScheme (IndexOutOfRange "make-vector" n)
  | otherwise = do
      i <- freshId
      Vec i <$> MV.replicate n fill

-- | A fresh mutable vector holding the given elements in order.
vectorFromList :: [Value] -> Eval Value
vectorFromList xs = do
  v <- MV.new (length xs)
  mapM_ (uncurry (MV.write v)) (zip [0 ..] xs)
  i <- freshId
  pure (Vec i v)

-- | The elements of a vector as they are now. A 'TypeError' for
-- anything that is not a vector.
vectorToList :: Value -> Eval [Value]
vectorToList (Vec _ v) = mapM (MV.read v) [0 .. MV.length v - 1]
vectorToList v = throwScheme (TypeError "a vector" v)

-- Bytevectors

-- | A fresh mutable bytevector holding the given bytes in order.
bytevectorFromList :: [Word8] -> Eval Value
bytevectorFromList ws = do
  v <- UMV.new (length ws)
  mapM_ (uncurry (UMV.write v)) (zip [0 ..] ws)
  i <- freshId
  pure (Bytes i v)

-- | The bytes of a bytevector as they are now. A 'TypeError' for
-- anything that is not a bytevector.
bytevectorToList :: Value -> Eval [Word8]
bytevectorToList (Bytes _ v) = mapM (UMV.read v) [0 .. UMV.length v - 1]
bytevectorToList v = throwScheme (TypeError "a bytevector" v)

-- Procedures

-- | Wrap a Haskell function as a Scheme procedure. The name is what
-- arity errors report and what @write@ shows inside @#\<procedure ...>@.
-- 'Iota.Eval.applyK' checks the argument count against the arity before
-- the function runs, so the function only ever sees an argument list
-- of a permitted length. It may throw a 'SchemeError', which @applyK@
-- turns into a Scheme condition.
primitive :: Text -> Arity -> ([Value] -> Eval Value) -> Value
primitive name arity fn = Proc (Primitive name arity fn)

-- | The same for a function that takes the continuation of the call,
-- which it must use to return. For procedures that call back into
-- Scheme or change the dynamic state. See 'PrimitiveK' for what such a
-- function may and may not do.
primitiveK :: Text -> Arity -> ([Value] -> K -> Eval Value) -> Value
primitiveK name arity fn = Proc (PrimitiveK name arity fn)

-- | The name a procedure carries. A primitive has the name it was
-- registered under and a closure the name it was defined with, or
-- @lambda@ if it never got one. Continuations and parameter objects
-- have the fixed names @continuation@ and @parameter@.
procedureName :: Procedure -> Text
procedureName = \case
  Primitive {..} -> primName
  PrimitiveK {..} -> primName
  Closure {..} -> closureName
  Continuation {} -> "continuation"
  Parameter {} -> "parameter"

-- Multiple values

-- | The values delivered to a continuation, as a list.
valuesToList :: Value -> [Value]
valuesToList (MultipleValues vs) = vs
valuesToList v = [v]

-- | The inverse of 'valuesToList'. A single value stays bare.
listToValues :: [Value] -> Value
listToValues [v] = v
listToValues vs = MultipleValues vs

-- Equivalence

-- | @eqv?@. Numbers and characters by value, everything else by
-- identity. Exactness counts, and @0.0@ and @-0.0@ are different. Two
-- primitives are the same when they have the same name.
eqv :: Value -> Value -> Eval Bool
eqv a b = pure (eqvPure a b)

eqvPure :: Value -> Value -> Bool
eqvPure a b = case (a, b) of
  (Null, Null) -> True
  (Boolean x, Boolean y) -> x == y
  (Sym x, Sym y) -> x == y
  (Character x, Character y) -> x == y
  -- Exactness counts, so (eqv? 1 1.0) is #f. So is (eqv? 0.0 -0.0),
  -- because (/ 1 x) can tell them apart and eqv? has to as well.
  (Num (Exact x), Num (Exact y)) -> x == y
  (Num (Inexact x), Num (Inexact y)) ->
    (x == y && isNegativeZero x == isNegativeZero y) || (isNaN x && isNaN y)
  (Num (Complex xr xi), Num (Complex yr yi)) -> eqvPure (Num xr) (Num yr) && eqvPure (Num xi) (Num yi)
  (Str i _, Str j _) -> i == j
  (Pair i _ _, Pair j _ _) -> i == j
  (Vec i _, Vec j _) -> i == j
  (Bytes i _, Bytes j _) -> i == j
  (PortVal x, PortVal y) -> portId x == portId y
  (PromiseVal x, PromiseVal y) -> x == y
  (RecordVal x, RecordVal y) -> recordId x == recordId y
  (RecordTypeVal x, RecordTypeVal y) -> rtdId x == rtdId y
  (EnvVal x, EnvVal y) -> globalId x == globalId y
  (Proc x, Proc y) -> sameProcedure x y
  (EofObject, EofObject) -> True
  (Unspecified, Unspecified) -> True
  _ -> False

sameProcedure :: Procedure -> Procedure -> Bool
sameProcedure x y = case (x, y) of
  (Closure {closureId = i}, Closure {closureId = j}) -> i == j
  (Continuation {contId = i}, Continuation {contId = j}) -> i == j
  (Parameter {paramId = i}, Parameter {paramId = j}) -> i == j
  (Primitive {primName = n1}, Primitive {primName = n2}) -> n1 == n2
  (PrimitiveK {primName = n1}, PrimitiveK {primName = n2}) -> n1 == n2
  _ -> False

-- | @eq?@ is allowed to be the same as @eqv?@, and here it is.
eqp :: Value -> Value -> Eval Bool
eqp = eqv

-- | @equal?@, and it terminates on cyclic data.
--
-- The cheap path is a plain recursive walk with a step budget. When
-- the budget runs out, on huge data or on a cycle, it starts over with
-- the union-find algorithm from Adams and Dybvig. That version merges
-- pairs and vectors into equivalence classes as it compares them and
-- treats two objects already in one class as equal. Going round a cycle
-- therefore stops.
equal :: Value -> Value -> Eval Bool
equal a0 b0 = do
  (_, r) <- quick (100_000 :: Int) a0 b0
  case r of
    Just ok -> pure ok
    Nothing -> slow a0 b0
  where
    -- Nothing means the budget ran out.
    quick :: Int -> Value -> Value -> Eval (Int, Maybe Bool)
    quick budget a b
      | budget <= 0 = pure (0, Nothing)
      | eqvPure a b = pure (budget, Just True)
      | otherwise = case (a, b) of
          (Str {}, Str {}) -> do
            tx <- stringText a
            ty <- stringText b
            pure (budget, Just (tx == ty))
          (Pair _ xa xd, Pair _ ya yd) -> do
            xa' <- readIORef xa
            ya' <- readIORef ya
            (b1, headEq) <- quick (budget - 1) xa' ya'
            case headEq of
              Just True -> do
                xd' <- readIORef xd
                yd' <- readIORef yd
                quick b1 xd' yd'
              other -> pure (b1, other)
          (Vec _ x, Vec _ y)
            | MV.length x /= MV.length y -> pure (budget, Just False)
            | otherwise -> goIndexed budget (MV.length x) (MV.read x) (MV.read y)
          (Bytes {}, Bytes {}) -> do
            xs <- bytevectorToList a
            ys <- bytevectorToList b
            pure (budget, Just (xs == ys))
          (ErrObj k1 m1 i1, ErrObj k2 m2 i2)
            | k1 == k2 && m1 == m2 && length i1 == length i2 ->
                goList budget i1 i2
            | otherwise -> pure (budget, Just False)
          _ -> pure (budget, Just False)

    goIndexed budget n readX readY = loop budget 0
      where
        loop bud i
          | i >= n = pure (bud, Just True)
          | otherwise = do
              x <- readX i
              y <- readY i
              (bud', ok) <- quick (bud - 1) x y
              case ok of
                Just True -> loop bud' (i + 1)
                other -> pure (bud', other)

    goList budget [] [] = pure (budget, Just True)
    goList budget (x : xs) (y : ys) = do
      (b1, ok) <- quick (budget - 1) x y
      case ok of
        Just True -> goList b1 xs ys
        other -> pure (b1, other)
    goList budget _ _ = pure (budget, Just False)

    -- The exact version. Union-find keyed on object ids.
    slow a b = do
      table <- newIORef (IntMap.empty :: IntMap (IORef UF))
      let node i = do
            t <- readIORef table
            case IntMap.lookup i t of
              Just n -> pure n
              Nothing -> do
                n <- newIORef (Root 1)
                modifyIORef' table (IntMap.insert i n)
                pure n
          find n =
            readIORef n >>= \case
              Root _ -> pure n
              Link parent -> do
                root <- find parent
                writeIORef n (Link root) -- path compression
                pure root
          -- True when the two were already in one class.
          unionFind i j = do
            ri <- node i >>= find
            rj <- node j >>= find
            if ri == rj
              then pure True
              else do
                Root si <- readIORef ri
                Root sj <- readIORef rj
                if si < sj
                  then writeIORef ri (Link rj) >> writeIORef rj (Root (si + sj))
                  else writeIORef rj (Link ri) >> writeIORef ri (Root (si + sj))
                pure False
          e x y
            | eqvPure x y = pure True
            | otherwise = case (x, y) of
                (Str {}, Str {}) -> (==) <$> stringText x <*> stringText y
                (Pair i xa xd, Pair j ya yd) -> do
                  seen <- unionFind i j
                  if seen
                    then pure True
                    else do
                      ok <- join2 (readIORef xa) (readIORef ya)
                      if ok then join2 (readIORef xd) (readIORef yd) else pure False
                (Vec i p, Vec j q)
                  | MV.length p /= MV.length q -> pure False
                  | otherwise -> do
                      seen <- unionFind i j
                      if seen
                        then pure True
                        else allM [join2 (MV.read p k) (MV.read q k) | k <- [0 .. MV.length p - 1]]
                (Bytes {}, Bytes {}) -> (==) <$> bytevectorToList x <*> bytevectorToList y
                (ErrObj k1 m1 i1, ErrObj k2 m2 i2)
                  | k1 == k2 && m1 == m2 && length i1 == length i2 ->
                      allM (zipWith e i1 i2)
                _ -> pure False
          join2 mx my = do
            x <- mx
            y <- my
            e x y
      e a b

    allM [] = pure True
    allM (m : ms) = m >>= \ok -> if ok then allM ms else pure False

data UF = Root !Int | Link (IORef UF)

-- Datum conversion

-- | A fresh mutable value from a datum. It allocates every time, so two
-- reads of the same text are equal? but not eqv?. Labels become shared
-- and possibly cyclic structure. Aliases become their plain symbol.
datumToValue :: Datum -> Eval Value
datumToValue = datumToValueWith False

-- | The same for a literal in program text. Every pair, string, vector
-- and bytevector it builds is marked constant, so the mutators refuse
-- it. See 'requireMutable'.
literalToValue :: Datum -> Eval Value
literalToValue = datumToValueWith True

datumToValueWith :: Bool -> Datum -> Eval Value
datumToValueWith literal d0 = do
  labels <- newIORef (IntMap.empty :: IntMap Value)
  -- A forward #n# reference gets a placeholder pair. A second pass
  -- patches them once every label has a value.
  placeholders <- newIORef (IntMap.empty :: IntMap Int) -- placeholder id -> label
  let go = \case
        DBool b -> pure (Boolean b)
        DNumber n -> pure (Num n)
        DChar c -> pure (Character c)
        DString t -> if literal then makeLiteralString t else makeString t
        DSymbol s -> pure (Sym s)
        DAlias a -> pure (Sym (identBase (DAlias a)))
        DList xs tl -> do
          tl' <- maybe (pure Null) go tl
          xs' <- mapM go xs
          foldr (\x acc -> acc >>= consWith x) (pure tl') xs'
        DVector xs -> mapM go xs >>= vectorFromList >>= mark
        DBytevector ws -> bytevectorFromList ws >>= mark
        DLabel n d -> do
          v <- go d
          modifyIORef' labels (IntMap.insert n v)
          pure v
        DLabelRef n -> do
          known <- readIORef labels
          case IntMap.lookup n known of
            Just v -> pure v
            Nothing -> do
              p <- cons Null Null
              case p of
                Pair i _ _ -> modifyIORef' placeholders (IntMap.insert i n)
                _ -> pure ()
              pure p
  v <- go d0
  ph <- readIORef placeholders
  patchPlaceholders labels ph v
  where
    mark v
      | not literal = pure v
      | otherwise = pure $ case v of
          Vec i mv -> Vec (literalId i) mv
          Bytes i mv -> Bytes (literalId i) mv
          other -> other
    consWith x acc = do
      p <- cons x acc
      case p of
        Pair i a d | literal -> pure (Pair (literalId i) a d)
        other -> pure other

-- | The second pass. Swaps each placeholder for the value its label names.
patchPlaceholders :: IORef (IntMap Value) -> IntMap Int -> Value -> Eval Value
patchPlaceholders labels ph v =
  if IntMap.null ph
    then pure v
    else do
      known <- readIORef labels
      let target n = case IntMap.lookup n known of
            Just t -> pure t
            Nothing -> throwScheme (ParseFailure ("undefined datum label #" <> tshow n <> "#"))
          resolve x = case x of
            Pair i _ _ | Just n <- IntMap.lookup i ph -> target n
            _ -> pure x
          -- One walk over the tree just built. A slot holding a
          -- placeholder is patched and not descended into, since its
          -- target is elsewhere in the same tree. That is why this
          -- terminates even though the patching creates cycles.
          patchSlot readSlot writeSlot = do
            el <- readSlot
            case el of
              Pair i _ _ | IntMap.member i ph -> resolve el >>= writeSlot
              _ -> patch el
          patch x = case x of
            Pair _ a d -> do
              patchSlot (readIORef a) (writeIORef a)
              patchSlot (readIORef d) (writeIORef d)
            Vec _ mv -> mapM_ (\k -> patchSlot (MV.read mv k) (MV.write mv k)) [0 .. MV.length mv - 1]
            _ -> pure ()
      patch v
      resolve v
  where
    tshow :: Int -> Text
    tshow = T.pack . show


-- | The symbol underneath any renaming. A datum that is not an
-- identifier gives the symbol @?@, so check with 'Iota.Syntax.isIdent'
-- first where that matters.
identBase :: Datum -> Symbol
identBase = \case
  DSymbol s -> s
  DAlias a -> identBase (aliasIdent a)
  _ -> Symbol "?"

-- | The other direction, for @eval@. Procedures, ports and the other
-- objects with no source form are rejected with a 'TypeError'. So is a
-- list that is circular through its cdrs. A cycle through a car or
-- through a vector is not detected, and the conversion recurses until
-- the stack runs out. Shared structure is not preserved. Each
-- occurrence is copied.
valueToDatum :: Value -> Eval Datum
valueToDatum = \case
  Null -> pure (DList [] Nothing)
  Boolean b -> pure (DBool b)
  Num n -> pure (DNumber n)
  Character c -> pure (DChar c)
  Sym s -> pure (DSymbol s)
  v@(Str {}) -> DString <$> stringText v
  v@(Pair {}) -> do
    (xs, tl) <- improperList v
    xs' <- mapM valueToDatum xs
    case tl of
      Null -> pure (DList xs' Nothing)
      _ -> DList xs' . Just <$> valueToDatum tl
  v@(Vec {}) -> DVector <$> (vectorToList v >>= mapM valueToDatum)
  v@(Bytes {}) -> DBytevector <$> bytevectorToList v
  v -> throwScheme (TypeError "a datum" v)

-- Argument checking helpers

-- | The symbol, or a 'TypeError'.
wantSymbol :: Value -> Eval Symbol
wantSymbol (Sym s) = pure s
wantSymbol v = throwScheme (TypeError "a symbol" v)

-- | Any number, complex included, or a 'TypeError'.
wantNumber :: Value -> Eval Number
wantNumber (Num n) = pure n
wantNumber v = throwScheme (TypeError "a number" v)

-- | A number that is not complex.
wantReal :: Value -> Eval Number
wantReal v = do
  n <- wantNumber v
  if N.isReal n then pure n else throwScheme (TypeError "a real number" v)

-- | An integer, exact or inexact, so @3.0@ is accepted and comes back
-- as 3. Use 'wantIndex' where exactness matters.
wantInteger :: Value -> Eval Integer
wantInteger v = do
  n <- wantNumber v
  case N.toIntegerMaybe n of
    Just i -> pure i
    Nothing -> throwScheme (TypeError "an integer" v)

-- | An index. Exact, non-negative, and it fits in an Int.
wantIndex :: Value -> Eval Int
wantIndex v = do
  n <- wantNumber v
  case n of
    Exact r | Just i <- N.toIntegerMaybe (Exact r), i >= 0, i <= fromIntegral (maxBound :: Int) -> pure (fromInteger i)
    _ -> throwScheme (TypeError "an exact non-negative integer" v)

-- | The character, or a 'TypeError'.
wantChar :: Value -> Eval Char
wantChar (Character c) = pure c
wantChar v = throwScheme (TypeError "a character" v)

-- | The contents of a string, copied out. This is 'stringText', so it
-- costs O(n).
wantString :: Value -> Eval Text
wantString = stringText

-- | The car and cdr cells of a pair. Writing through them skips the
-- 'requireMutable' check.
wantPair :: Value -> Eval (IORef Value, IORef Value)
wantPair (Pair _ a d) = pure (a, d)
wantPair v = throwScheme (TypeError "a pair" v)

-- | The storage of a vector, not a copy. Writing through it skips the
-- 'requireMutable' check.
wantVector :: Value -> Eval (MV.IOVector Value)
wantVector (Vec _ v) = pure v
wantVector v = throwScheme (TypeError "a vector" v)

-- | Anything that can be applied: a primitive, a closure, a
-- continuation or a parameter object.
wantProcedure :: Value -> Eval Procedure
wantProcedure (Proc p) = pure p
wantProcedure v = throwScheme (TypeError "a procedure" v)

-- | The port, open or closed, or a 'TypeError'.
wantPort :: Value -> Eval Port
wantPort (PortVal p) = pure p
wantPort v = throwScheme (TypeError "a port" v)

-- | The elements of a proper list. 'requireList' with a generic name
-- in the message.
wantList :: Value -> Eval [Value]
wantList = requireList "procedure"
