-- | The numeric procedures. All the arithmetic is in "Iota.Number".
-- This file unpacks arguments and packs results.
module Iota.Primitives.Numeric
  ( numericPrimitives
  ) where

import Iota.Number qualified as N
import Iota.Reader (numberFromText)
import Iota.Types
import Iota.Value

numericPrimitives :: [(Symbol, Value)]
numericPrimitives =
  [ -- arithmetic
    prim "+" (AtLeast 0) $ \args -> Num . foldl N.add (Exact 0) <$> mapM wantNumber args
  , prim "*" (AtLeast 0) $ \args -> Num . foldl N.mul (Exact 1) <$> mapM wantNumber args
  , prim "-" (AtLeast 1) $
      \args ->
        mapM wantNumber args >>= \case
          [x] -> pure (Num (N.negate' x))
          (x : xs) -> pure (Num (foldl N.sub x xs))
          [] -> throwScheme (ArityMismatch "-" (AtLeast 1) 0)
  , prim "/" (AtLeast 1) $
      \args ->
        mapM wantNumber args >>= \case
          [x] -> Num <$> N.divide (Exact 1) x
          (x : xs) -> Num <$> foldM' N.divide x xs
          [] -> throwScheme (ArityMismatch "/" (AtLeast 1) 0)
  , prim "abs" (Exactly 1) $ unaryReal (pure . N.abs')
  , prim "quotient" (Exactly 2) $ binaryNum N.quotient
  , prim "remainder" (Exactly 2) $ binaryNum N.remainder
  , prim "modulo" (Exactly 2) $ binaryNum N.modulo
  , prim "truncate-quotient" (Exactly 2) $ binaryNum N.quotient
  , prim "truncate-remainder" (Exactly 2) $ binaryNum N.remainder
  , prim "floor-remainder" (Exactly 2) $ binaryNum N.modulo
  , prim "gcd" (AtLeast 0) $ \args -> do
      ns <- mapM wantNumber args
      Num . N.abs' <$> foldM' N.gcd' (Exact 0) ns
  , prim "lcm" (AtLeast 0) $ \args -> do
      ns <- mapM wantNumber args
      Num . N.abs' <$> foldM' N.lcm' (Exact 1) ns
  , prim "numerator" (Exactly 1) $ unaryReal N.numerator'
  , prim "denominator" (Exactly 1) $ unaryReal N.denominator'
  , prim "floor" (Exactly 1) $ unaryReal (pure . N.floor')
  , prim "ceiling" (Exactly 1) $ unaryReal (pure . N.ceiling')
  , prim "truncate" (Exactly 1) $ unaryReal (pure . N.truncate')
  , prim "round" (Exactly 1) $ unaryReal (pure . N.round')
  , prim "expt" (Exactly 2) $ binaryNum N.expt
  , prim "rationalize" (Exactly 2) $ binaryReal N.rationalize'
  , prim "sqrt" (Exactly 1) $ unaryNum (pure . N.sqrt')
  , prim "exact-integer-sqrt" (Exactly 1) $ \case
      [v] -> do
        i <- wantInteger v
        if i < 0
          then throwScheme (TypeError "a non-negative integer" v)
          else do
            let (s, r) = N.exactIntegerSqrt i
            pure (MultipleValues [integer s, integer r])
      _ -> impossibleArity
  , prim "exp" (Exactly 1) $ unaryNum (pure . N.transcendental exp exp)
  , prim "log" (Between 1 2) $ \args -> do
      ns <- mapM wantNumber args
      case ns of
        [x] -> pure (Num (N.transcendental log log x))
        [x, b]
          | N.isReal x && N.isReal b && not (N.isNegative x) && not (N.isNegative b) ->
              pure (Num (N.fromDouble (logBase (N.toDouble b) (N.toDouble x))))
          | otherwise -> Num <$> N.divide (N.transcendental log log x) (N.transcendental log log b)
        _ -> impossibleArity
  , prim "sin" (Exactly 1) $ unaryNum (pure . N.transcendental sin sin)
  , prim "cos" (Exactly 1) $ unaryNum (pure . N.transcendental cos cos)
  , prim "tan" (Exactly 1) $ unaryNum (pure . N.transcendental tan tan)
  , prim "asin" (Exactly 1) $ unaryNum (pure . N.transcendental asin asin)
  , prim "acos" (Exactly 1) $ unaryNum (pure . N.transcendental acos acos)
  , prim "atan" (Between 1 2) $ \args -> case args of
      [x] -> Num . N.transcendental atan atan <$> wantNumber x
      [y, x] -> do
        -- two arguments only make sense on the real plane
        y' <- wantReal y
        x' <- wantReal x
        pure (Num (N.atan2' y' x'))
      _ -> impossibleArity
  , prim "square" (Exactly 1) $ unaryNum (\n -> pure (N.mul n n))
  , -- comparisons chain, (< 1 2 3) checks each adjacent pair
    prim "=" (AtLeast 2) $ chain N.eqNum
  , prim "<" (AtLeast 2) $ chainReal N.ltNum
  , prim "<=" (AtLeast 2) $ chainReal N.leNum
  , prim ">" (AtLeast 2) $ chainReal N.gtNum
  , prim ">=" (AtLeast 2) $ chainReal N.geNum
  , prim "max" (AtLeast 1) $ extremum N.gtNum
  , prim "min" (AtLeast 1) $ extremum N.ltNum
  , -- predicates
    numPred "number?" (const True)
  , numPred "complex?" (const True)
  , numPred "real?" N.isReal
  , numPred "rational?" N.isRational
  , numPred "integer?" N.isInteger
  , numPred "exact?" N.isExact
  , numPred "inexact?" N.isInexact
  , numPred "exact-integer?" (\n -> N.isExact n && N.isInteger n)
  , numPred "exact-rational?" (\n -> N.isExact n && N.isRational n)
  , numPred "zero?" N.isZero
  , realPred "positive?" N.isPositive
  , realPred "negative?" N.isNegative
  , numPred "finite?" N.isFinite
  , numPred "infinite?" (\n -> not (N.isFinite n) && not (N.isNaN' n))
  , numPred "nan?" N.isNaN'
  , numPred "even?" (\n -> maybe False even (N.toIntegerMaybe n))
  , numPred "odd?" (\n -> maybe False odd (N.toIntegerMaybe n))
  , -- exactness
    prim "exact" (Exactly 1) $ unaryNum N.toExact
  , prim "inexact" (Exactly 1) $ unaryNum (pure . N.toInexact)
  , prim "exact->inexact" (Exactly 1) $ unaryNum (pure . N.toInexact)
  , prim "inexact->exact" (Exactly 1) $ unaryNum N.toExact
  , -- to and from strings
    prim "number->string" (Between 1 2) $ \args -> case args of
      [v] -> do
        n <- wantNumber v
        N.numberToText 10 n >>= makeString
      [v, r] -> do
        n <- wantNumber v
        radix <- wantIndex r
        N.numberToText radix n >>= makeString
      _ -> impossibleArity
  , prim "string->number" (Between 1 2) $ \args -> do
      (txt, radix) <- case args of
        [v] -> (,) <$> wantString v <*> pure 10
        [v, r] -> (,) <$> wantString v <*> wantIndex r
        _ -> impossibleArity
      if radix `notElem` [2, 8, 10, 16]
        then throwScheme (TypeError "a radix of 2, 8, 10 or 16" (integer (fromIntegral radix)))
        else pure (maybe (Boolean False) Num (numberFromText radix txt))
  , -- (scheme complex)
    prim "real-part" (Exactly 1) $ unaryNum (pure . N.realPart)
  , prim "imag-part" (Exactly 1) $ unaryNum (pure . N.imagPart)
  , prim "magnitude" (Exactly 1) $ unaryNum (pure . N.magnitude)
  , prim "angle" (Exactly 1) $ unaryNum (pure . N.angle)
  , prim "make-rectangular" (Exactly 2) $ binaryReal (\re im -> pure (N.makeRectangular re im))
  , prim "make-polar" (Exactly 2) $ binaryReal (\mag ang -> pure (N.makePolar mag ang))
  ]
  where
    prim :: Symbol -> Arity -> ([Value] -> Eval Value) -> (Symbol, Value)
    prim name arity fn = (name, primitive (symbolName name) arity fn)

    unaryNum :: (Number -> Eval Number) -> [Value] -> Eval Value
    unaryNum f = \case
      [v] -> wantNumber v >>= fmap Num . f
      _ -> impossibleArity

    binaryNum :: (Number -> Number -> Eval Number) -> [Value] -> Eval Value
    binaryNum f = \case
      [a, b] -> do
        x <- wantNumber a
        y <- wantNumber b
        Num <$> f x y
      _ -> impossibleArity

    -- the operations that have no meaning on the complex plane
    unaryReal :: (Number -> Eval Number) -> [Value] -> Eval Value
    unaryReal f = \case
      [v] -> wantReal v >>= fmap Num . f
      _ -> impossibleArity

    binaryReal :: (Number -> Number -> Eval Number) -> [Value] -> Eval Value
    binaryReal f = \case
      [a, b] -> do
        x <- wantReal a
        y <- wantReal b
        Num <$> f x y
      _ -> impossibleArity

    chain :: (Number -> Number -> Bool) -> [Value] -> Eval Value
    chain rel args = do
      ns <- mapM wantNumber args
      pure (Boolean (and (zipWith rel ns (drop 1 ns))))

    chainReal :: (Number -> Number -> Bool) -> [Value] -> Eval Value
    chainReal rel args = do
      ns <- mapM wantReal args
      pure (Boolean (and (zipWith rel ns (drop 1 ns))))

    extremum :: (Number -> Number -> Bool) -> [Value] -> Eval Value
    extremum better args = do
      ns <- mapM wantReal args
      case ns of
        [] -> impossibleArity
        (x : xs) ->
          -- one inexact argument makes the result inexact even when the
          -- winner was exact
          let winner = foldl (\a b -> if better b a then b else a) x xs
           in pure . Num $
                if any N.isInexact ns then N.toInexact winner else winner

    numPred :: Symbol -> (Number -> Bool) -> (Symbol, Value)
    numPred name p =
      prim name (Exactly 1) $ \case
        [Num n] -> pure (Boolean (p n))
        [_] -> pure (Boolean False)
        _ -> impossibleArity

    -- like numPred, but a complex argument is an error rather than #f
    realPred :: Symbol -> (Number -> Bool) -> (Symbol, Value)
    realPred name p =
      prim name (Exactly 1) $ \case
        [v@(Num n)] -> if N.isReal n then pure (Boolean (p n)) else throwScheme (TypeError "a real number" v)
        [_] -> pure (Boolean False)
        _ -> impossibleArity

    foldM' :: (b -> a -> Eval b) -> b -> [a] -> Eval b
    foldM' _ z [] = pure z
    foldM' f z (x : xs) = f z x >>= \z' -> foldM' f z' xs

    -- applyK checks the count first, so these branches never run. The
    -- throw keeps the pattern match total.
    impossibleArity :: Eval a
    impossibleArity = throwScheme (UserError "internal arity error" [])
