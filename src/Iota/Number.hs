-- | Arithmetic on 'Number'. A few rules hold everywhere in this file. An
-- inexact argument makes the result inexact. Exact arithmetic runs over
-- Rational and never rounds. A complex number whose imaginary part is an
-- exact zero is a real number, and the constructors below keep it that
-- way, so the rest of the file can pattern match on Exact and Inexact
-- without worrying about a complex number in disguise.
module Iota.Number
  ( -- * Construction
    fromInt
  , fromInteger'
  , fromDouble
  , makeRectangular
  , makePolar

    -- * Predicates
  , isExact
  , isInexact
  , isInteger
  , isRational
  , isReal
  , isZero
  , isPositive
  , isNegative
  , isFinite
  , isNaN'

    -- * Conversion
  , toDouble
  , toExact
  , toInexact
  , toIntegerMaybe

    -- * Arithmetic
  , add
  , sub
  , mul
  , divide
  , negate'
  , abs'
  , quotient
  , remainder
  , modulo
  , gcd'
  , lcm'
  , expt
  , sqrt'
  , exactIntegerSqrt
  , floor'
  , ceiling'
  , truncate'
  , round'
  , numerator'
  , denominator'
  , transcendental
  , atan2'

    -- * Complex parts
  , realPart
  , imagPart
  , magnitude
  , angle

    -- * Comparison
  , rationalize'
  , eqNum
  , ltNum
  , leNum
  , gtNum
  , geNum
  , compareNum

    -- * External representation
  , numberToText
  , numberToText10
  ) where

import Data.Char (intToDigit)
import Data.Complex (Complex ((:+)))
import Data.Complex qualified as C
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import Data.Text qualified as T
import Iota.Types
import Numeric (floatToDigits, showHex, showInt, showIntAtBase, showOct)

-- Construction and basic queries

fromInt :: Int -> Number
fromInt = Exact . fromIntegral

fromInteger' :: Integer -> Number
fromInteger' = Exact . fromInteger

fromDouble :: Double -> Number
fromDouble = Inexact

-- | The only way a complex number gets built. An exact zero imaginary
-- part gives back the real part on its own. An inexact zero does not,
-- so 1.0+0.0i stays complex, which is what the report describes. If
-- either part is inexact both become inexact.
makeRectangular :: Number -> Number -> Number
makeRectangular re im
  | isExactZero im = re
  | isInexact re || isInexact im = Complex (toInexact re) (toInexact im)
  | otherwise = Complex re im

makePolar :: Number -> Number -> Number
makePolar mag ang
  | isExactZero ang = mag
  | otherwise =
      let a = toDouble ang
       in makeRectangular (mul mag (Inexact (cos a))) (mul mag (Inexact (sin a)))

isExactZero :: Number -> Bool
isExactZero = \case
  Exact 0 -> True
  _ -> False

isExact :: Number -> Bool
isExact = \case
  Exact _ -> True
  Inexact _ -> False
  Complex a b -> isExact a && isExact b

isInexact :: Number -> Bool
isInexact = not . isExact

-- | (integer? 3.0) is #t, so inexact values count too.
isInteger :: Number -> Bool
isInteger = \case
  Exact r -> denominator r == 1
  Inexact d -> isFiniteDouble d && d == fromIntegral (truncate d :: Integer)
  Complex _ _ -> False

-- | Every finite real number here is rational.
isRational :: Number -> Bool
isRational = \case
  Exact _ -> True
  Inexact d -> isFiniteDouble d
  Complex _ _ -> False

isReal :: Number -> Bool
isReal = \case
  Complex _ _ -> False
  _ -> True

isZero :: Number -> Bool
isZero = \case
  Exact r -> r == 0
  Inexact d -> d == 0
  Complex a b -> isZero a && isZero b

isPositive :: Number -> Bool
isPositive = \case
  Exact r -> r > 0
  Inexact d -> d > 0
  Complex _ _ -> False

isNegative :: Number -> Bool
isNegative = \case
  Exact r -> r < 0
  Inexact d -> d < 0
  Complex _ _ -> False

isFinite :: Number -> Bool
isFinite = \case
  Exact _ -> True
  Inexact d -> isFiniteDouble d
  Complex a b -> isFinite a && isFinite b

isNaN' :: Number -> Bool
isNaN' = \case
  Exact _ -> False
  Inexact d -> isNaN d
  Complex a b -> isNaN' a || isNaN' b

isFiniteDouble :: Double -> Bool
isFiniteDouble d = not (isNaN d) && not (isInfinite d)

-- Conversion

-- | Only meaningful for real numbers. The callers that can see a
-- complex number check first.
toDouble :: Number -> Double
toDouble = \case
  Exact r -> fromRational r
  Inexact d -> d
  Complex a _ -> toDouble a

toComplexDouble :: Number -> Complex Double
toComplexDouble n = toDouble (realPart n) :+ toDouble (imagPartOrZero n)

fromComplexDouble :: Complex Double -> Number
fromComplexDouble (a :+ b) = makeRectangular (Inexact a) (Inexact b)

-- | exact. There is no exact value for an infinity or a NaN.
toExact :: Number -> Eval Number
toExact n@(Exact _) = pure n
toExact (Inexact d)
  | isFiniteDouble d = pure (Exact (toRational d))
  | otherwise = throwScheme (TypeError "a finite number" (Num (Inexact d)))
toExact (Complex a b) = makeRectangular <$> toExact a <*> toExact b

toInexact :: Number -> Number
toInexact = \case
  Complex a b -> Complex (toInexact a) (toInexact b)
  n -> Inexact (toDouble n)

-- | The integer inside, when there is one.
toIntegerMaybe :: Number -> Maybe Integer
toIntegerMaybe = \case
  Exact r | denominator r == 1 -> Just (numerator r)
  Inexact d
    | isFiniteDouble d && d == fromIntegral (truncate d :: Integer) ->
        Just (truncate d)
  _ -> Nothing

-- Complex parts

realPart :: Number -> Number
realPart = \case
  Complex a _ -> a
  n -> n

-- | The imaginary part of a real number has the exactness of the number,
-- so (imag-part 2.5) is 0.0.
imagPart :: Number -> Number
imagPart = \case
  Complex _ b -> b
  n -> if isExact n then Exact 0 else Inexact 0

-- Used inside the arithmetic. An exact zero here means a real number
-- costs nothing extra when it meets a complex one.
imagPartOrZero :: Number -> Number
imagPartOrZero = \case
  Complex _ b -> b
  _ -> Exact 0

magnitude :: Number -> Number
magnitude = \case
  n@(Complex a b)
    | isExact n -> sqrt' (add (mul a a) (mul b b))
    | otherwise -> Inexact (C.magnitude (toComplexDouble n))
  n -> abs' n

angle :: Number -> Number
angle = \case
  n@(Complex _ _) -> Inexact (C.phase (toComplexDouble n))
  n
    | isNegative n -> Inexact pi
    | isExact n -> Exact 0
    | otherwise -> Inexact 0

-- Arithmetic

-- | Contagion for real numbers is handled here and nowhere else. The
-- exact operation runs when both sides are exact. Otherwise both go
-- through Double.
binary ::
  (Rational -> Rational -> Rational) ->
  (Double -> Double -> Double) ->
  Number ->
  Number ->
  Number
binary fe _ (Exact a) (Exact b) = Exact (fe a b)
binary _ fi a b = Inexact (fi (toDouble a) (toDouble b))

add, sub, mul :: Number -> Number -> Number
add a b
  | isReal a && isReal b = binary (+) (+) a b
  | otherwise = makeRectangular (add (realPart a) (realPart b)) (add (imagPartOrZero a) (imagPartOrZero b))
sub a b
  | isReal a && isReal b = binary (-) (-) a b
  | otherwise = makeRectangular (sub (realPart a) (realPart b)) (sub (imagPartOrZero a) (imagPartOrZero b))
mul a b
  | isReal a && isReal b = binary (*) (*) a b
  | otherwise =
      let (ar, ai) = (realPart a, imagPartOrZero a)
          (br, bi) = (realPart b, imagPartOrZero b)
       in makeRectangular (sub (mul ar br) (mul ai bi)) (add (mul ar bi) (mul ai br))

-- | Division. An exact zero divisor is an error. An inexact one gives an
-- infinity. Dividing by a complex number multiplies through by the
-- conjugate, so the divisor becomes a real number and the exactness
-- rules take care of themselves.
divide :: Number -> Number -> Eval Number
divide a b
  | isReal a && isReal b =
      if isExact a && isExact b && isZero b
        then throwScheme (DivideByZero "/")
        else pure (binary (/) (/) a b)
  | isReal b = makeRectangular <$> divide (realPart a) b <*> divide (imagPartOrZero a) b
  | otherwise = do
      let (ar, ai) = (realPart a, imagPartOrZero a)
          (br, bi) = (realPart b, imagPartOrZero b)
          den = add (mul br br) (mul bi bi)
      re <- divide (add (mul ar br) (mul ai bi)) den
      im <- divide (sub (mul ai br) (mul ar bi)) den
      pure (makeRectangular re im)

negate' :: Number -> Number
negate' = \case
  Exact r -> Exact (negate r)
  Inexact d -> Inexact (negate d)
  Complex a b -> Complex (negate' a) (negate' b)

-- | abs on a real number. The primitive rejects complex arguments, but
-- for completeness this gives the magnitude for them.
abs' :: Number -> Number
abs' = \case
  Exact r -> Exact (abs r)
  Inexact d -> Inexact (abs d)
  n -> magnitude n

-- | Shared by quotient, remainder and modulo. Integers only, and the
-- exactness of the arguments carries through.
integerOp :: Text -> (Integer -> Integer -> Integer) -> Number -> Number -> Eval Number
integerOp who f a b = do
  x <- wantInteger who a
  y <- wantInteger who b
  if y == 0
    then throwScheme (DivideByZero who)
    else
      let r = f x y
       in pure (if isExact a && isExact b then Exact (fromInteger r) else Inexact (fromInteger r))

wantInteger :: Text -> Number -> Eval Integer
wantInteger who n = case toIntegerMaybe n of
  Just i -> pure i
  Nothing -> throwScheme (TypeError (who <> " expects an integer") (Num n))

quotient, remainder, modulo :: Number -> Number -> Eval Number
quotient = integerOp "quotient" Prelude.quot
remainder = integerOp "remainder" Prelude.rem
modulo = integerOp "modulo" Prelude.mod

-- | The two-argument core of gcd and lcm. The variadic wrapper is in
-- the primitives. Unlike integerOp these are total, since gcd 0 0 is 0.
gcd', lcm' :: Number -> Number -> Eval Number
gcd' = totalIntegerOp "gcd" Prelude.gcd
lcm' = totalIntegerOp "lcm" Prelude.lcm

totalIntegerOp :: Text -> (Integer -> Integer -> Integer) -> Number -> Number -> Eval Number
totalIntegerOp who f a b = do
  x <- wantInteger who a
  y <- wantInteger who b
  let r = f x y
  pure (if isExact a && isExact b then Exact (fromInteger r) else Inexact (fromInteger r))

-- | expt. An exact base to an exact integer power stays exact, however
-- big it gets, and that includes an exact complex base. A negative real
-- base with a fractional power has a complex answer, so that case goes
-- through the complex exp and log rather than coming back as a NaN.
-- Everything else goes through Double.
expt :: Number -> Number -> Eval Number
expt base power = case (base, toIntegerMaybe power) of
  (Exact b, Just e)
    | isExact power ->
        if
          | e >= 0 -> pure (Exact (b ^ e))
          | b /= 0 -> pure (Exact (recip (b ^ negate e)))
          | otherwise -> throwScheme (DivideByZero "expt")
  (Complex _ _, Just e)
    | isExact power ->
        if e >= 0
          then pure (powBy base e)
          else divide (Exact 1) (powBy base (negate e))
  _
    | isReal base && isReal power && not (isNegative base && not (isInteger power)) ->
        pure (Inexact (toDouble base ** toDouble power))
    | isZero base -> pure (if isZero power then Exact 1 else toInexact base)
    | otherwise ->
        pure (fromComplexDouble (exp (toComplexDouble power * log (toComplexDouble base))))
  where
    powBy :: Number -> Integer -> Number
    powBy _ 0 = Exact 1
    powBy b e
      | even e = let h = powBy b (e `div` 2) in mul h h
      | otherwise = mul b (powBy b (e - 1))

-- | sqrt. Perfect squares, including rational ones, come back exact. A
-- negative real gives an imaginary result, and (sqrt -4) is +2i.
sqrt' :: Number -> Number
sqrt' n@(Exact r)
  | r < 0 = makeRectangular (Exact 0) (sqrt' (Exact (negate r)))
  | Just num <- exactRoot (numerator r)
  , Just den <- exactRoot (denominator r) =
      Exact (num % den)
  | otherwise = Inexact (sqrt (toDouble n))
sqrt' (Inexact d)
  | d < 0 = makeRectangular (Inexact 0) (Inexact (sqrt (negate d)))
  | otherwise = Inexact (sqrt d)
sqrt' n = fromComplexDouble (sqrt (toComplexDouble n))

exactRoot :: Integer -> Maybe Integer
exactRoot n
  | n < 0 = Nothing
  | r * r == n = Just r
  | otherwise = Nothing
  where
    r = integerSquareRoot n

-- | Newton's method on Integer. Going through Double is wrong past 2^53.
integerSquareRoot :: Integer -> Integer
integerSquareRoot n
  | n < 2 = n
  | otherwise = go (initialGuess n)
  where
    initialGuess m = 2 ^ ((integerLog2 m `div` 2) + 1)
    integerLog2 :: Integer -> Int
    integerLog2 m = length (takeWhile (<= m) (iterate (* 2) 1)) - 1
    go guess =
      let next = (guess + n `div` guess) `div` 2
       in if next >= guess then guess else go next

-- | The root and the remainder.
exactIntegerSqrt :: Integer -> (Integer, Integer)
exactIntegerSqrt n = let r = integerSquareRoot n in (r, n - r * r)

-- | Shared by the four rounding operations. Inexact in, inexact out.
-- The primitives only call this on real numbers.
rounder :: (Rational -> Integer) -> Number -> Number
rounder f = \case
  Exact r -> Exact (fromInteger (f r))
  Inexact d
    | isFiniteDouble d ->
        let r = fromInteger (f (toRational d))
         in -- (round -0.5) must be -0.0, so the sign of the input survives.
            Inexact (if r == 0 && (d < 0 || isNegativeZero d) then -0.0 else r)
    | otherwise -> Inexact d
  n -> n

floor', ceiling', truncate', round' :: Number -> Number
floor' = rounder floor
ceiling' = rounder ceiling
truncate' = rounder truncate
-- Prelude's round already rounds halves to even, which is what the report wants.
round' = rounder round

numerator' :: Number -> Eval Number
numerator' = ratioPart "numerator" numerator

denominator' :: Number -> Eval Number
denominator' = ratioPart "denominator" denominator

ratioPart :: Text -> (Rational -> Integer) -> Number -> Eval Number
ratioPart who f = \case
  Exact r -> pure (Exact (fromInteger (f r)))
  Inexact d
    | isFiniteDouble d -> pure (Inexact (fromInteger (f (toRational d))))
  n -> throwScheme (TypeError (who <> " expects a rational") (Num n))

-- | exp, log, sin and the others. Always inexact. The first function is
-- the real version, the second the complex one. A complex argument goes
-- to the second. So does a real argument the first cannot handle, which
-- is how (log -1) and (asin 2) come out complex instead of NaN.
transcendental :: (Double -> Double) -> (Complex Double -> Complex Double) -> Number -> Number
transcendental f g n = case n of
  Complex _ _ -> fromComplexDouble (g (toComplexDouble n))
  _ ->
    let d = toDouble n
        r = f d
     in if isNaN r && not (isNaN d)
          then fromComplexDouble (g (d :+ 0))
          else Inexact r

atan2' :: Number -> Number -> Number
atan2' y x = Inexact (atan2 (toDouble y) (toDouble x))

-- | rationalize. The simplest rational within y of x, using the
-- continued fraction algorithm from the back of the report.
rationalize' :: Number -> Number -> Eval Number
rationalize' x tol
  | isNaN' x || isNaN' tol = pure (Inexact (0 / 0))
  | not (isFinite tol) = pure (if isExact x && isExact tol then Exact 0 else Inexact 0)
  | not (isFinite x) = pure x
  | otherwise = do
      let rx = case x of Exact r -> r; n -> toRational (toDouble n)
          rt = abs (case tol of Exact r -> r; n -> toRational (toDouble n))
          result = simplestRational (rx - rt) (rx + rt)
      pure $
        if isExact x && isExact tol
          then Exact result
          else Inexact (fromRational result)

-- | The simplest rational in a closed interval. Smallest denominator
-- first, then smallest numerator.
simplestRational :: Rational -> Rational -> Rational
simplestRational lo hi
  | lo <= 0 && 0 <= hi = 0
  | hi < 0 = negate (go (negate hi) (negate lo))
  | otherwise = go lo hi
  where
    -- 0 < a <= b
    go a b =
      let fa = floor a :: Integer
       in if fromInteger fa == a
            then a
            else
              if fa < (floor b :: Integer)
                then fromInteger (fa + 1)
                else fromInteger fa + recip (go (recip (b - fromInteger fa)) (recip (a - fromInteger fa)))

-- Comparison

-- | Numeric comparison, which is not eqv?. (= 1 1.0) is true. Nothing
-- when a NaN is involved, and Nothing for a complex number, since there
-- is no order on those. Equality of complex numbers is handled in eqNum.
--
-- A mixed comparison converts the inexact side to Rational rather than
-- the exact side to Double. Done the other way round,
-- @(= (+ (expt 2 53) 1) (inexact (expt 2 53)))@ would come out true.
compareNum :: Number -> Number -> Maybe Ordering
compareNum (Exact a) (Exact b) = Just (compare a b)
compareNum a b
  | not (isReal a) || not (isReal b) = Nothing
  | isNaN' a || isNaN' b = Nothing
  -- Infinities cannot go through toRational, and (expt 10 400) would
  -- overflow to inf as a Double. So compare by sign first.
  | not (isFinite a) || not (isFinite b) = Just (compare (side a) (side b))
  | otherwise = Just (compare (asRational a) (asRational b))
  where
    asRational = \case
      Exact r -> r
      n -> toRational (toDouble n)
    side :: Number -> Int
    side n
      | isFinite n = 0
      | isPositive n = 1
      | otherwise = -1

eqNum, ltNum, leNum, gtNum, geNum :: Number -> Number -> Bool
eqNum a b
  | isReal a && isReal b = compareNum a b == Just EQ
  | otherwise = eqNum (realPart a) (realPart b) && eqNum (imagPartOrZero a) (imagPartOrZero b)
ltNum a b = compareNum a b == Just LT
leNum a b = compareNum a b `elem` [Just LT, Just EQ]
gtNum a b = compareNum a b == Just GT
geNum a b = compareNum a b `elem` [Just GT, Just EQ]

-- External representation

-- | number->string. A radix other than 10 only works for exact
-- numbers, which is the restriction the report places on it.
numberToText :: Int -> Number -> Eval Text
numberToText 10 n = pure (numberToText10 n)
numberToText radix n
  | radix `notElem` [2, 8, 16] =
      throwScheme (TypeError "a radix of 2, 8, 10 or 16" (Num (fromInt radix)))
  | otherwise = case n of
      Complex a b -> do
        re <- exactText a
        _ <- exactText b
        pure (complexText re (imagText radix b))
      _ -> exactText n
  where
    exactText = \case
      Exact r
        | denominator r == 1 -> pure (showAtRadix radix (numerator r))
        | otherwise ->
            pure (showAtRadix radix (numerator r) <> "/" <> showAtRadix radix (denominator r))
      _ -> throwScheme (TypeError "an exact number for a radix other than 10" (Num n))

-- | The decimal rendering. It cannot fail, so it is pure. An inexact
-- integer keeps its .0, because 3.0 and 3 are different objects. A
-- complex number is the real part, the signed imaginary part and an i,
-- with the real part left out when it is an exact zero, so +i and 3-2i
-- read back the way they were written.
numberToText10 :: Number -> Text
numberToText10 = \case
  Exact r
    | denominator r == 1 -> showAtRadix 10 (numerator r)
    | otherwise -> showAtRadix 10 (numerator r) <> "/" <> showAtRadix 10 (denominator r)
  Inexact d
    | isNaN d -> "+nan.0"
    | isInfinite d -> if d > 0 then "+inf.0" else "-inf.0"
    | otherwise -> doubleToText d
  Complex a b -> complexText (numberToText10 a) (imagText 10 b)

-- The real part is left out when it is an exact zero.
complexText :: Text -> Text -> Text
complexText re im = (if re == "0" then "" else re) <> im <> "i"

-- | The imaginary part with its sign. Exact one and minus one print as a
-- bare sign.
imagText :: Int -> Number -> Text
imagText radix = \case
  Exact 1 -> "+"
  Exact (-1) -> "-"
  n ->
    let t = case n of
          Exact r
            | denominator r == 1 -> showAtRadix radix (numerator r)
            | otherwise -> showAtRadix radix (numerator r) <> "/" <> showAtRadix radix (denominator r)
          _ -> numberToText10 n
     in if T.take 1 t `elem` ["+", "-"] then t else "+" <> t

-- | The shortest digits that read back to the same double, written
-- positionally, as in 0.01 or 123456789.0. Extreme exponents get the
-- d.ddde form. Haskell's show switches to exponent form at 0.1 and
-- 10^7, which is not what a Scheme user expects.
doubleToText :: Double -> Text
doubleToText d
  | d == 0 = if isNegativeZero d then "-0.0" else "0.0"
  | d < 0 = "-" <> doubleToText (negate d)
  | otherwise =
      let (digits, e) = floatToDigits 10 d -- d = 0.d1d2... * 10^e
          ds = map intToDigit digits
          n = length ds
       in T.pack $
            if
              | e > 21 || e < -5 ->
                  -- scientific, one digit before the point
                  let (h, t) = splitAt 1 ds
                   in h <> "." <> (if null t then "0" else t) <> "e" <> show (e - 1)
              | e <= 0 -> "0." <> replicate (negate e) '0' <> ds
              | e >= n -> ds <> replicate (e - n) '0' <> ".0"
              | otherwise -> let (h, t) = splitAt e ds in h <> "." <> t

showAtRadix :: Int -> Integer -> Text
showAtRadix radix i
  | i < 0 = "-" <> showAtRadix radix (negate i)
  | otherwise = T.pack $ case radix of
      2 -> showIntAtBase 2 intToDigit i ""
      8 -> showOct i ""
      16 -> showHex i ""
      10 -> showInt i ""
      _ -> showIntAtBase (fromIntegral radix) intToDigit i ""
