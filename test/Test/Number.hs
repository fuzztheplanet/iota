-- | The numeric tower, one case at a time.
module Test.Number (tests) where

import Iota.Number qualified as N
import Iota.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Number"
    [ testCase "exact addition stays exact" $
        N.add (Exact 1) (Exact 2) @?= Exact 3
    , testCase "contagion: exact + inexact = inexact" $
        N.add (Exact 1) (Inexact 2.5) @?= Inexact 3.5
    , testCase "rational arithmetic is precise" $
        N.add (Exact (1 / 3)) (Exact (1 / 6)) @?= Exact (1 / 2)
    , testCase "= crosses exactness" $
        assertBool "1 = 1.0" (N.eqNum (Exact 1) (Inexact 1.0))
    , testCase "NaN compares unequal to itself" $
        assertBool "nan /= nan" (not (N.eqNum (Inexact (0 / 0)) (Inexact (0 / 0))))
    , testCase "sqrt of a perfect square is exact" $
        N.sqrt' (Exact 16) @?= Exact 4
    , testCase "sqrt of a non-square is inexact" $
        N.sqrt' (Exact 2) @?= Inexact (sqrt 2)
    , testCase "exact-integer-sqrt" $
        N.exactIntegerSqrt 17 @?= (4, 1)
    , testCase "round is banker's rounding" $ do
        N.round' (Exact (5 / 2)) @?= Exact 2 -- 2.5 -> 2
        N.round' (Exact (7 / 2)) @?= Exact 4 -- 3.5 -> 4
    , testCase "floor/ceiling/truncate on negatives" $ do
        N.floor' (Exact (-7 / 2)) @?= Exact (-4)
        N.ceiling' (Exact (-7 / 2)) @?= Exact (-3)
        N.truncate' (Exact (-7 / 2)) @?= Exact (-3)
    , testCase "decimal rendering keeps .0 on inexact integers" $
        N.numberToText10 (Inexact 3.0) @?= "3.0"
    , testCase "rationals render as n/d" $
        N.numberToText10 (Exact (22 / 7)) @?= "22/7"
    , testCase "doubles render positionally" $ do
        N.numberToText10 (Inexact 0.01) @?= "0.01"
        N.numberToText10 (Inexact 1e7) @?= "10000000.0"
        N.numberToText10 (Inexact 123456789.0) @?= "123456789.0"
        N.numberToText10 (Inexact 0.1) @?= "0.1"
        N.numberToText10 (Inexact 1e21) @?= "1.0e21"
        N.numberToText10 (Inexact 1.5e-7) @?= "1.5e-7"
        N.numberToText10 (Inexact (-0.0)) @?= "-0.0"
    , testCase "exact numbers are not below infinity's shadow" $ do
        N.compareNum (Exact (10 ^ (400 :: Int))) (Inexact (1 / 0)) @?= Just LT
        N.compareNum (Inexact (-1 / 0)) (Exact (-(10 ^ (400 :: Int)))) @?= Just LT
    , testCase "infinities render like the reader spells them" $ do
        N.numberToText10 (Inexact (1 / 0)) @?= "+inf.0"
        N.numberToText10 (Inexact (-1 / 0)) @?= "-inf.0"
    , testCase "an exact zero imaginary part is not a complex number" $ do
        N.makeRectangular (Exact 1) (Exact 0) @?= Exact 1
        N.makeRectangular (Inexact 1) (Inexact 0) @?= Complex (Inexact 1) (Inexact 0)
    , testCase "one inexact part makes both inexact" $
        N.makeRectangular (Exact 1) (Inexact 2) @?= Complex (Inexact 1) (Inexact 2)
    , testCase "complex arithmetic stays exact" $ do
        N.mul (Complex (Exact 0) (Exact 1)) (Complex (Exact 0) (Exact 1)) @?= Exact (-1)
        N.add (Complex (Exact 1) (Exact 2)) (Complex (Exact 1) (Exact (-2))) @?= Exact 2
    , testCase "sqrt of a negative number is imaginary" $ do
        N.sqrt' (Exact (-4)) @?= Complex (Exact 0) (Exact 2)
        N.sqrt' (Inexact (-4)) @?= Complex (Inexact 0) (Inexact 2)
    , testCase "complex numbers render the way they are written" $ do
        N.numberToText10 (Complex (Exact 0) (Exact 1)) @?= "+i"
        N.numberToText10 (Complex (Exact 3) (Exact (-1))) @?= "3-i"
        N.numberToText10 (Complex (Exact 1) (Exact 2)) @?= "1+2i"
        N.numberToText10 (Complex (Inexact 1.5) (Inexact (-2))) @?= "1.5-2.0i"
        N.numberToText10 (Complex (Inexact 0) (Inexact (1 / 0))) @?= "0.0+inf.0i"
    ]
