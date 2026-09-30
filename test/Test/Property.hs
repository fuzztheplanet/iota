-- | QuickCheck properties. Writing then reading gives the datum back, and
-- numbers survive a trip through their decimal rendering.
module Test.Property (tests) where

import Data.Char (isPrint)
import Data.Text (Text)
import Data.Text qualified as T
import Iota.Number qualified as N
import Iota.Printer (writeDatum)
import Iota.Reader (numberFromText, readDatum)
import Iota.Types
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "properties"
    [ testProperty "write then read is the identity on data" $
        forAll genDatum $ \d -> readDatum "<prop>" (writeDatum d) === Right d
    , testProperty "doubles round-trip through their decimal rendering" $
        \(NonNaN d) ->
          numberFromText 10 (N.numberToText10 (Inexact d)) === Just (Inexact d)
    , testProperty "exact rationals round-trip" $
        \(n :: Integer) (Positive (den :: Integer)) ->
          let r = fromInteger n / fromInteger den
           in numberFromText 10 (N.numberToText10 (Exact r)) === Just (Exact r)
    , testProperty "the decimal rendering is positional for ordinary magnitudes" $
        \(NonNaN d) ->
          (abs d >= 1e-5 && abs d < 1e21) ==> not (T.any (== 'e') (N.numberToText10 (Inexact d)))
    ]

newtype NonNaN = NonNaN Double
  deriving stock (Show)

instance Arbitrary NonNaN where
  arbitrary = NonNaN <$> (arbitrary `suchThat` (\d -> not (isNaN d) && not (isInfinite d)))
  shrink (NonNaN d) = [NonNaN d' | d' <- shrink d, not (isNaN d'), not (isInfinite d')]

-- | Random data, limited to what round-trips. No NaN, because it is not
-- equal to itself. Printable strings only. Symbols the printer either
-- leaves alone or wraps in bars.
genDatum :: Gen Datum
genDatum = sized go
  where
    -- A third of the size per level, otherwise the data gets huge.
    children n = do
      k <- choose (0, min 4 n)
      vectorOf k (go (n `div` 3))
    go n =
      frequency
        [ (2, DBool <$> arbitrary)
        , (3, DNumber <$> genNumber)
        , (1, DNumber <$> genComplex)
        , (2, DChar <$> genChar)
        , (2, DString <$> genText)
        , (3, DSymbol <$> genSymbol)
        , (if n > 0 then 3 else 0, DList <$> children n <*> pure Nothing)
        , (if n > 0 then 1 else 0, (\xs tl -> DList xs (Just tl)) <$> (children n `suchThat` (not . null)) <*> atom)
        , (if n > 0 then 1 else 0, DVector <$> children n)
        , (1, DBytevector <$> listOf arbitrary)
        ]
    atom = oneof [DNumber <$> genNumber, DNumber <$> genComplex, DSymbol <$> genSymbol, DBool <$> arbitrary]
    genNumber =
      oneof
        [ Exact . fromInteger <$> arbitrary
        , (\a (Positive b) -> Exact (fromInteger a / fromInteger b)) <$> arbitrary <*> arbitrary
        , (\(NonNaN d) -> Inexact d) <$> arbitrary
        ]
    -- complex numbers are built from two reals so the invariant holds
    genComplex = N.makeRectangular <$> genNumber <*> genNumber
    genChar = arbitrary `suchThat` (\c -> isPrint c || c `elem` ("\n\t " :: String))
    genText = T.pack <$> listOf genChar
    genSymbol = Symbol . T.pack <$> oneof [plain, listOf1 genChar]
    plain = listOf1 (elements (['a' .. 'z'] <> "-+*/<>=?!"))

-- Keeps the Text import from being reported as unused.
_unusedText :: Text
_unusedText = ""
