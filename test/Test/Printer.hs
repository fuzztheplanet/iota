-- | write and display against fixed strings.
module Test.Printer (tests) where

import Data.Text (Text)
import Iota.Printer (writeDatum)
import Iota.Reader (readDatum)
import Test.Tasty
import Test.Tasty.HUnit

-- | Read the input, write it back, compare. This checks the reader and
-- the printer against each other.
roundTrips :: Text -> Text -> TestTree
roundTrips input expected =
  testCase (show input) $
    case readDatum "<test>" input of
      Left err -> assertFailure (show err)
      Right d -> writeDatum d @?= expected

tests :: TestTree
tests =
  testGroup
    "Printer"
    [ roundTrips "42" "42"
    , roundTrips "1/2" "1/2"
    , roundTrips "3.5" "3.5"
    , roundTrips "#t" "#t"
    , roundTrips "()" "()"
    , roundTrips "(1 2 3)" "(1 2 3)"
    , roundTrips "(1 . 2)" "(1 . 2)"
    , roundTrips "(1 2 . 3)" "(1 2 . 3)"
    , roundTrips "\"hi\\nthere\"" "\"hi\\nthere\""
    , roundTrips "#\\a" "#\\a"
    , roundTrips "#\\space" "#\\space"
    , roundTrips "#\\newline" "#\\newline"
    , roundTrips "#(1 #(2))" "#(1 #(2))"
    , roundTrips "#u8(1 2 255)" "#u8(1 2 255)"
    , roundTrips "|odd symbol|" "|odd symbol|"
    , -- Quote abbreviations survive the round trip.
      roundTrips "'x" "'x"
    , roundTrips "`(a ,b ,@c)" "`(a ,b ,@c)"
    ]
