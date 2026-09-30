-- | Reader tests. Text in, Datum out, compared with plain equality.
module Test.Reader (tests) where

import Data.Text (Text)
import Iota.Reader (ReadFailure (..), numberFromText, readDatum, readDatumFromText)
import Iota.Types
import Test.Tasty
import Test.Tasty.HUnit

readsTo :: Text -> Datum -> TestTree
readsTo input expected =
  testCase (show input) (readDatum "<test>" input @?= Right expected)

failsToRead :: Text -> TestTree
failsToRead input =
  testCase ("rejects " <> show input) $
    case readDatum "<test>" input of
      Left _ -> pure ()
      Right d -> assertFailure ("unexpectedly read: " <> show d)

sym :: Text -> Datum
sym = DSymbol . Symbol

int :: Integer -> Datum
int = DNumber . Exact . fromInteger

tests :: TestTree
tests =
  testGroup
    "Reader"
    [ testGroup
        "atoms"
        [ readsTo "foo" (sym "foo")
        , readsTo "+" (sym "+")
        , readsTo "-" (sym "-")
        , readsTo "..." (sym "...")
        , readsTo "1+" (sym "1+")
        , readsTo "list->vector" (sym "list->vector")
        , readsTo "|two words|" (sym "two words")
        , readsTo "#t" (DBool True)
        , readsTo "#false" (DBool False)
        ]
    , testGroup
        "numbers"
        [ readsTo "42" (int 42)
        , readsTo "-17" (int (-17))
        , readsTo "1/2" (DNumber (Exact (1 / 2)))
        , readsTo "-3/4" (DNumber (Exact (-3 / 4)))
        , readsTo "3.14" (DNumber (Inexact 3.14))
        , readsTo ".5" (DNumber (Inexact 0.5))
        , readsTo "1e3" (DNumber (Inexact 1000.0))
        , readsTo "#xff" (int 255)
        , readsTo "#b1010" (int 10)
        , readsTo "#o777" (int 511)
        , readsTo "#e1.5" (DNumber (Exact (3 / 2)))
        , readsTo "#i2" (DNumber (Inexact 2.0))
        , readsTo "+inf.0" (DNumber (Inexact (1 / 0)))
        , readsTo "+i" (DNumber (Complex (Exact 0) (Exact 1)))
        , readsTo "-i" (DNumber (Complex (Exact 0) (Exact (-1))))
        , readsTo "1-2i" (DNumber (Complex (Exact 1) (Exact (-2))))
        , readsTo "1/2+3/4i" (DNumber (Complex (Exact (1 / 2)) (Exact (3 / 4))))
        , readsTo "-2.5i" (DNumber (Complex (Inexact 0) (Inexact (-2.5))))
        , readsTo "1+0i" (DNumber (Exact 1))
        , readsTo "2@0" (DNumber (Exact 2))
        , readsTo "#x1+ai" (DNumber (Complex (Exact 1) (Exact 10)))
        , readsTo "1+" (DSymbol "1+")
        , readsTo "+" (DSymbol "+")
        , testCase "string->number rejects junk" $
            numberFromText 10 "12abc" @?= Nothing
        , testCase "string->number radix 16" $
            numberFromText 16 "ff" @?= Just (Exact 255)
        ]
    , testGroup
        "characters and strings"
        [ readsTo "#\\a" (DChar 'a')
        , readsTo "#\\space" (DChar ' ')
        , readsTo "#\\newline" (DChar '\n')
        , readsTo "#\\x41" (DChar 'A')
        , readsTo "\"hello\"" (DString "hello")
        , readsTo "\"a\\nb\"" (DString "a\nb")
        , readsTo "\"say \\\"hi\\\"\"" (DString "say \"hi\"")
        , readsTo "\"\\x41;\"" (DString "A")
        ]
    , testGroup
        "lists"
        [ readsTo "()" (DList [] Nothing)
        , readsTo "(1 2 3)" (DList [int 1, int 2, int 3] Nothing)
        , readsTo "(1 . 2)" (DList [int 1] (Just (int 2)))
        , readsTo "(1 2 . 3)" (DList [int 1, int 2] (Just (int 3)))
        , readsTo "[1 2]" (DList [int 1, int 2] Nothing)
        , readsTo "(a (b c))" (DList [sym "a", DList [sym "b", sym "c"] Nothing] Nothing)
        , failsToRead "(1 2"
        , failsToRead ")"
        , failsToRead "( . 3)" -- a dotted tail needs a car
        ]
    , testGroup
        "abbreviations"
        [ readsTo "'x" (DList [sym "quote", sym "x"] Nothing)
        , readsTo "`x" (DList [sym "quasiquote", sym "x"] Nothing)
        , readsTo ",x" (DList [sym "unquote", sym "x"] Nothing)
        , readsTo ",@x" (DList [sym "unquote-splicing", sym "x"] Nothing)
        , readsTo "''x" (DList [sym "quote", DList [sym "quote", sym "x"] Nothing] Nothing)
        ]
    , testGroup
        "vectors and bytevectors"
        [ readsTo "#(1 2)" (DVector [int 1, int 2])
        , readsTo "#()" (DVector [])
        , readsTo "#u8(0 255)" (DBytevector [0, 255])
        , failsToRead "#u8(256)"
        , failsToRead "#(1 . 2)" -- vectors have no dotted tail
        , failsToRead "#\\x110000" -- beyond the Unicode range
        , failsToRead "\"\\xd800;\"" -- surrogate escape
        ]
    , testGroup
        "datum labels and directives"
        [ readsTo "#0=(a . #0#)" (DLabel 0 (DList [sym "a"] (Just (DLabelRef 0))))
        , readsTo "(#1=x #1#)" (DList [DLabel 1 (sym "x"), DLabelRef 1] Nothing)
        , readsTo "#!fold-case ABC" (sym "abc")
        , readsTo "#!fold-case (A #!no-fold-case B)" (DList [sym "a", sym "B"] Nothing)
        , failsToRead "#!bogus x"
        , failsToRead "#b#b101" -- one radix prefix only
        , failsToRead "#e#e1" -- one exactness prefix only
        , readsTo "#e1e3" (int 1000)
        , readsTo "#x#e10" (int 16)
        , failsToRead "#foo"
        , testCase "incomplete input is reported as such" $
            readDatumFromText "(1 2" @?= Left Incomplete
        , testCase "malformed input is not incomplete" $
            case readDatumFromText ") 1" of
              Left (Malformed _) -> pure ()
              other -> assertFailure ("unexpected: " <> show other)
        , testCase "-0.0 keeps its sign" $
            case readDatum "<test>" "-0.0" of
              Right (DNumber (Inexact d)) -> assertBool "negative zero" (isNegativeZero d)
              other -> assertFailure ("unexpected: " <> show other)
        , testCase "#e-0.0 is exact zero" $
            readDatum "<test>" "#e-0.0" @?= Right (int 0)
        , testCase "a datum and the rest" $
            fmap (fmap fst) (readDatumFromText " 1 2") @?= Right (Just (int 1))
        ]
    , testGroup
        "comments"
        [ readsTo "1 ; the loneliest number" (int 1)
        , readsTo "#| block |# 2" (int 2)
        , readsTo "#| nested #| deeper |# still |# 3" (int 3)
        , readsTo "#;(ignored me) 4" (int 4)
        , readsTo "(1 #;2 3)" (DList [int 1, int 3] Nothing)
        ]
    ]
