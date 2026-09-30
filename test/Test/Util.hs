-- | Helpers the evaluator tests share.
module Test.Util
  ( evalsTo
  , evalCases
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Iota.Interpreter (evalText)
import Test.Tasty
import Test.Tasty.HUnit

-- | The last value of the input, written out, must equal the expected
-- text. Every case gets a fresh interpreter so nothing leaks between
-- tests. That means loading the stdlib each time. It is fast enough that
-- I have not bothered sharing one.
evalsTo :: Text -> Text -> TestTree
evalsTo input expected =
  testCase (T.unpack (T.take 60 input)) $ do
    out <- evalText input
    out @?= expected

-- | A group of @(input, expected)@ pairs under one name.
evalCases :: TestName -> [(Text, Text)] -> TestTree
evalCases name cases =
  testGroup name (map (uncurry evalsTo) cases)
