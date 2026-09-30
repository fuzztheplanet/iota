-- | One tasty tree for everything. Reader, numbers and printer first,
-- then the evaluator cases, then the golden programs.
module Main (main) where

import Test.Eval qualified
import Test.Golden qualified
import Test.Macro qualified
import Test.Ports qualified
import Test.Property qualified
import Test.Number qualified
import Test.Printer qualified
import Test.Reader qualified
import Test.Scheme qualified
import Test.Tasty

main :: IO ()
main = do
  golden <- Test.Golden.tests
  defaultMain $
    testGroup
      "iota"
      [ Test.Reader.tests
      , Test.Number.tests
      , Test.Printer.tests
      , Test.Eval.tests
      , Test.Scheme.tests
      , Test.Macro.tests
      , Test.Property.tests
      , Test.Ports.tests
      , golden
      ]
