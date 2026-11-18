-- | One tasty tree for everything. Reader, numbers and printer first,
-- then the evaluator cases, the command line, the golden programs, and
-- last the examples and the documentation, which are run as well.
module Main (main) where

import Test.CLI qualified
import Test.Coverage qualified
import Test.Docs qualified
import Test.Eval qualified
import Test.Examples qualified
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
  examples <- Test.Examples.tests
  docs <- Test.Docs.tests
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
      , Test.CLI.tests
      , Test.Coverage.tests
      , golden
      , examples
      , docs
      ]
