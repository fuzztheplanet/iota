-- | The command line, without starting a process. Argument lists go
-- through 'parserInfo', the same value 'Iota.CLI.main' hands to
-- optparse, so the forwarding rules and the version and help options
-- are the real ones. The cases follow the section "The command line" of
-- docs/guide.org.
module Test.CLI (tests) where

import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import Iota.CLI (Command (..), parserInfo)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, renderFailure)
import System.Exit (ExitCode (..))
import Test.Tasty
import Test.Tasty.HUnit

-- | What 'Iota.CLI.main' would do with these arguments. Either the
-- command it would run, or the text it would print and the status it
-- would exit with.
parse :: [String] -> Either (String, ExitCode) Command
parse args = case execParserPure defaultPrefs parserInfo args of
  Success cmd -> Right cmd
  Failure failure -> Left (renderFailure failure "iota")
  CompletionInvoked _ -> Left ("completion invoked", ExitFailure 1)

parsesTo :: [String] -> Command -> TestTree
parsesTo args expected =
  testCase ("iota " <> unwords args) (parse args @?= Right expected)

-- | The arguments make iota print something and exit successfully,
-- which is how optparse reports --version and --help.
printsAndExits :: [String] -> (String -> Bool) -> TestTree
printsAndExits args ok =
  testCase ("iota " <> unwords args) $ case parse args of
    Left (msg, ExitSuccess) -> assertBool ("unexpected text: " <> msg) (ok msg)
    other -> assertFailure ("expected text and exit status 0, got: " <> show other)

-- | A usage error. Text on standard error and a non-zero status.
isRejected :: [String] -> TestTree
isRejected args =
  testCase ("iota " <> unwords args <> " is a usage error") $ case parse args of
    Left (_, ExitFailure _) -> pure ()
    other -> assertFailure ("expected a usage error, got: " <> show other)

-- | @iota X.Y@, whatever the version in the cabal file is.
isVersionLine :: String -> Bool
isVersionLine msg = case words msg of
  ["iota", v@(c : _)] -> isDigit c && all (\x -> isDigit x || x == '.') v
  _ -> False

isUsage :: String -> Bool
isUsage msg = "Usage: iota" `isInfixOf` msg

tests :: TestTree
tests =
  testGroup
    "command line"
    [ testGroup
        "the repl"
        [ [] `parsesTo` Repl
        , ["repl"] `parsesTo` Repl
        ]
    , testGroup
        "running a script"
        [ ["prog.iota"] `parsesTo` Run "prog.iota" []
        , ["prog.iota", "a", "b"] `parsesTo` Run "prog.iota" ["a", "b"]
        , ["run", "prog.iota"] `parsesTo` Run "prog.iota" []
        , ["run", "prog.iota", "a", "b"] `parsesTo` Run "prog.iota" ["a", "b"]
        , -- a file may be called like a subcommand if it is spelled out
          ["run", "eval"] `parsesTo` Run "eval" []
        ]
    , testGroup
        "evaluating an expression"
        [ ["eval", "(+ 1 2)"] `parsesTo` EvalExpr "(+ 1 2)"
        , -- the text is passed on as it is, several forms included
          ["eval", "(define x 1) x"] `parsesTo` EvalExpr "(define x 1) x"
        , isRejected ["eval"]
        , isRejected ["run"]
        ]
    , testGroup
        "options after the script name go to the script"
        [ ["prog.iota", "--verbose"] `parsesTo` Run "prog.iota" ["--verbose"]
        , ["prog.iota", "--verbose", "-x", "a", "b"] `parsesTo` Run "prog.iota" ["--verbose", "-x", "a", "b"]
        , ["prog.iota", "a", "--verbose"] `parsesTo` Run "prog.iota" ["a", "--verbose"]
        , ["prog.iota", "--name=value"] `parsesTo` Run "prog.iota" ["--name=value"]
        ]
    , testGroup
        "everything after -- goes to the script"
        [ ["prog.iota", "--", "--verbose"] `parsesTo` Run "prog.iota" ["--verbose"]
        , ["prog.iota", "--", "--help"] `parsesTo` Run "prog.iota" ["--help"]
        , ["prog.iota", "--", "--version", "-V", "-h"] `parsesTo` Run "prog.iota" ["--version", "-V", "-h"]
        , ["run", "prog.iota", "--", "--verbose"] `parsesTo` Run "prog.iota" ["--verbose"]
        , ["run", "prog.iota", "--", "--help"] `parsesTo` Run "prog.iota" ["--help"]
        , ["run", "prog.iota", "a", "--", "-x", "b"] `parsesTo` Run "prog.iota" ["a", "-x", "b"]
        , -- the script name can come after the separator too
          ["--", "prog.iota", "--help"] `parsesTo` Run "prog.iota" ["--help"]
        , -- only the first separator is dropped. A later one is an argument
          ["prog.iota", "--", "a", "--", "b"] `parsesTo` Run "prog.iota" ["a", "--", "b"]
        , ["run", "prog.iota", "--", "a", "--", "b"] `parsesTo` Run "prog.iota" ["a", "--", "b"]
        , ["prog.iota", "--", "--help", "--", "x"] `parsesTo` Run "prog.iota" ["--help", "--", "x"]
        , -- an expression that starts with a dash needs the separator
          ["eval", "--", "-5"] `parsesTo` EvalExpr "-5"
        ]
    , testGroup
        "an unknown option before the script name is taken for the script"
        [ ["--verbose", "prog.iota"] `parsesTo` Run "--verbose" ["prog.iota"]
        , ["-x", "prog.iota", "a"] `parsesTo` Run "-x" ["prog.iota", "a"]
        ]
    , testGroup
        "the version"
        [ printsAndExits ["--version"] isVersionLine
        , printsAndExits ["-V"] isVersionLine
        , -- iota's own, even after a script name
          printsAndExits ["prog.iota", "--version"] isVersionLine
        ]
    , testGroup
        "the usage"
        [ -- the top level usage names the subcommands and the options
          printsAndExits ["--help"] (\msg -> isUsage msg && all (`isInfixOf` msg) ["repl", "run", "eval", "--version"])
        , printsAndExits ["-h"] isUsage
        , -- iota's own, even after a script name
          printsAndExits ["prog.iota", "--help"] isUsage
        , printsAndExits ["run", "--help"] ("Usage: iota run FILE" `isPrefixOf`)
        , printsAndExits ["eval", "--help"] ("Usage: iota eval EXPR" `isPrefixOf`)
        ]
    ]
