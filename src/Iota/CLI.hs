-- | The command line.
--
-- @
-- iota                      start the repl
-- iota repl                 the same, spelled out
-- iota FILE [ARG...]        run a script
-- iota run FILE [ARG...]    the same, spelled out
-- iota eval EXPR            evaluate EXPR and print the value of its last form
-- iota -V, iota --version   print the version and exit
-- iota -h, iota --help      print the usage and exit
-- @
--
-- The reference for all of this, corner cases included, is the section
-- \"The command line\" of the user's guide, docs\/guide.org. If the two
-- disagree, fix this comment.
--
-- This is in the library rather than app\/Main.hs so the tests can reach
-- 'parserInfo'. Test.CLI runs argument lists through it.
module Iota.CLI
  ( main
  , Command (..)
  , parseCommand
  , parserInfo
  ) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import Iota.Interpreter
import Iota.Printer (writeText)
import Iota.Repl (repl)
import Options.Applicative
import Paths_iota (version)
import System.Environment (withArgs)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stderr, stdin, stdout, utf8)

-- | What the arguments asked for.
data Command
  = -- | No arguments, or the @repl@ subcommand.
    Repl
  | -- | The script and the arguments it should see in @(command-line)@
    -- after its own name.
    Run FilePath [String]
  | -- | The source text given to the @eval@ subcommand. It may hold
    -- several forms. They all run and the value of the last is printed.
    EvalExpr String
  deriving stock (Eq, Show)

-- | The subcommands, then a bare @FILE [ARG...]@, then nothing at all for
-- the repl. This is only the part that produces a 'Command'. The version
-- and help options and the forwarding of unknown options to the script
-- are added in 'parserInfo', which is what 'main' and the tests run.
parseCommand :: Parser Command
parseCommand =
  hsubparser
    ( command "repl" (info (pure Repl) (progDesc "Start the interactive repl"))
        <> command "run" (info runParser (progDesc "Run a Scheme program"))
        <> command
          "eval"
          ( info
              (EvalExpr <$> strArgument (metavar "EXPR" <> help "Expression to evaluate"))
              (progDesc "Evaluate one expression and print the result")
          )
    )
    -- iota file.iota with no subcommand runs the file
    <|> runParser
    <|> pure Repl
  where
    runParser =
      Run
        <$> strArgument (metavar "FILE" <> help "Scheme source file")
        <*> many (strArgument (metavar "ARG..." <> help "Arguments passed to the script"))

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("iota " <> showVersion version)
    (long "version" <> short 'V' <> help "Show the version and exit")

-- | The whole command line parser, as 'main' runs it. 'parseCommand'
-- plus @--version@ and @--help@, with unknown options forwarded.
--
-- In the bare @iota FILE ARG...@ form an option iota does not know is
-- handed to the script. @--help@, @-h@, @--version@ and @-V@ are iota's
-- own wherever they appear, and after the @run@ subcommand an unknown
-- option is rejected. Everything after a @--@ goes to the script
-- untouched in both forms.
parserInfo :: ParserInfo Command
parserInfo =
  info
    (parseCommand <**> versionOption <**> helper)
    ( fullDesc
        -- an unrecognised option after the script name belongs to the
        -- script, not to optparse
        <> forwardOptions
        <> progDesc "An R7RS-small Scheme interpreter"
        <> header "iota, a small Scheme"
    )

-- | Parse the arguments and do what they say. A script or an @eval@ that
-- ends in an uncaught error prints the message on standard error and
-- exits with status 1.
main :: IO ()
main = do
  cmd <- execParser parserInfo
  -- UTF-8 for the standard streams whatever the locale says. Source files
  -- are read as UTF-8 as well, so this keeps the two consistent.
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  interp <- newInterpreter
  -- (command-line) reads getArgs, so rewrite argv to what the report
  -- wants first, the script name followed by the script's own arguments.
  case cmd of
    Repl -> withArgs ["iota"] (repl interp)
    Run path scriptArgs ->
      withArgs (path : scriptArgs) $
        runFile interp path >>= \case
          Left err -> do
            TIO.hPutStrLn stderr err
            exitFailure
          Right _ -> pure ()
    EvalExpr expr ->
      withArgs ["iota"] $
        runText interp "<eval>" (T.pack expr) >>= \case
          Left err -> do
            TIO.hPutStrLn stderr err
            exitFailure
          Right v -> do
            out <- writeText v
            -- same convention as the repl
            case out of
              "" -> pure ()
              "#<unspecified>" -> pure ()
              t -> TIO.putStrLn t
