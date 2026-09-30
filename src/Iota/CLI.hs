-- | The command line.
--
-- @
-- iota                    repl
-- iota repl               repl
-- iota FILE [ARG...]      run a script
-- iota run FILE [ARG...]  same, spelled out
-- iota eval EXPR          evaluate one expression and print it
-- iota --version
-- @
--
-- This is in the library rather than app/Main.hs so the tests can reach
-- 'parseCommand'.
module Iota.CLI
  ( main
  , Command (..)
  , parseCommand
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

data Command
  = Repl
  | -- | the script and the arguments it should see in @(command-line)@
    Run FilePath [String]
  | EvalExpr String
  deriving stock (Eq, Show)

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

main :: IO ()
main = do
  cmd <-
    execParser $
      info
        (parseCommand <**> versionOption <**> helper)
        ( fullDesc
            -- an unrecognised option after the script name belongs to the
            -- script, not to optparse
            <> forwardOptions
            <> progDesc "An R7RS-small Scheme interpreter"
            <> header "iota, a small Scheme"
        )
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
