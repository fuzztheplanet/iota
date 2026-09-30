-- | The repl, on haskeline like the old iota. An unbalanced line gets a
-- continuation prompt instead of a parse error, tab completes over the
-- bound names, and the colon commands from the old one survived.
module Iota.Repl
  ( repl
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, nub, sort)
import Data.Text qualified as T
import Iota.Syntax (globalNames)
import System.IO (hIsTerminalDevice, stdin)
import Iota.Interpreter
import Iota.Printer (writeDatum)
import Iota.Reader (readDatums)
import Iota.Types
import System.Console.Haskeline
import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory (XdgState))
import System.FilePath ((</>))

repl :: Interpreter -> IO ()
repl interp = do
  histFile <- historyPath
  tty <- hIsTerminalDevice stdin
  if tty then putStrLn "iota, an R7RS-small Scheme. Type :help for help." else pure ()
  runInputT (settings histFile) (withInterrupt loop)
  where
    settings :: FilePath -> Settings IO
    settings histFile =
      Settings
        { complete = completer
        , historyFile = Just histFile
        , autoAddHistory = True
        }

    completer :: CompletionFunc IO
    completer = completeWord Nothing " \t\n()[]\"'`," $ \prefix -> do
      names <- globalNames (interpEnv interp)
      let matches =
            [ T.unpack name
            | n <- names
            , let name = symbolName n
            , T.pack prefix `T.isPrefixOf` name
            ]
      pure (map simpleCompletion (sort (nub matches)))

    -- Ctrl-C at the prompt clears the line. Ctrl-C during an evaluation
    -- comes back as an exception through the interpreter boundary and
    -- lands here too.
    loop :: InputT IO ()
    loop = handleInterrupt (outputStrLn "interrupted" >> loop) $ do
      minput <- getInputLine "iota> "
      case minput of
        Nothing -> outputStrLn "Bye." -- ctrl-d
        Just line
          | all (`elem` (" \t" :: String)) line -> loop
          | ":" `isPrefixOf` line -> meta (words line)
          | otherwise -> do
              full <- collect (T.pack line)
              result <- liftIO (runTextForOutput interp "<repl>" full)
              case result of
                "" -> pure () -- zero values
                "#<unspecified>" -> pure () -- don't echo defines
                t -> outputStrLn (T.unpack t)
              loop

    collect :: T.Text -> InputT IO T.Text
    collect acc
      | parenBalance acc <= 0 = pure acc
      | otherwise =
          getInputLine "  ... " >>= \case
            Nothing -> pure acc
            Just more -> collect (acc <> "\n" <> T.pack more)

    meta :: [String] -> InputT IO ()
    meta = \case
      (":q" : _) -> outputStrLn "Bye."
      (":quit" : _) -> outputStrLn "Bye."
      (":help" : _) -> do
        mapM_
          outputStrLn
          [ ":help          show this help"
          , ":ast EXPR      print the parsed form of EXPR"
          , ":env [PREFIX]  list bound names (optionally filtered)"
          , ":load FILE     run FILE in the current environment"
          , ":q, :quit      leave the repl"
          ]
        loop
      (":ast" : rest) -> do
        let src = T.pack (unwords rest)
        case readDatums "<repl>" src of
          Left err -> outputStrLn (T.unpack err)
          Right ds -> mapM_ (outputStrLn . T.unpack . writeDatum) ds
        loop
      (":env" : rest) -> do
        names <- liftIO (globalNames (interpEnv interp))
        let prefix = T.pack (concat (take 1 rest))
            shown = [symbolName n | n <- names, prefix `T.isPrefixOf` symbolName n]
        outputStrLn (T.unpack (T.unwords shown))
        loop
      (":load" : rest@(_ : _)) -> do
        let path = unwords rest
        result <- liftIO (runFile interp path)
        case result of
          Left err -> outputStrLn (T.unpack err)
          Right _ -> outputStrLn ("loaded " <> path)
        loop
      (":load" : _) -> do
        outputStrLn "usage: :load FILE"
        loop
      (cmd : _) -> do
        outputStrLn ("unknown command: " <> cmd <> " (try :help)")
        loop
      [] -> loop

-- | @~\/.local\/state\/iota\/history@, or wherever XDG_STATE_HOME points.
historyPath :: IO FilePath
historyPath = do
  dir <- getXdgDirectory XdgState "iota"
  createDirectoryIfMissing True dir
  pure (dir </> "history")

-- | Open parens minus closed, skipping strings, comments and @#\(@.
-- Only decides whether to show the continuation prompt, so approximate
-- is fine. The reader has the final say.
parenBalance :: T.Text -> Int
parenBalance = go 0 . T.unpack
  where
    go n [] = n
    go n ('"' : rest) = go n (skipString rest)
    go n (';' : rest) = go n (dropWhile (/= '\n') rest)
    -- #\( is a character, not an open paren. Skip the char after #\.
    go n ('#' : '\\' : _ : rest) = go n rest
    go n ('#' : '|' : rest) = go n (skipBlock (1 :: Int) rest)
    go n (c : rest)
      | c `elem` ("([" :: String) = go (n + 1) rest
      | c `elem` (")]" :: String) = go (n - 1) rest
      | otherwise = go n rest

    skipString [] = []
    skipString ('\\' : _ : rest) = skipString rest
    skipString ('"' : rest) = rest
    skipString (_ : rest) = skipString rest

    skipBlock _ [] = []
    skipBlock 1 ('|' : '#' : rest) = rest
    skipBlock d ('|' : '#' : rest) = skipBlock (d - 1) rest
    skipBlock d ('#' : '|' : rest) = skipBlock (d + 1) rest
    skipBlock d (_ : rest) = skipBlock d rest
