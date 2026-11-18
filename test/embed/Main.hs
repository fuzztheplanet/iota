-- | Runs the embedding example, examples/haskell/Embed.hs, and compares
-- what it prints with examples/haskell/Embed.expected.
--
-- This is a test suite of its own because the example writes to the
-- real standard output, as a program that embeds iota would. In the
-- main suite other tests and the test reporter write there at the same
-- time, so its output could not be told apart from theirs.
module Main (main) where

import Control.Exception (bracket, finally)
import qualified Embed
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (exitFailure)
import System.IO

exampleFile, expectedFile :: FilePath
exampleFile = "examples/haskell/Embed.hs"
expectedFile = "examples/haskell/Embed.expected"

-- | Run an action with the standard output going to a file, and return
-- what it wrote.
capture :: IO () -> IO String
capture action = do
  dir <- getTemporaryDirectory
  bracket (openTempFile dir "iota-embed.out") (\(path, h) -> hClose h >> removeFile path) $ \(path, h) -> do
    hFlush stdout
    saved <- hDuplicate stdout
    hDuplicateTo h stdout
    (action >> hFlush stdout) `finally` (hDuplicateTo saved stdout >> hClose saved)
    hClose h
    text <- readFile path
    length text `seq` pure text

main :: IO ()
main = do
  expected <- readFile expectedFile
  actual <- capture Embed.main
  if actual == expected
    then putStrLn (exampleFile ++ " prints what " ++ expectedFile ++ " says")
    else do
      hPutStr stderr (exampleFile ++ " printed\n" ++ indent actual ++ "and " ++ expectedFile ++ " has\n" ++ indent expected)
      exitFailure
  where
    indent = unlines . map ("  " ++) . lines
