-- | The programs under examples/, as golden tests. Each
-- examples/NAME.iota runs in its own interpreter and what it wrote to
-- the output port is compared with test/golden/examples/NAME.golden.
-- Only the files directly in examples/ are programs. The directories
-- below it hold the libraries they import.
--
-- An example has to behave here as it does from the command line, so it
-- sees its own path in (command-line) and finds libraries next to
-- itself. What it writes to the error port is dropped, which leaves
-- room for output that changes from run to run, a timing for example.
-- An example that ends in an uncaught error or a failing exit status
-- fails the test whatever it printed.
--
-- Running the tests with @--accept -p Examples@ rewrites these golden
-- files and no others. Read the diff before committing them.
module Test.Examples (tests) where

import Control.Exception (Exception, IOException, displayException, throwIO, try)
import Control.Monad (filterM)
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Iota.Interpreter (captureOutput, newInterpreter, runFile)
import System.Directory (doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (replaceExtension, takeBaseName, takeExtension, (</>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Util (discardErrorOutput, withCommandLine)

examplesDir, goldenDir :: FilePath
examplesDir = "examples"
goldenDir = "test/golden/examples"

tests :: IO TestTree
tests = do
  r <- try (listDirectory examplesDir >>= filterM (doesFileExist . (examplesDir </>)))
  pure $ testGroup "Examples" $ case r of
    Left (e :: IOException) ->
      [testCase examplesDir (assertFailure (examplesDir <> " cannot be listed: " <> displayException e))]
    Right names ->
      let programs = [name | name <- sort names, takeExtension name == ".iota"]
       in [ goldenVsString
              (takeBaseName name)
              (goldenDir </> replaceExtension name ".golden")
              (runExample (examplesDir </> name))
          | name <- programs
          ]
            <> [ -- A golden file is created when it is missing, so a renamed
                 -- or deleted example would leave its old one behind.
                 testCase "every golden file belongs to an example" $ do
                  goldens <- try (listDirectory goldenDir)
                  let expected = [replaceExtension name ".golden" | name <- programs]
                  case goldens of
                    Left (_ :: IOException) -> pure ()
                    Right found -> [g | g <- sort found, g `notElem` expected] @?= []
               ]

-- | The example sees its own path in (command-line), as it would when
-- started as @iota examples/NAME.iota@.
runExample :: FilePath -> IO BL.ByteString
runExample path = withCommandLine [path] $ do
  interp <- newInterpreter
  (r, out) <- captureOutput interp (discardErrorOutput interp (try (runFile interp path)))
  case r of
    Right (Right _) -> pure ()
    Right (Left err) -> failed ("ended with an uncaught error:\n" <> T.unpack err)
    Left ExitSuccess -> pure ()
    Left (ExitFailure n) -> failed ("exited with status " <> show n)
  pure (BL.fromStrict (TE.encodeUtf8 out))
  where
    failed why = throwIO (ExampleFailed (path <> " " <> why))

-- | An example that did not run to a clean end. The Show instance is
-- the bare message, because that is what the test report prints.
newtype ExampleFailed = ExampleFailed String

instance Show ExampleFailed where
  show (ExampleFailed why) = why

instance Exception ExampleFailed
