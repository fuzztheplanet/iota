-- | Helpers the evaluator tests share.
module Test.Util
  ( evalsTo
  , evalCases
  , discardErrorOutput
  , withCommandLine
  , withSandbox
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, finally, onException, try)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Iota.Counter (freshId)
import Iota.Interpreter (Interpreter (..), evalText)
import Iota.Types
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (withArgs)
import System.IO (hClose, openTempFile)
import System.IO.Unsafe (unsafePerformIO)
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

-- | Point the current error port at a buffer nobody reads while an
-- action runs. 'Iota.Interpreter.captureOutput' does the same for the
-- output port and keeps the text. Programs that report on the error
-- port, the examples for one, would otherwise write into the test log.
discardErrorOutput :: Interpreter -> IO a -> IO a
discardErrorOutput interp action = do
  buf <- newIORef []
  st <- newIORef (OutputString buf)
  i <- freshId
  let sink = PortVal Port {portId = i, portName = "discard", portTextual = True, portState = st}
  cell <- case rtStderr (interpRuntime interp) of
    Proc Parameter {paramCell = c} -> pure c
    _ -> fail "standard error is not a parameter"
  old <- readIORef cell
  writeIORef cell sink
  action `finally` writeIORef cell old

-- | Run an action with the given process arguments, which is what
-- (command-line) returns. The arguments belong to the whole process and
-- the tests run in parallel, so everything that replaces them goes
-- through here and takes its turn.
withCommandLine :: [String] -> IO a -> IO a
withCommandLine args action = withMVar argumentsLock (\_ -> withArgs args action)

argumentsLock :: MVar ()
argumentsLock = unsafePerformIO (newMVar ())
{-# NOINLINE argumentsLock #-}

-- | A scratch directory for one test, removed afterwards whether the
-- test passed, failed or threw. The name is only there to make a
-- leftover recognisable.
--
-- Tests run in parallel, and so may whole test processes that share a
-- temporary directory, two builds on one machine for instance. A fixed
-- name would have them delete each other's files. 'openTempFile' hands
-- out a file name nobody else has, and the directory is that name with
-- @.d@ on the end.
withSandbox :: String -> (FilePath -> IO a) -> IO a
withSandbox name action = bracket reserve release (action . snd)
  where
    reserve = do
      tmp <- getTemporaryDirectory
      (reserved, h) <- openTempFile tmp ("iota-test-" <> name <> "-")
      hClose h
      let dir = reserved <> ".d"
      createDirectory dir `onException` removeFile reserved
      pure (reserved, dir)
    release (reserved, dir) =
      ignoringIOErrors (removeDirectoryRecursive dir) `finally` ignoringIOErrors (removeFile reserved)
    ignoringIOErrors io = try @IOError io >>= \_ -> pure ()
