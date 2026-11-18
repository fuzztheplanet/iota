-- | The tests that touch the file system or the process. File ports,
-- libraries found on disk, include paths, fold-case in a file and exit
-- codes.
module Test.Ports (tests) where

import Control.Exception (ErrorCall (..), throwIO, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Iota.Interpreter
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util (withSandbox)

-- | Write the source to the path, run it, and return what it displayed
-- and the error if there was one.
runProgram :: FilePath -> Text -> IO (Text, Maybe Text)
runProgram path src = do
  TIO.writeFile path src
  interp <- newInterpreter
  (r, out) <- captureOutput interp (runFile interp path)
  pure (out, either Just (const Nothing) r)

tests :: TestTree
tests =
  testGroup
    "files and processes"
    [ testCase "read on a file port is incremental" $ withSandbox "read" $ \dir -> do
        TIO.writeFile (dir </> "data.txt") "(1 2)\nrest of line\n#!fold-case FOO\n"
        interp <- newInterpreter
        out <-
          runTextForOutput interp "<t>" $
            "(let* ((p (open-input-file \"" <> T.pack (dir </> "data.txt") <> "\")) (a (read p)) (b (read-line p)) (c (read-line p)) (d (read p)) (e (read-char p)) (f (read-char p))) (list a b c d e (eof-object? f)))"
        -- read stops right after the datum. read-line then gets the rest
        -- of that line, which is empty, and the newline after the last
        -- datum is still there for read-char.
        out @?= "((1 2) \"\" \"rest of line\" foo #\\newline #t)"
    , testCase "a library is loaded from a .iota file next to the program" $ withSandbox "library" $ \dir -> do
        createDirectoryIfMissing True (dir </> "my")
        TIO.writeFile (dir </> "my" </> "util.iota") "(define-library (my util) (import (scheme base)) (export twice) (begin (define (twice x) (* 2 x))))"
        (out, _) <- runProgram (dir </> "prog.iota") "(import (scheme base) (scheme write) (my util)) (display (twice 21))"
        out @?= "42"
    , testCase "include is relative to the including file" $ withSandbox "include" $ \dir -> do
        createDirectoryIfMissing True (dir </> "sub")
        TIO.writeFile (dir </> "sub" </> "helper.iota") "(define from-helper 'included)"
        (out, _) <- runProgram (dir </> "sub" </> "main.iota") "(import (scheme base) (scheme write)) (include \"helper.iota\") (display from-helper)"
        out @?= "included"
    , testCase "errors in files carry the location of the form" $ withSandbox "location" $ \dir -> do
        TIO.writeFile (dir </> "bad.iota") "(import (scheme base))\n\n(car 1)\n"
        interp <- newInterpreter
        r <- runFile interp (dir </> "bad.iota")
        case r of
          Left msg -> assertBool ("message: " <> T.unpack msg) ((T.pack (dir </> "bad.iota") <> ":3:1: expected a pair, got: 1") == msg)
          Right _ -> assertFailure "expected an error"
    , -- A file is read to the end before any of it runs. Expansion is per
      -- form, after the forms in front of it have run.
      testCase "a read error anywhere in a file means none of it runs" $ withSandbox "read-error" $ \dir -> do
        (out, err) <- runProgram (dir </> "unbalanced.iota") "(display \"a\")\n(newline)\n(display \"b\"\n"
        out @?= ""
        case err of
          Just msg -> assertBool ("message: " <> T.unpack msg) ("read error:" `T.isPrefixOf` msg)
          Nothing -> assertFailure "expected a read error"
    , testCase "an expansion error is reported after the forms before it have run" $ withSandbox "expansion-error" $ \dir -> do
        let path = dir </> "badsyntax.iota"
        (out, err) <- runProgram path "(display \"a\")\n(if)\n(display \"c\")\n"
        out @?= "a"
        err @?= Just (T.pack path <> ":2:1: bad syntax in if: (if)")
    , testCase "exit unwinds dynamic-wind and reports its code" $ do
        interp <- newInterpreter
        (r, out) <- captureOutput interp $ try @ExitCode (runText interp "<t>" "(dynamic-wind (lambda () #f) (lambda () (exit 3)) (lambda () (display \"after\")))")
        case r of
          Left (ExitFailure 3) -> out @?= "after"
          other -> assertFailure ("unexpected: " <> show (fmap (fmap (const ())) other))
    , testCase "emergency-exit skips dynamic-wind" $ do
        interp <- newInterpreter
        (r, out) <- captureOutput interp $ try @ExitCode (runText interp "<t>" "(dynamic-wind (lambda () #f) (lambda () (emergency-exit 4)) (lambda () (display \"after\")))")
        case r of
          Left (ExitFailure 4) -> out @?= ""
          other -> assertFailure ("unexpected: " <> show (fmap (fmap (const ())) other))
    , -- The scratch directories these tests use. Several test processes may
      -- share one temporary directory, so a name is never used twice, and
      -- a test that fails must not leave its files behind.
      testCase "two sandboxes of one name are two directories" $
        withSandbox "same" $ \one ->
          withSandbox "same" $ \other -> do
            assertBool "the same directory twice" (one /= other)
            mapM_ (\dir -> doesDirectoryExist dir >>= assertBool (dir <> " does not exist")) [one, other]
    , testCase "a sandbox is removed when its test throws" $ do
        r <- try $ withSandbox "throws" $ \dir -> do
          TIO.writeFile (dir </> "file.txt") "content"
          _ <- throwIO (ErrorCall dir)
          pure ()
        case r of
          Left (ErrorCall dir) -> doesDirectoryExist dir >>= assertBool (dir <> " is still there") . not
          Right () -> assertFailure "the action did not throw"
    , testCase "an uncaught error resets the dynamic state" $ do
        interp <- newInterpreter
        _ <- runTextForOutput interp "<t>" "(define p (make-parameter 10)) (parameterize ((p 5)) (car 1))"
        out <- runTextForOutput interp "<t>" "(p)"
        out @?= "10"
        _ <- runTextForOutput interp "<t>" "(dynamic-wind (lambda () #f) (lambda () (car 1)) (lambda () (display \"after\")))"
        (_, out2) <- captureOutput interp (runTextForOutput interp "<t>" "(call/cc (lambda (k) (k 1)))")
        out2 @?= ""
    ]
