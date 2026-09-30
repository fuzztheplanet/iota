-- | Every test/scheme/NAME.iota runs in its own interpreter. Whatever it
-- displayed, followed by the written value of its last form, is compared
-- with test/golden/NAME.golden. The layout comes from adamwespiser/scheme.
--
-- Running the tests with --accept rewrites the golden files. Read the
-- diff before committing them.
module Test.Golden (tests) where

import Data.List (sort)
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.ByteString.Lazy qualified as BL
import Iota.Interpreter (captureOutput, newInterpreter, runTextForOutput)
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, takeBaseName, takeExtension, (</>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)

schemeDir, goldenDir :: FilePath
schemeDir = "test/scheme"
goldenDir = "test/golden"

tests :: IO TestTree
tests = do
  files <- sort . filter ((== ".iota") . takeExtension) <$> listDirectory schemeDir
  pure $
    testGroup
      "Golden"
      [ goldenVsString
          (takeBaseName file)
          (goldenDir </> replaceExtension file ".golden")
          (runOne (schemeDir </> file))
      | file <- files
      ]

runOne :: FilePath -> IO BL.ByteString
runOne path = do
  interp <- newInterpreter
  src <- TIO.readFile path
  (value, displayed) <- captureOutput interp (runTextForOutput interp path src)
  pure (BL.fromStrict (TE.encodeUtf8 (displayed <> value <> "\n")))
