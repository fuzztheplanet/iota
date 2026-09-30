-- | Where everything is wired together. newInterpreter builds the
-- implementation environment out of the core syntax, the Haskell
-- primitives and the Scheme half of the stdlib, sets up the standard
-- libraries as views onto it, and makes an interaction environment with
-- all of them imported. runText is the error boundary. No Scheme error
-- gets past it.
{-# LANGUAGE TemplateHaskell #-}
module Iota.Interpreter
  ( -- * Building an interpreter
    newInterpreter
  , Interpreter (..)

    -- * Running code
  , runText
  , runTextForOutput
  , runFile
  , evalText

    -- * Test support
  , captureOutput
  ) where

import Control.Exception (SomeException, displayException, evaluate, finally, fromException, throwIO, try)
import Control.Monad (forM, forM_)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Iota.Counter (freshId)
import Iota.Eval (rewind)
import Iota.Expand
import Iota.Library
import Iota.Primitives (primitiveBindings)
import Iota.Primitives.IO (StdPorts (..), makeStandardPorts)
import Iota.Printer (formatError, writeText)
import Iota.Reader (readDatums, readDatumsLocated)
import Iota.Syntax
import Iota.Types
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import System.FilePath (splitSearchPath)

-- | The runtime and the environment the user types into.
data Interpreter = Interpreter
  { interpRuntime :: Runtime
  , interpEnv :: GlobalEnv
  }

-- | lib/iota/stdlib.iota, embedded at compile time, so rebuild after
-- editing it. It used to be loaded from a cabal data file path and that
-- only worked under cabal run.
stdlibSource :: Text
stdlibSource = TE.decodeUtf8 $(makeRelativeToProject "lib/iota/stdlib.iota" >>= embedFile)

newInterpreter :: IO Interpreter
newInterpreter = do
  ports <- makeStandardPorts
  impl <- newGlobalEnv "iota"
  dyn <- newIORef DynState {dynWinds = [], dynHandlers = []}
  libs <- newIORef Map.empty
  interaction <- newIORef Nothing
  loc <- newIORef Nothing
  libPath <- maybe [] splitSearchPath <$> lookupEnv "IOTA_LIBRARY_PATH"
  let rt =
        Runtime
          { rtDyn = dyn
          , rtImpl = impl
          , rtLibraries = libs
          , rtInteraction = interaction
          , rtStdin = stdinParam ports
          , rtStdout = stdoutParam ports
          , rtStderr = stderrParam ports
          , rtLibraryPath = libPath <> ["lib"]
          , rtLoc = loc
          }
  installSpecialForms impl
  forM_ (primitiveBindings rt) $ \(name, value) -> do
    cell <- globalCell impl name
    writeIORef (cellRef cell) value
  stdlib <- either (fail . T.unpack) pure (readDatums "stdlib.iota" stdlibSource)
  r <- try (runFormsK (exFor rt "lib/iota/stdlib.iota") impl stdlib pure)
  case r of
    Left (e :: SchemeError) -> do
      msg <- formatError e
      fail ("error loading stdlib: " <> T.unpack msg)
    Right _ -> pure ()
  -- The standard libraries are export lists over impl. Every name is
  -- checked here so a typo in Iota.Library fails at boot rather than
  -- when someone imports the library.
  forM_ (standardLibraryExports <> [(LibName ["iota", "extras"], iotaExtrasExports)]) $
    \(name, names) -> do
      exports <- forM names $ \n -> do
        let s = Symbol n
        globalLookup impl s >>= \case
          Just den -> pure (s, den)
          Nothing -> fail ("boot: " <> show name <> " exports undefined name " <> T.unpack n)
      registerLibrary rt Library {libName = name, libExports = Map.fromList exports}
  user <- newGlobalEnv "user"
  importInto (exFor rt ".") user (map nameDatum (map fst standardLibraryExports <> [LibName ["iota", "extras"]]))
  -- import and define-library are program declarations, not library
  -- exports, so the user environment gets them by hand.
  forM_ ["import", "define-library"] $ \s -> globalBind user s (DenSpecial s)
  writeIORef interaction (Just user)
  pure Interpreter {interpRuntime = rt, interpEnv = user}
  where
    nameDatum (LibName parts) = DList (map (DSymbol . Symbol) parts) Nothing

-- | Run a whole program and return the last value or the rendered
-- error. Every SchemeError and any stray IO exception stops here. The
-- one thing let through is ExitCode, which exit throws on purpose.
--
-- On an error the form's dynamic extent is abandoned. The dynamic-wind
-- frames it was inside are unwound, and that undoes any parameterize,
-- and the dynamic state goes back to what it was before the form. Errors
-- from a real file get the location of the top level form in front.
runText :: Interpreter -> FilePath -> Text -> IO (Either Text Value)
runText interp name src = case readDatumsLocated name src of
  Left perr -> Left <$> formatError (ParseFailure perr)
  Right ds -> do
    let rt = interpRuntime interp
    saved <- readIORef (rtDyn rt)
    r <- try (runLocatedK (exFor rt name) (interpEnv interp) ds pure)
    case r of
      Right v -> pure (Right v)
      Left (e :: SomeException) -> do
        loc <- readIORef (rtLoc rt)
        let located msg = case loc of
              Just l | not (isPseudoFile (locFile l)) -> showLoc l <> ": " <> msg
              _ -> msg
        if
          -- exit has already unwound and emergency-exit must not, so no
          -- rewinding here. Drop the state and rethrow.
          | Just (ec :: ExitCode) <- fromException e -> writeIORef (rtDyn rt) saved >> throwIO ec
          | Just (serr :: SchemeError) <- fromException e -> resetDyn rt saved >> Left . located <$> formatError serr
          | otherwise -> resetDyn rt saved >> pure (Left (located (T.pack (displayException e))))
  where
    isPseudoFile f = take 1 f == "<"

-- | Leave the wind frames an abandoned computation was in, running
-- their after thunks, then put the old dynamic state back. A failing
-- after thunk is ignored. The state is being thrown away anyway.
resetDyn :: Runtime -> DynState -> IO ()
resetDyn rt saved = do
  current <- readIORef (rtDyn rt)
  _ <- try @SomeException (rewind rt (dynWinds current) (dynWinds saved) (pure Unspecified))
  writeIORef (rtDyn rt) saved


-- | runText, then write the result. This is what the repl and the golden
-- tests print. The text is forced before it is returned so a lazy error
-- cannot get out past the boundary.
runTextForOutput :: Interpreter -> FilePath -> Text -> IO Text
runTextForOutput interp name src =
  runText interp name src >>= \case
    Left err -> pure err
    Right v -> do
      r <- try (writeText v >>= evaluate . forceText)
      case r of
        Right t -> pure t
        Left (e :: SomeException)
          | Just (serr :: SchemeError) <- fromException e -> formatError serr
          | otherwise -> pure (T.pack (displayException e))
  where
    forceText t = T.length t `seq` t

-- | iota run file.iota.
runFile :: Interpreter -> FilePath -> IO (Either Text Value)
runFile interp path = do
  r <- try (readSourceFile path)
  case r of
    Left (e :: SchemeError) -> Left <$> formatError e
    Right src -> runText interp path src

-- | A fresh interpreter and one program. For the tests.
evalText :: Text -> IO Text
evalText src = do
  interp <- newInterpreter
  runTextForOutput interp "<eval>" src

-- | Point the current output port at a buffer while an action runs.
-- Returns the result and whatever was displayed. The golden tests
-- compare that output as well.
captureOutput :: Interpreter -> IO a -> IO (a, Text)
captureOutput interp action = do
  buf <- newIORef []
  st <- newIORef (OutputString buf)
  i <- freshId
  let capture = PortVal Port {portId = i, portName = "capture", portTextual = True, portState = st}
  cell <- case rtStdout (interpRuntime interp) of
    Proc Parameter {paramCell = c} -> pure c
    _ -> fail "standard output is not a parameter"
  old <- readIORef cell
  writeIORef cell capture
  r <- action `finally` writeIORef cell old
  out <- T.concat . reverse <$> readIORef buf
  pure (r, out)
