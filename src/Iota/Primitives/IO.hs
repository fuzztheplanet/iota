-- | Ports, input and output, and the system interface procedures
-- (command-line, environment variables, time).
--
-- The three standard ports are parameter objects, made once in
-- 'makeStandardPorts'. A procedure called without a port reads the
-- parameter, so parameterize on current-output-port redirects display
-- and (current-output-port) returns the same object every time.
--
-- Input handles carry a pushback buffer. read pulls lines into it until
-- a whole datum parses and leaves the rest for the next call. Without
-- that (read) at a terminal would block until end of file.
module Iota.Primitives.IO
  ( ioPrimitives
  , StdPorts (..)
  , makeStandardPorts
  ) where

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.IORef
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector.Unboxed.Mutable qualified as UMV
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8)
import GHC.Clock (getMonotonicTimeNSec)
import Iota.Counter (freshId)
import Iota.Printer (displayText, writeSharedText, writeSimpleText, writeText)
import Iota.Reader (ReadFailure (..), readDatumFromText)
import Iota.Types
import Iota.Value
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.IO (Handle, IOMode (ReadMode, WriteMode), hClose, hFlush, hGetChar, hGetLine, hIsEOF, hLookAhead, hReady, hSetEncoding, utf8)
import System.IO qualified as SIO

-- | stdin, stdout, stderr as parameter objects.
data StdPorts = StdPorts
  { stdinParam :: Value
  , stdoutParam :: Value
  , stderrParam :: Value
  }

makeStandardPorts :: IO StdPorts
makeStandardPorts = do
  i <- handlePort "stdin" True SIO.stdin True
  o <- handlePort "stdout" True SIO.stdout False
  e <- handlePort "stderr" True SIO.stderr False
  StdPorts <$> portParameter i <*> portParameter o <*> portParameter e

portParameter :: Value -> IO Value
portParameter initial = do
  i <- freshId
  cell <- newIORef initial
  pure (Proc Parameter {paramId = i, paramCell = cell, paramConverter = Nothing})

handlePort :: T.Text -> Bool -> Handle -> Bool -> IO Value
handlePort name textual h isInput = do
  st <-
    if isInput
      then InputHandle h <$> newIORef "" >>= newIORef
      else newIORef (OutputHandle h)
  i <- freshId
  pure (PortVal Port {portId = i, portName = name, portTextual = textual, portState = st})

-- | TAI minus UTC, 37 seconds since the 2017 leap second. If another
-- one is announced, IOTA_TAI_OFFSET overrides this until the constant
-- is updated.
taiMinusUtc :: Double
taiMinusUtc = 37

taiOffset :: IO Double
taiOffset =
  lookupEnv "IOTA_TAI_OFFSET" >>= \case
    Just s | [(d, "")] <- reads s -> pure d
    _ -> pure taiMinusUtc

-- | The port argument is optional and comes last. Without it the
-- current port parameter is used.
ioPrimitives :: Runtime -> [(Symbol, Value)]
ioPrimitives rt =
  [ ("current-input-port", rtStdin rt)
  , ("current-output-port", rtStdout rt)
  , ("current-error-port", rtStderr rt)
  , -- output
    prim "display" (Between 1 2) $ output displayText
  , prim "write" (Between 1 2) $ output writeText
  , prim "write-shared" (Between 1 2) $ output writeSharedText
  , prim "write-simple" (Between 1 2) $ output writeSimpleText
  , prim "write-char" (Between 1 2) $ output (fmap T.singleton . wantChar)
  , prim "newline" (Between 0 1) $ \args -> do
      p <- outPortFor args
      writeTextual p "\n"
      pure Unspecified
  , prim "write-string" (Between 1 4) $ \case
      (sv : rest) -> do
        t <- wantString sv
        (portArgs, sliced) <- case rest of
          [] -> pure ([], t)
          [pv] -> pure ([pv], t)
          [pv, startv] -> do
            start <- wantIndex startv
            checkRange "write-string" (T.length t) start (T.length t)
            pure ([pv], T.drop start t)
          [pv, startv, endv] -> do
            start <- wantIndex startv
            end <- wantIndex endv
            checkRange "write-string" (T.length t) start end
            pure ([pv], T.take (end - start) (T.drop start t))
          _ -> impossible
        p <- outPortFor portArgs
        writeTextual p sliced
        pure Unspecified
      _ -> impossible
  , prim "write-u8" (Between 1 2) $ \case
      (bv : rest) -> do
        b <- wantByte bv
        p <- outPortFor rest
        writeBinary p (BS.singleton b)
        pure Unspecified
      _ -> impossible
  , prim "write-bytevector" (Between 1 4) $ \case
      (bv : rest) -> do
        ws <- bytevectorToList bv
        (portArgs, sliced) <- case rest of
          [] -> pure ([], ws)
          [pv] -> pure ([pv], ws)
          [pv, startv] -> do
            start <- wantIndex startv
            checkRange "write-bytevector" (length ws) start (length ws)
            pure ([pv], drop start ws)
          [pv, startv, endv] -> do
            start <- wantIndex startv
            end <- wantIndex endv
            checkRange "write-bytevector" (length ws) start end
            pure ([pv], take (end - start) (drop start ws))
          _ -> impossible
        p <- outPortFor portArgs
        writeBinary p (BS.pack sliced)
        pure Unspecified
      _ -> impossible
  , prim "flush-output-port" (Between 0 1) $ \args -> do
      p <- outPortFor args
      readIORef (portState p) >>= \case
        OutputHandle h -> hFlush h
        _ -> pure ()
      pure Unspecified
  , -- textual input
    prim "read" (Between 0 1) $ \args -> inPortFor args >>= textualInput "read" >>= readDatumFrom
  , prim "read-char" (Between 0 1) $ \args -> inPortFor args >>= textualInput "read-char" >>= readChar True
  , prim "peek-char" (Between 0 1) $ \args -> inPortFor args >>= textualInput "peek-char" >>= readChar False
  , prim "read-line" (Between 0 1) $ \args -> inPortFor args >>= textualInput "read-line" >>= readLine
  , prim "read-string" (Between 1 2) $ \case
      (kv : rest) -> do
        k <- wantIndex kv
        p <- inPortFor rest >>= textualInput "read-string"
        t <- readChars p k
        if T.null t && k > 0 then pure EofObject else makeString t
      _ -> impossible
  , prim "char-ready?" (Between 0 1) $ \args -> do
      p <- inPortFor args >>= textualInput "char-ready?"
      Boolean <$> inputReady p
  , prim "eof-object" (Exactly 0) $ \_ -> pure EofObject
  , -- binary input
    prim "read-u8" (Between 0 1) $ \args -> inPortFor args >>= binaryInput "read-u8" >>= readByte True
  , prim "peek-u8" (Between 0 1) $ \args -> inPortFor args >>= binaryInput "peek-u8" >>= readByte False
  , prim "u8-ready?" (Between 0 1) $ \args -> do
      p <- inPortFor args >>= binaryInput "u8-ready?"
      Boolean <$> inputReady p
  , prim "read-bytevector" (Between 1 2) $ \case
      (kv : rest) -> do
        k <- wantIndex kv
        p <- inPortFor rest >>= binaryInput "read-bytevector"
        bytes <- readBytes p k
        if BS.null bytes && k > 0 then pure EofObject else bytevectorFromList (BS.unpack bytes)
      _ -> impossible
  , prim "read-bytevector!" (Between 1 4) $ \case
      (bv : rest) -> do
        requireMutable "read-bytevector!" bv
        target <- case bv of
          Bytes _ mv -> pure mv
          v -> throwScheme (TypeError "a bytevector" v)
        let len = UMV.length target
        (portArgs, start, end) <- case rest of
          [] -> pure ([], 0, len)
          [pv] -> pure ([pv], 0, len)
          [pv, sv] -> wantIndex sv >>= \s -> pure ([pv], s, len)
          [pv, sv, ev] -> (\s e -> ([pv], s, e)) <$> wantIndex sv <*> wantIndex ev
          _ -> impossible
        checkRange "read-bytevector!" len start end
        p <- inPortFor portArgs >>= binaryInput "read-bytevector!"
        bytes <- readBytes p (end - start)
        if BS.null bytes && end > start
          then pure EofObject
          else do
            mapM_ (\(i, b) -> UMV.write target (start + i) b) (zip [0 ..] (BS.unpack bytes))
            pure (integer (fromIntegral (BS.length bytes)))
      _ -> impossible
  , -- in-memory ports
    prim "open-input-string" (Exactly 1) $ \case
      [sv] -> do
        t <- wantString sv
        ref <- newIORef t
        newPort "string" True (InputString ref)
      _ -> impossible
  , prim "open-output-string" (Exactly 0) $ \_ -> do
      ref <- newIORef []
      newPort "string" True (OutputString ref)
  , prim "get-output-string" (Exactly 1) $ \case
      [pv] -> do
        p <- wantPort pv
        readIORef (portState p) >>= \case
          OutputString ref -> readIORef ref >>= makeString . T.concat . reverse
          _ -> throwScheme (PortError "get-output-string: not a string output port")
      _ -> impossible
  , prim "open-input-bytevector" (Exactly 1) $ \case
      [bv] -> do
        ws <- bytevectorToList bv
        ref <- newIORef (BS.pack ws)
        newPort "bytevector" False (InputBytes ref)
      _ -> impossible
  , prim "open-output-bytevector" (Exactly 0) $ \_ -> do
      ref <- newIORef []
      newPort "bytevector" False (OutputBytes ref)
  , prim "get-output-bytevector" (Exactly 1) $ \case
      [pv] -> do
        p <- wantPort pv
        readIORef (portState p) >>= \case
          OutputBytes ref -> readIORef ref >>= bytevectorFromList . BS.unpack . BS.concat . reverse
          _ -> throwScheme (PortError "get-output-bytevector: not a bytevector output port")
      _ -> impossible
  , -- files
    prim "open-input-file" (Exactly 1) $ openFilePort ReadMode True
  , prim "open-output-file" (Exactly 1) $ openFilePort WriteMode True
  , prim "open-binary-input-file" (Exactly 1) $ openFilePort ReadMode False
  , prim "open-binary-output-file" (Exactly 1) $ openFilePort WriteMode False
  , prim "close-port" (Exactly 1) closePort
  , prim "close-input-port" (Exactly 1) closePort
  , prim "close-output-port" (Exactly 1) closePort
  , prim "input-port?" (Exactly 1) $ portPred isInputState
  , prim "output-port?" (Exactly 1) $ portPred isOutputState
  , prim "input-port-open?" (Exactly 1) $ portPred isInputState
  , prim "output-port-open?" (Exactly 1) $ portPred isOutputState
  , prim "textual-port?" (Exactly 1) $ \case
      [PortVal p] -> pure (Boolean (portTextual p))
      [_] -> pure (Boolean False)
      _ -> impossible
  , prim "binary-port?" (Exactly 1) $ \case
      [PortVal p] -> pure (Boolean (not (portTextual p)))
      [_] -> pure (Boolean False)
      _ -> impossible
  , prim "file-exists?" (Exactly 1) $ \case
      [sv] -> do
        path <- wantString sv
        Boolean <$> doesFileExist (T.unpack path)
      _ -> impossible
  , prim "delete-file" (Exactly 1) $ \case
      [sv] -> do
        path <- wantString sv
        r <- try @IOException (removeFile (T.unpack path))
        case r of
          Right () -> pure Unspecified
          Left e -> throwScheme (FileError (T.pack (show e)))
      _ -> impossible
  , -- system interface. Iota.CLI rewrites argv before we get here so
    -- it reads (script arg ...), or ("iota") in the repl.
    prim "command-line" (Exactly 0) $ \_ -> do
      argv <- getArgs
      strs <- mapM (makeString . T.pack) argv
      list strs
  , prim "get-environment-variable" (Exactly 1) $ \case
      [sv] -> do
        var <- wantString sv
        lookupEnv (T.unpack var) >>= \case
          Just v -> makeString (T.pack v)
          Nothing -> pure (Boolean False)
      _ -> impossible
  , prim "get-environment-variables" (Exactly 0) $ \_ -> do
      env <- getEnvironment
      pairs <- mapM (\(k, v) -> do
                        ks <- makeString (T.pack k)
                        vs <- makeString (T.pack v)
                        cons ks vs) env
      list pairs
  , -- (scheme time). current-second is on the TAI scale, so POSIX time
    -- plus the offset.
    prim "current-second" (Exactly 0) $ \_ -> do
      t <- getPOSIXTime
      offset <- taiOffset
      pure (Num (Inexact (realToFrac t + offset)))
  , prim "current-jiffy" (Exactly 0) $ \_ -> do
      ns <- getMonotonicTimeNSec
      pure (integer (fromIntegral ns))
  , prim "jiffies-per-second" (Exactly 0) $ \_ -> pure (integer 1_000_000_000)
  ]
  where
    prim name arity fn = (name, primitive (symbolName name) arity fn)

    impossible :: Eval a
    impossible = throwScheme (UserError "internal arity error" [])

    newPort name textual st = do
      ref <- newIORef st
      i <- freshId
      pure (PortVal Port {portId = i, portName = name, portTextual = textual, portState = ref})

    outPortFor args = case args of
      [] -> currentPort (rtStdout rt)
      [pv] -> wantPort pv
      _ -> impossible

    inPortFor args = case args of
      [] -> currentPort (rtStdin rt)
      [pv] -> wantPort pv
      _ -> impossible

    currentPort param = case param of
      Proc Parameter {paramCell = cell} -> readIORef cell >>= wantPort
      _ -> throwScheme (PortError "no current port")

    textualInput who p = do
      st <- readIORef (portState p)
      if not (portTextual p)
        then throwScheme (PortError (who <> ": not a textual port"))
        else
          if isInputState st
            then pure p
            else throwScheme (PortError (who <> ": not an open input port"))

    binaryInput who p = do
      st <- readIORef (portState p)
      if portTextual p
        then throwScheme (PortError (who <> ": not a binary port"))
        else
          if isInputState st
            then pure p
            else throwScheme (PortError (who <> ": not an open input port"))

    output render = \case
      [v] -> do
        p <- currentPort (rtStdout rt)
        render v >>= writeTextual p
        pure Unspecified
      [v, pv] -> do
        p <- wantPort pv
        render v >>= writeTextual p
        pure Unspecified
      _ -> impossible

    writeTextual p t
      | not (portTextual p) = throwScheme (PortError "not a textual port")
      | otherwise =
          readIORef (portState p) >>= \case
            OutputHandle h -> TIO.hPutStr h t
            OutputString ref -> modifyIORef' ref (t :)
            ClosedPort -> throwScheme (PortError "write to a closed port")
            _ -> throwScheme (PortError "not an output port")

    writeBinary p bytes
      | portTextual p = throwScheme (PortError "not a binary port")
      | otherwise =
          readIORef (portState p) >>= \case
            OutputHandle h -> BS.hPut h bytes
            OutputBytes ref -> modifyIORef' ref (bytes :)
            ClosedPort -> throwScheme (PortError "write to a closed port")
            _ -> throwScheme (PortError "not an output port")

    openFilePort mode textual = \case
      [sv] -> do
        path <- wantString sv
        r <- try @IOException $ do
          h <- (if textual then SIO.openFile else SIO.openBinaryFile) (T.unpack path) mode
          if textual then hSetEncoding h utf8 else pure ()
          pure h
        case r of
          Right h -> handlePort path textual h (mode == ReadMode)
          Left e -> throwScheme (FileError (T.pack (show e)))
      _ -> impossible

    closePort = \case
      [pv] -> do
        p <- wantPort pv
        readIORef (portState p) >>= \case
          InputHandle h _ -> hClose h
          OutputHandle h -> hClose h
          _ -> pure ()
        writeIORef (portState p) ClosedPort
        pure Unspecified
      _ -> impossible

    isInputState = \case
      InputHandle _ _ -> True
      InputString _ -> True
      InputBytes _ -> True
      _ -> False

    isOutputState = \case
      OutputHandle _ -> True
      OutputString _ -> True
      OutputBytes _ -> True
      _ -> False

    portPred p = \case
      [PortVal port] -> Boolean . p <$> readIORef (portState port)
      [_] -> pure (Boolean False)
      _ -> impossible

    checkRange who len start end =
      if start < 0 || end > len || start > end
        then throwScheme (IndexOutOfRange who (if start > len then start else end))
        else pure ()

    wantByte v = do
      i <- wantInteger v
      if i < 0 || i > 255
        then throwScheme (TypeError "a byte (0-255)" v)
        else pure (fromInteger i :: Word8)

    -- Textual input

    -- Nothing at eof
    nextChar consume p =
      readIORef (portState p) >>= \case
        InputHandle h buf -> do
          t <- readIORef buf
          case T.uncons t of
            Just (c, rest) -> do
              if consume then writeIORef buf rest else pure ()
              pure (Just c)
            Nothing -> do
              eof <- hIsEOF h
              if eof
                then pure Nothing
                else Just <$> (if consume then hGetChar h else hLookAhead h)
        InputString ref -> do
          t <- readIORef ref
          case T.uncons t of
            Nothing -> pure Nothing
            Just (c, rest) -> do
              if consume then writeIORef ref rest else pure ()
              pure (Just c)
        _ -> throwScheme (PortError "not an open input port")

    readChar consume p = maybe EofObject Character <$> nextChar consume p

    readChars p k = go k []
      where
        go 0 acc = pure (T.pack (reverse acc))
        go n acc =
          nextChar True p >>= \case
            Nothing -> pure (T.pack (reverse acc))
            Just c -> go (n - 1 :: Int) (c : acc)

    readLine p =
      readIORef (portState p) >>= \case
        InputHandle h buf -> do
          t <- readIORef buf
          case T.breakOn "\n" t of
            (line, rest)
              | not (T.null rest) -> do
                  writeIORef buf (T.drop 1 rest)
                  makeString line
              | otherwise -> do
                  eof <- hIsEOF h
                  if eof
                    then
                      if T.null t
                        then pure EofObject
                        else writeIORef buf "" >> makeString t
                    else do
                      more <- TIO.hGetLine h
                      writeIORef buf ""
                      makeString (t <> more)
        InputString ref -> do
          t <- readIORef ref
          if T.null t
            then pure EofObject
            else do
              let (line, rest) = T.breakOn "\n" t
              writeIORef ref (T.drop 1 rest)
              makeString line
        _ -> throwScheme (PortError "not an open input port")

    inputReady p =
      readIORef (portState p) >>= \case
        InputHandle h buf -> do
          t <- readIORef buf
          if not (T.null t)
            then pure True
            else do
              eof <- hIsEOF h
              if eof then pure True else hReady h
        InputString _ -> pure True
        InputBytes _ -> pure True
        _ -> pure False

    -- One datum off the port. On a handle, keep appending lines to the
    -- buffer until the parser stops saying the input ran out. A malformed
    -- datum drops the buffer and reports a read error.
    readDatumFrom p =
      readIORef (portState p) >>= \case
        InputString ref -> do
          t <- readIORef ref
          case readDatumFromText t of
            Right (Just (d, rest)) -> writeIORef ref rest >> datumToValue d
            Right Nothing -> writeIORef ref "" >> pure EofObject
            Left Incomplete -> writeIORef ref "" >> throwScheme (ParseFailure "read: unexpected end of input inside a datum")
            Left (Malformed err) -> writeIORef ref "" >> throwScheme (ParseFailure err)
        InputHandle h buf -> do
          let loop = do
                t <- readIORef buf
                case readDatumFromText t of
                  Right (Just (d, rest)) -> writeIORef buf rest >> datumToValue d
                  Left (Malformed err) -> writeIORef buf "" >> throwScheme (ParseFailure err)
                  other -> do
                    eof <- hIsEOF h
                    if eof
                      then case other of
                        Right Nothing -> writeIORef buf "" >> pure EofObject
                        _ -> writeIORef buf "" >> throwScheme (ParseFailure "read: unexpected end of input inside a datum")
                      else do
                        line <- hGetLine h
                        writeIORef buf (t <> T.pack line <> "\n")
                        loop
          loop
        _ -> throwScheme (PortError "not an open input port")

    -- Binary input

    readByte consume p =
      readIORef (portState p) >>= \case
        InputBytes ref -> do
          bs <- readIORef ref
          case BS.uncons bs of
            Nothing -> pure EofObject
            Just (b, rest) -> do
              if consume then writeIORef ref rest else pure ()
              pure (integer (fromIntegral b))
        InputHandle h _ -> do
          eof <- hIsEOF h
          if eof
            then pure EofObject
            else
              if consume
                then do
                  bs <- BS.hGet h 1
                  case BS.uncons bs of
                    Just (b, _) -> pure (integer (fromIntegral b))
                    Nothing -> pure EofObject
                else do
                  -- binary handles are char8, so a char is a byte
                  c <- hLookAhead h
                  pure (integer (fromIntegral (ord c)))
        _ -> throwScheme (PortError "not an open input port")

    readBytes p k =
      readIORef (portState p) >>= \case
        InputBytes ref -> do
          bs <- readIORef ref
          let (taken, rest) = BS.splitAt k bs
          writeIORef ref rest
          pure taken
        InputHandle h _ -> do
          eof <- hIsEOF h
          if eof then pure BS.empty else BS.hGet h k
        _ -> throwScheme (PortError "not an open input port")
