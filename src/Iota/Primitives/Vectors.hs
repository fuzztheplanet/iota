-- | Vectors and bytevectors. Mostly index checking.
module Iota.Primitives.Vectors
  ( vectorPrimitives
  ) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.ByteString qualified as BS
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as UMV
import Iota.Types
import Iota.Value

vectorPrimitives :: [(Symbol, Value)]
vectorPrimitives =
  [ prim "make-vector" (Between 1 2) $ \args -> case args of
      [nv] -> wantIndex nv >>= \n -> makeVector n Unspecified
      [nv, fill] -> wantIndex nv >>= \n -> makeVector n fill
      _ -> impossible
  , prim "vector" (AtLeast 0) vectorFromList
  , prim "vector-length" (Exactly 1) $ \case
      [v] -> integer . fromIntegral . MV.length <$> wantVector v
      _ -> impossible
  , prim "vector-ref" (Exactly 2) $ \case
      [vv, kv] -> do
        mv <- wantVector vv
        k <- wantIndex kv
        if k < MV.length mv
          then MV.read mv k
          else throwScheme (IndexOutOfRange "vector-ref" k)
      _ -> impossible
  , prim "vector-set!" (Exactly 3) $ \case
      [vv, kv, x] -> do
        requireMutable "vector-set!" vv
        mv <- wantVector vv
        k <- wantIndex kv
        if k < MV.length mv
          then MV.write mv k x >> pure Unspecified
          else throwScheme (IndexOutOfRange "vector-set!" k)
      _ -> impossible
  , prim "vector->list" (Between 1 3) $ \args -> do
      (mv, start, end) <- vecSlice "vector->list" args
      mapM (MV.read mv) [start .. end - 1] >>= list
  , prim "list->vector" (Exactly 1) $ \case
      [v] -> requireList "list->vector" v >>= vectorFromList
      _ -> impossible
  , prim "vector-fill!" (Between 2 4) $ \args -> case args of
      (vv : fill : rest) -> do
        requireMutable "vector-fill!" vv
        mv <- wantVector vv
        (start, end) <- sliceBounds "vector-fill!" (MV.length mv) rest
        mapM_ (\i -> MV.write mv i fill) [start .. end - 1]
        pure Unspecified
      _ -> impossible
  , prim "vector-copy" (Between 1 3) $ \args -> do
      (mv, start, end) <- vecSlice "vector-copy" args
      xs <- mapM (MV.read mv) [start .. end - 1]
      vectorFromList xs
  , prim "vector-copy!" (Between 3 5) $ \args -> case args of
      (tov : atv : fromv : rest) -> do
        requireMutable "vector-copy!" tov
        to <- wantVector tov
        at <- wantIndex atv
        from <- wantVector fromv
        (start, end) <- sliceBounds "vector-copy!" (MV.length from) rest
        if at + (end - start) > MV.length to
          then throwScheme (IndexOutOfRange "vector-copy!" at)
          else do
            -- copy through a list so overlapping ranges work
            xs <- mapM (MV.read from) [start .. end - 1]
            mapM_ (\(i, x) -> MV.write to i x) (zip [at ..] xs)
            pure Unspecified
      _ -> impossible
  , prim "vector-append" (AtLeast 0) $ \args -> do
      xss <- mapM vectorToList args
      vectorFromList (concat xss)
  , prim "vector->string" (Between 1 3) $ \args -> do
      (mv, start, end) <- vecSlice "vector->string" args
      xs <- mapM (MV.read mv) [start .. end - 1]
      cs <- mapM wantChar xs
      makeString (T.pack cs)
  , prim "string->vector" (Exactly 1) $ \case
      [sv] -> do
        t <- wantString sv
        vectorFromList (map Character (T.unpack t))
      _ -> impossible
  , -- bytevectors
    prim "make-bytevector" (Between 1 2) $ \args -> do
      (n, fill) <- case args of
        [nv] -> (,) <$> wantIndex nv <*> pure 0
        [nv, fv] -> (,) <$> wantIndex nv <*> wantByte fv
        _ -> impossible
      bytevectorFromList (replicate n fill)
  , prim "bytevector" (AtLeast 0) $ \args ->
      mapM wantByte args >>= bytevectorFromList
  , prim "bytevector-length" (Exactly 1) $ \case
      [Bytes _ bv] -> pure (integer (fromIntegral (UMV.length bv)))
      [v] -> throwScheme (TypeError "a bytevector" v)
      _ -> impossible
  , prim "bytevector-u8-ref" (Exactly 2) $ \case
      [Bytes _ bv, kv] -> do
        k <- wantIndex kv
        if k < UMV.length bv
          then integer . fromIntegral <$> UMV.read bv k
          else throwScheme (IndexOutOfRange "bytevector-u8-ref" k)
      [v, _] -> throwScheme (TypeError "a bytevector" v)
      _ -> impossible
  , prim "bytevector-u8-set!" (Exactly 3) $ \case
      [b@(Bytes _ bv), kv, xv] -> do
        requireMutable "bytevector-u8-set!" b
        k <- wantIndex kv
        x <- wantByte xv
        if k < UMV.length bv
          then UMV.write bv k x >> pure Unspecified
          else throwScheme (IndexOutOfRange "bytevector-u8-set!" k)
      [v, _, _] -> throwScheme (TypeError "a bytevector" v)
      _ -> impossible
  , prim "bytevector-copy" (Between 1 3) $ \args -> case args of
      (bvv : rest) -> do
        ws <- bytevectorToList bvv
        (start, end) <- sliceBounds "bytevector-copy" (length ws) rest
        bytevectorFromList (take (end - start) (drop start ws))
      _ -> impossible
  , prim "bytevector-append" (AtLeast 0) $ \args -> do
      wss <- mapM bytevectorToList args
      bytevectorFromList (concat wss)
  , prim "utf8->string" (Exactly 1) $ \case
      [bvv] -> do
        ws <- bytevectorToList bvv
        makeString (TE.decodeUtf8With TEE.lenientDecode (BS.pack ws))
      _ -> impossible
  , prim "string->utf8" (Exactly 1) $ \case
      [sv] -> do
        t <- wantString sv
        bytevectorFromList (BS.unpack (TE.encodeUtf8 t))
      _ -> impossible
  ]
  where
    prim name arity fn = (name, primitive (symbolName name) arity fn)

    impossible :: Eval a
    impossible = throwScheme (UserError "internal arity error" [])

    wantByte v = do
      i <- wantInteger v
      if i < 0 || i > 255
        then throwScheme (TypeError "a byte (0-255)" v)
        else pure (fromInteger i)

    sliceBounds who len rest = case rest of
      [] -> pure (0, len)
      [startv] -> do
        start <- wantIndex startv
        check who len start len
        pure (start, len)
      [startv, endv] -> do
        start <- wantIndex startv
        end <- wantIndex endv
        check who len start end
        pure (start, end)
      _ -> impossible

    vecSlice who args = case args of
      (vv : rest) -> do
        mv <- wantVector vv
        (start, end) <- sliceBounds who (MV.length mv) rest
        pure (mv, start, end)
      [] -> impossible

    check who len start end =
      if start < 0 || end > len || start > end
        then throwScheme (IndexOutOfRange who start)
        else pure ()
