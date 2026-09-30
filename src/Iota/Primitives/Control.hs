-- | The control primitives. apply, call/cc, dynamic-wind, the exception
-- procedures, parameters, promises, records, eval, load, exit.
--
-- Most of these are a 'PrimitiveK' because they call back into Scheme or
-- change the dynamic state. A PrimitiveK must not throw for a Scheme
-- level error. Argument checks go through 'guardIO' so a failure reaches
-- the current handler with the continuation of the call.
--
-- map and its relatives are here in CPS rather than in the stdlib. The
-- accumulator is an immutable Haskell list, so re-entering a continuation
-- captured inside the mapped procedure builds a new result instead of
-- overwriting the one already returned.
module Iota.Primitives.Control
  ( controlPrimitives
  ) where

import Control.Monad (forM_)
import Data.IORef
import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV
import Iota.Counter (freshId)
import Iota.Eval
import Iota.Expand
import Iota.Syntax (newGlobalEnv)
import Iota.Types
import Iota.Value
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, stderr, stdout)

controlPrimitives :: Runtime -> [(Symbol, Value)]
controlPrimitives rt =
  [ primK "apply" (AtLeast 1) $ \args k -> case args of
      [f] -> applyK rt f [] k
      (f : rest) ->
        -- last argument is spread
        guardIO rt k (requireList "apply" (last rest)) $ \spread ->
          applyK rt f (init rest <> spread) k
      [] -> k Unspecified
  , primK1 "call-with-current-continuation" $ \f k -> callCC rt f k
  , primK1 "call/cc" $ \f k -> callCC rt f k
  , prim "values" (AtLeast 0) $ pure . listToValues
  , primK2 "call-with-values" $ \producer consumer k ->
      applyK rt producer [] $ \result -> applyK rt consumer (valuesToList result) k
  , primK3 "dynamic-wind" $ \before thunk after k ->
      guardIO rt k (mapM_ wantProcedure [before, thunk, after]) $ \_ ->
        dynamicWind rt before thunk after k
  , -- the mapping procedures, all stop at the shortest argument
    primKRest "map" (AtLeast 2) $ \f lists k ->
      guardIO rt k (mapM (requireList "map") lists) $ \xss ->
        mapK f xss [] $ \results -> list results >>= k
  , primKRest "for-each" (AtLeast 2) $ \f lists k ->
      guardIO rt k (mapM (requireList "for-each") lists) $ \xss ->
        forEachK f xss k
  , primKRest "vector-map" (AtLeast 2) $ \f vecs k ->
      guardIO rt k (mapM vectorToList vecs) $ \xss ->
        mapK f xss [] $ \results -> vectorFromList results >>= k
  , primKRest "vector-for-each" (AtLeast 2) $ \f vecs k ->
      guardIO rt k (mapM vectorToList vecs) $ \xss -> forEachK f xss k
  , primKRest "string-map" (AtLeast 2) $ \f strs k ->
      guardIO rt k (mapM wantString strs) $ \ts ->
        mapK f (map (map Character . T.unpack) ts) [] $ \results ->
          guardIO rt k (mapM wantChar results) $ \cs -> makeString (T.pack cs) >>= k
  , primKRest "string-for-each" (AtLeast 2) $ \f strs k ->
      guardIO rt k (mapM wantString strs) $ \ts ->
        forEachK f (map (map Character . T.unpack) ts) k
  , -- exceptions
    primK2 "with-exception-handler" $ \handler thunk k ->
      guardIO rt k (mapM_ wantProcedure [handler, thunk]) $ \_ ->
        withExceptionHandler rt handler thunk k
  , primK1 "raise" $ \obj k -> raiseValue rt False obj k
  , primK1 "raise-continuable" $ \obj k -> raiseValue rt True obj k
  , primKRest "error" (AtLeast 1) $ \msgv irritants k ->
      guardIO rt k (wantString msgv) $ \msg ->
        raiseValue rt False (ErrObj ErrorGeneral msg irritants) k
  , prim1 "error-object?" $ \v -> pure (Boolean (isErrObj v))
  , prim1 "error-object-message" $ \case
      ErrObj _ m _ -> makeString m
      v -> throwScheme (TypeError "an error object" v)
  , prim1 "error-object-irritants" $ \case
      ErrObj _ _ irr -> list irr
      v -> throwScheme (TypeError "an error object" v)
  , prim1 "file-error?" $ \v -> pure (Boolean (isKind ErrorFile v))
  , prim1 "read-error?" $ \v -> pure (Boolean (isKind ErrorRead v))
  , -- guard. The expander rewrites (guard (var clause ...) body ...) as
    -- (%guard (lambda () body ...) (lambda (var reraise) clauses)).
    --
    -- The handler first escapes to the dynamic environment of the guard
    -- and only then evaluates the clauses. If none matches, reraise goes
    -- back into the dynamic environment of the original raise and raises
    -- there again with raise-continuable. Both transitions run the wind
    -- thunks. This is the part of R7RS 4.2.7 that is easy to skip.
    primK2 "%guard" $ \thunk handler k -> do
      d0 <- readDyn rt
      let onRaise = primitiveK "guard-handler" (Exactly 1) $ \args kh -> do
            let condition = head args
            d1 <- readDyn rt
            rewind rt (dynWinds d1) (dynWinds d0) $ do
              writeIORef (rtDyn rt) d0
              let reraise = primitiveK "reraise" (Exactly 0) $ \_ _ -> do
                    d0' <- readDyn rt
                    rewind rt (dynWinds d0') (dynWinds d1) $ do
                      writeIORef (rtDyn rt) d1
                      raiseValue rt True condition kh
              applyK rt handler [condition, reraise] k
      withHandlers rt (onRaise : dynHandlers d0) (applyK rt thunk []) k
  , -- parameters
    prim2 "%make-parameter" $ \initial converter -> do
      i <- freshId
      cell <- newIORef initial
      let conv = case converter of
            Boolean False -> Nothing
            c -> Just c
      pure (Proc Parameter {paramId = i, paramCell = cell, paramConverter = conv})
  , -- (%parameterize (list p ...) (list v ...) thunk). Convert the values,
    -- then dynamic-wind a swap in and a swap out. Swapping instead of
    -- saving and restoring means re-entry through a continuation sees the
    -- right values with no extra bookkeeping.
    primK3 "%parameterize" $ \paramsV valsV thunk k ->
      guardIO rt k (parameterPairs paramsV valsV) $ \pairs -> do
        let convertAll [] acc kont = kont (reverse acc)
            convertAll ((p, v) : rest) acc kont = case paramConverter p of
              Nothing -> convertAll rest ((p, v) : acc) kont
              Just conv -> applyK rt conv [v] $ \v' -> convertAll rest ((p, v') : acc) kont
        convertAll pairs [] $ \converted -> do
          slots <- mapM (\(p, v) -> (,) p <$> newIORef v) converted
          let swap = forM_ slots $ \(p, slot) -> do
                cur <- readIORef (paramCell p)
                new <- readIORef slot
                writeIORef (paramCell p) new
                writeIORef slot cur
              swapper name = primitive name (Exactly 0) $ \_ -> swap >> pure Unspecified
          dynamicWind rt (swapper "parameterize-enter") thunk (swapper "parameterize-exit") k
  , -- promises. A promise points at a box. Forcing a delay-force chain
    -- makes the promises share one box. force itself is in the stdlib.
    prim1 "%delay-force" $ \thunk -> newPromise False thunk
  , prim1 "make-promise" $ \case
      p@(PromiseVal _) -> pure p
      v -> newPromise True v
  , prim1 "promise?" $ \case
      PromiseVal _ -> pure (Boolean True)
      _ -> pure (Boolean False)
  , prim1 "%promise-done?" $ \p -> Boolean . promiseDone <$> promiseBox p
  , prim1 "%promise-value" $ \p -> promiseValue <$> promiseBox p
  , prim2 "%promise-update!" $ \new old -> case (new, old) of
      (PromiseVal newRef, PromiseVal oldRef) -> do
        newBox <- readIORef newRef
        oldBox <- readIORef oldRef
        readIORef newBox >>= writeIORef oldBox
        writeIORef newRef oldBox
        pure Unspecified
      (PromiseVal _, v) -> throwScheme (TypeError "a promise" v)
      (v, _) -> throwScheme (TypeError "a promise" v)
  , -- case-lambda, the first clause whose arity fits wins
    primK "%case-lambda" (AtLeast 0) $ \clauses k ->
      guardIO rt k (mapM wantProcedure clauses) $ \procs ->
        k . primitiveK "case-lambda" (AtLeast 0) $ \args k' ->
          case filter (accepts (length args)) procs of
            (p : _) -> applyK rt (Proc p) args k'
            [] -> raiseError rt (UserError "case-lambda: no clause matches the arguments" args) k'
  , -- records. define-record-type expands into calls to these.
    prim2 "%make-record-type" $ \nameV fieldsV -> do
      name <- wantSymbol nameV
      fields <- requireList "define-record-type" fieldsV >>= mapM wantSymbol
      i <- freshId
      pure (RecordTypeVal RecordType {rtdId = i, rtdName = name, rtdFields = fields})
  , prim3 "%record-constructor" $ \rtdV fieldsV nameV -> do
      rtd <- wantRtd rtdV
      fields <- requireList "define-record-type" fieldsV >>= mapM wantSymbol
      name <- wantSymbol nameV
      indices <- mapM (fieldIndex rtd) fields
      pure . primitive (symbolName name) (Exactly (length fields)) $ \vals -> do
        slots <- MV.replicate (length (rtdFields rtd)) Unspecified
        forM_ (zip indices vals) $ \(ix, v) -> MV.write slots ix v
        i <- freshId
        pure (RecordVal Record {recordId = i, recordType = rtd, recordFields = slots})
  , prim2 "%record-predicate" $ \rtdV nameV -> do
      rtd <- wantRtd rtdV
      name <- wantSymbol nameV
      pure . primitive (symbolName name) (Exactly 1) $ \case
        [RecordVal r] -> pure (Boolean (rtdId (recordType r) == rtdId rtd))
        _ -> pure (Boolean False)
  , prim3 "%record-accessor" $ \rtdV fieldV nameV -> do
      rtd <- wantRtd rtdV
      ix <- wantSymbol fieldV >>= fieldIndex rtd
      name <- wantSymbol nameV
      pure . primitive (symbolName name) (Exactly 1) $ \case
        [RecordVal r] | rtdId (recordType r) == rtdId rtd -> MV.read (recordFields r) ix
        [v] -> throwScheme (TypeError ("a " <> symbolName (rtdName rtd)) v)
        _ -> impossible
  , prim3 "%record-modifier" $ \rtdV fieldV nameV -> do
      rtd <- wantRtd rtdV
      ix <- wantSymbol fieldV >>= fieldIndex rtd
      name <- wantSymbol nameV
      pure . primitive (symbolName name) (Exactly 2) $ \case
        [RecordVal r, v] | rtdId (recordType r) == rtdId rtd -> MV.write (recordFields r) ix v >> pure Unspecified
        [v, _] -> throwScheme (TypeError ("a " <> symbolName (rtdName rtd)) v)
        _ -> impossible
  , -- eval, environments, load
    primK "eval" (Between 1 2) $ \args k -> do
      let (exprV, envV) = case args of
            [e, ev] -> (e, Just ev)
            [e] -> (e, Nothing)
            _ -> (Unspecified, Nothing)
      guardIO rt k (targetEnv envV) $ \genv ->
        guardIO rt k (valueToDatum exprV >>= expandTop (exFor rt ".") genv) $ \core ->
          compile rt [] core NoFrame k
  , prim "environment" (AtLeast 0) $ \specs -> do
      ds <- mapM valueToDatum specs
      EnvVal <$> environmentFrom (exFor rt ".") ds
  , prim "interaction-environment" (Exactly 0) $ \_ -> EnvVal <$> interactionEnv
  , prim "scheme-report-environment" (Between 0 1) $ \_ -> EnvVal <$> r5rsEnv
  , prim "null-environment" (Between 0 1) $ \_ -> EnvVal <$> r5rsEnv
  , primK "load" (Between 1 2) $ \args k -> do
      let (pathV, envV) = case args of
            [p, ev] -> (p, Just ev)
            [p] -> (p, Nothing)
            _ -> (Unspecified, Nothing)
      guardIO rt k ((,) <$> wantString pathV <*> targetEnv envV) $ \(path, genv) ->
        guardIO rt k (readSourceDatums (T.unpack path)) $ \ds ->
          runFormsK (exFor rt (T.unpack path)) genv ds k
  , prim "features" (Exactly 0) $ \_ -> list (map Sym featureSymbols)
  , prim "%undefined" (Exactly 0) $ \_ -> pure Undefined
  , -- exit runs the pending dynamic-wind after thunks first.
    -- emergency-exit does not.
    primK "exit" (Between 0 1) $ \args k ->
      guardIO rt k (exitCodeOf args) $ \code -> unwindAll rt (flushAll >> exitWith code)
  , prim "emergency-exit" (Between 0 1) $ \args -> do
      code <- exitCodeOf args
      flushAll
      exitWith code
  ]
  where
    prim name arity fn = (name, primitive (symbolName name) arity fn)
    primK name arity fn = (name, primitiveK (symbolName name) arity fn)

    -- applyK has checked the count before we get here. The fall-through
    -- cases never fire.
    prim1 name fn = prim name (Exactly 1) $ \case
      [a] -> fn a
      _ -> impossible
    prim2 name fn = prim name (Exactly 2) $ \case
      [a, b] -> fn a b
      _ -> impossible
    prim3 name fn = prim name (Exactly 3) $ \case
      [a, b, c] -> fn a b c
      _ -> impossible
    primK1 name fn = primK name (Exactly 1) $ \args k -> case args of
      [a] -> fn a k
      _ -> internal k
    primK2 name fn = primK name (Exactly 2) $ \args k -> case args of
      [a, b] -> fn a b k
      _ -> internal k
    primK3 name fn = primK name (Exactly 3) $ \args k -> case args of
      [a, b, c] -> fn a b c k
      _ -> internal k
    primKRest name arity fn = primK name arity $ \args k -> case args of
      (a : rest) -> fn a rest k
      [] -> internal k

    impossible :: Eval a
    impossible = throwScheme (UserError "internal arity error" [])
    internal k = raiseError rt (UserError "internal arity error" []) k

    mapK f xss acc kont = case rows xss of
      Nothing -> kont (reverse acc)
      Just (row, rest) -> applyK rt f row $ \v -> mapK f rest (v : acc) kont
    forEachK f xss k = case rows xss of
      Nothing -> k Unspecified
      Just (row, rest) -> applyK rt f row $ \_ -> forEachK f rest k
    rows xss
      | any null xss = Nothing
      | otherwise = Just (map head xss, map tail xss)

    isErrObj = \case
      ErrObj {} -> True
      _ -> False
    isKind kind = \case
      ErrObj k _ _ -> k == kind
      _ -> False

    parameterPairs paramsV valsV = do
      ps <- requireList "parameterize" paramsV
      vs <- requireList "parameterize" valsV
      params <- mapM asParameter ps
      pure (zip params vs)
    asParameter = \case
      Proc p@Parameter {} -> pure p
      v -> throwScheme (TypeError "a parameter object" v)

    newPromise done v = do
      box <- newIORef PromiseBox {promiseDone = done, promiseValue = v}
      PromiseVal <$> newIORef box
    promiseBox = \case
      PromiseVal ref -> readIORef ref >>= readIORef
      v -> throwScheme (TypeError "a promise" v)

    accepts n = \case
      Closure {..} -> if closureHasRest then n >= closureNParams else n == closureNParams
      Primitive {..} -> arityOk primArity n
      PrimitiveK {..} -> arityOk primArity n
      _ -> False
    arityOk arity n = case arity of
      Exactly x -> n == x
      AtLeast x -> n >= x
      Between lo hi -> n >= lo && n <= hi

    wantRtd = \case
      RecordTypeVal t -> pure t
      v -> throwScheme (TypeError "a record type" v)
    fieldIndex rtd f = case lookup f (zip (rtdFields rtd) [0 ..]) of
      Just i -> pure i
      Nothing -> throwScheme (UserError ("define-record-type: no such field: " <> symbolName f) [])

    interactionEnv =
      readIORef (rtInteraction rt) >>= \case
        Just g -> pure g
        Nothing -> pure (rtImpl rt)
    targetEnv = \case
      Nothing -> interactionEnv
      Just (EnvVal g) -> pure g
      Just v -> throwScheme (TypeError "an environment" v)
    r5rsEnv = do
      g <- newGlobalEnv "r5rs"
      importInto (exFor rt ".") g [DList [DSymbol "scheme", DSymbol "r5rs"] Nothing]
      pure g

    exitCodeOf = \case
      [] -> pure ExitSuccess
      [Boolean True] -> pure ExitSuccess
      [Boolean False] -> pure (ExitFailure 1)
      [v] -> do
        code <- wantInteger v
        pure (if code == 0 then ExitSuccess else ExitFailure (fromInteger (max 1 (min 255 code))))
      _ -> impossible
    flushAll = hFlush stdout >> hFlush stderr
