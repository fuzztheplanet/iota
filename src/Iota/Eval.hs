-- | The evaluator. Fully expanded Core goes in, a closure in continuation
-- passing style comes out. The reason for CPS is call/cc. A direct style
-- evaluator in IO cannot re-enter a continuation, and once everything is
-- CPS anyway, tail calls are free. Every Scheme call becomes a Haskell
-- tail call with the continuation passed along, and whatever work is
-- still pending lives on the heap, not the Haskell stack.
--
-- When a primitive fails it throws a SchemeError. applyK catches that at
-- the call and passes the condition object to the current Scheme handler
-- together with the continuation of the failing call. Only if no handler
-- is installed does the exception keep going up to the boundary in
-- "Iota.Interpreter".
module Iota.Eval
  ( -- * Compilation
    compile
  , runCore

    -- * Application
  , applyK
  , guardIO

    -- * Exceptions
  , raiseError
  , raiseValue
  , errorToValue
  , withExceptionHandler

    -- * Continuations and dynamic state
  , callCC
  , dynamicWind
  , rewind
  , unwindAll
  , readDyn
  , withHandlers
  ) where

import Control.Exception (throwIO, try)
import Data.IORef
import Data.Text qualified as T
import Data.Vector.Mutable qualified as MV
import Iota.Counter (freshId)
import Iota.Types
import Iota.Value

-- | The frame chain at compile time, innermost first. A local variable
-- is addressed by depth and index.
type Scope = [[LocalVar]]

address :: Scope -> LocalVar -> (Int, Int)
address scope var = go 0 scope
  where
    go _ [] = error ("internal error: unresolved local " <> show (localName var))
    go depth (frame : rest) = case lookup (localId var) (zip (map localId frame) [0 ..]) of
      Just i -> (depth, i)
      Nothing -> go (depth + 1) rest

frameAt :: Frame -> Int -> MV.IOVector Value
frameAt (Frame slots _) 0 = slots
frameAt (Frame _ parent) n = frameAt parent (n - 1)
frameAt NoFrame _ = error "internal error: frame depth out of range"

-- | Core to CPS closures. The runtime is captured once here instead of
-- being threaded through every closure. Compiled code needs it to get
-- at the dynamic state.
compile :: Runtime -> Scope -> Core -> Code
compile rt = go
  where
    go :: Scope -> Core -> Code
    go scope core = case core of
      CConst v -> \_ k -> k v
      CLocalRef var ->
        let (d, i) = address scope var
         in \env k -> do
              v <- MV.read (frameAt env d) i
              case v of
                Undefined -> raiseError rt (UnboundVariable (localName var)) k
                _ -> k v
      CGlobalRef cell -> \_ k -> do
        v <- readIORef (cellRef cell)
        case v of
          Undefined -> raiseError rt (UnboundVariable (cellName cell)) k
          _ -> k v
      CSetLocal var c ->
        let (d, i) = address scope var
            cv = go scope c
         in \env k -> cv env $ \v -> do
              MV.write (frameAt env d) i v
              k Unspecified
      CSetGlobal cell c ->
        let cv = go scope c
         in \env k -> cv env $ \v -> do
              old <- readIORef (cellRef cell)
              case old of
                Undefined -> raiseError rt (UnboundVariable (cellName cell)) k
                _ -> writeIORef (cellRef cell) v >> k Unspecified
      CDefineGlobal cell c ->
        let cv = go scope c
         in \env k -> cv env $ \v -> do
              writeIORef (cellRef cell) v
              k Unspecified
      CIf c t e ->
        let cc = go scope c
            ct = go scope t
            ce = go scope e
         in \env k -> cc env $ \v -> if isTruthy v then ct env k else ce env k
      CLambda {..} ->
        let vars = lamParams <> maybe [] pure lamRest
            body = go (vars : scope) lamBody
            nparams = length lamParams
            hasRest = maybe False (const True) lamRest
            name = symbolName lamName
         in \env k -> do
              i <- freshId
              k
                ( Proc
                    Closure
                      { closureId = i
                      , closureName = name
                      , closureNParams = nparams
                      , closureHasRest = hasRest
                      , closureBody = body
                      , closureEnv = env
                      }
                )
      CSeq [] -> \_ k -> k Unspecified
      CSeq [c] -> go scope c
      CSeq (c : cs) ->
        let cc = go scope c
            rest = go scope (CSeq cs)
         in \env k -> cc env $ \_ -> rest env k
      -- Small arities avoid the accumulator in evalArgs. Almost every call
      -- has three arguments or fewer.
      CApp f [] ->
        let cf = go scope f
         in \env k -> cf env $ \fv -> applyK rt fv [] k
      CApp f [a] ->
        let cf = go scope f
            ca = go scope a
         in \env k -> cf env $ \fv -> ca env $ \av -> applyK rt fv [av] k
      CApp f [a, b] ->
        let cf = go scope f
            ca = go scope a
            cb = go scope b
         in \env k -> cf env $ \fv -> ca env $ \av -> cb env $ \bv -> applyK rt fv [av, bv] k
      CApp f [a, b, c] ->
        let cf = go scope f
            ca = go scope a
            cb = go scope b
            cc = go scope c
         in \env k -> cf env $ \fv -> ca env $ \av -> cb env $ \bv -> cc env $ \cv -> applyK rt fv [av, bv, cv] k
      CApp f args ->
        let cf = go scope f
            cargs = map (go scope) args
         in \env k -> cf env $ \fv -> evalArgs cargs env [] $ \argv -> applyK rt fv argv k
      CLet binds body ->
        let vars = map fst binds
            inits = map (go scope . snd) binds
            cbody = go (vars : scope) body
            n = length vars
         in \env k -> evalArgs inits env [] $ \vals -> do
              slots <- MV.new n
              mapM_ (uncurry (MV.write slots)) (zip [0 ..] vals)
              cbody (Frame slots env) k
      CLetrec binds body ->
        let vars = map fst binds
            scope' = vars : scope
            inits = zip [0 ..] (map (go scope' . snd) binds)
            cbody = go scope' body
            n = length vars
         in \env k -> do
              slots <- MV.replicate n Undefined
              let env' = Frame slots env
                  run [] = cbody env' k
                  run ((i, ci) : rest) = ci env' $ \v -> MV.write slots i v >> run rest
              run inits

    -- Left to right. The report leaves the order open but this is what
    -- people expect.
    evalArgs :: [Code] -> Frame -> [Value] -> ([Value] -> IO Value) -> IO Value
    evalArgs [] _ acc kont = kont (reverse acc)
    evalArgs (c : cs) env acc kont = c env $ \v -> evalArgs cs env (v : acc) kont

-- | Compile and run a top level form.
runCore :: Runtime -> Core -> IO Value
runCore rt core = compile rt [] core NoFrame pure

-- | Apply a procedure to already evaluated arguments and pass the result
-- to k. Arity is checked here and nowhere else.
applyK :: Runtime -> Value -> [Value] -> K -> IO Value
applyK rt f args k = case f of
  Proc p -> case p of
    Primitive {..}
      | arityOk primArity n -> do
          r <- try (primFn args)
          case r of
            Right v -> k v
            Left e -> raiseError rt e k
      | otherwise -> raiseError rt (ArityMismatch primName primArity n) k
    PrimitiveK {..}
      | arityOk primArity n -> primFnK args k
      | otherwise -> raiseError rt (ArityMismatch primName primArity n) k
    Closure {..}
      | closureHasRest && n >= closureNParams -> do
          slots <- MV.new (closureNParams + 1)
          let (positional, extra) = splitAt closureNParams args
          mapM_ (uncurry (MV.write slots)) (zip [0 ..] positional)
          rest <- list extra
          MV.write slots closureNParams rest
          closureBody (Frame slots closureEnv) k
      | not closureHasRest && n == closureNParams -> do
          slots <- MV.new n
          mapM_ (uncurry (MV.write slots)) (zip [0 ..] args)
          closureBody (Frame slots closureEnv) k
      | otherwise ->
          raiseError
            rt
            ( ArityMismatch
                closureName
                (if closureHasRest then AtLeast closureNParams else Exactly closureNParams)
                n
            )
            k
    -- Calling a continuation. Run the dynamic-wind transitions between
    -- here and where it was captured, install its dynamic state, then
    -- call the closure. The current k is discarded.
    Continuation {..} -> do
      d <- readIORef (rtDyn rt)
      rewind rt (dynWinds d) (dynWinds contDyn) $ do
        writeIORef (rtDyn rt) contDyn
        contK (listToValues args)
    Parameter {..} -> case args of
      [] -> readIORef paramCell >>= k
      _ -> raiseError rt (ArityMismatch "parameter" (Exactly 0) n) k
  _ -> raiseError rt (NotApplicable f) k
  where
    n = length args
    arityOk arity m = case arity of
      Exactly x -> m == x
      AtLeast x -> m >= x
      Between lo hi -> m >= lo && m <= hi

-- | For PrimitiveK bodies. Run some IO that may throw a SchemeError,
-- usually argument checking, then continue with the result. A failure
-- goes to the Scheme handler with k as the continuation. The
-- continuation runs outside the try, otherwise an error raised further
-- down would be caught here a second time.
guardIO :: Runtime -> K -> IO a -> (a -> IO Value) -> IO Value
guardIO rt k action kont = do
  r <- try action
  case r of
    Right a -> kont a
    Left e -> raiseError rt e k

readDyn :: Runtime -> IO DynState
readDyn rt = readIORef (rtDyn rt)

-- | Install a handler stack for the duration of the action. The old one
-- comes back when the action's continuation fires.
withHandlers :: Runtime -> [Value] -> (K -> IO Value) -> K -> IO Value
withHandlers rt handlers action k = do
  d <- readIORef (rtDyn rt)
  writeIORef (rtDyn rt) d {dynHandlers = handlers}
  action $ \v -> do
    modifyIORef' (rtDyn rt) (\d' -> d' {dynHandlers = dynHandlers d})
    k v

-- | An error found on the Haskell side. With no handler installed the
-- SchemeError is thrown as is and the interpreter boundary prints it.
-- Otherwise it becomes a condition object and gets raised.
raiseError :: Runtime -> SchemeError -> K -> IO Value
raiseError rt e k = do
  d <- readIORef (rtDyn rt)
  case dynHandlers d of
    [] -> throwIO e
    _ -> do
      v <- errorToValue e
      raiseValue rt False v k

-- | raise, or raise-continuable when the Bool is set. The handler runs
-- with the handler stack minus itself. For a continuable raise the
-- handler's return value goes back to the raise point. For a plain raise
-- the handler is not supposed to return at all, and if it does that is a
-- second error, raised where the handler is running, so with the outer
-- handlers in place.
raiseValue :: Runtime -> Bool -> Value -> K -> IO Value
raiseValue rt continuable obj k = do
  d <- readIORef (rtDyn rt)
  case dynHandlers d of
    [] -> throwIO (RaisedObject obj)
    (h : outer) -> do
      writeIORef (rtDyn rt) d {dynHandlers = outer}
      applyK rt h [obj] $ \v ->
        if continuable
          then do
            modifyIORef' (rtDyn rt) (\d' -> d' {dynHandlers = dynHandlers d})
            k v
          else
            raiseValue
              rt
              False
              (ErrObj ErrorGeneral "exception handler returned from non-continuable raise" [obj])
              k

-- | @(with-exception-handler handler thunk)@.
withExceptionHandler :: Runtime -> Value -> Value -> K -> IO Value
withExceptionHandler rt handler thunk k = do
  d <- readIORef (rtDyn rt)
  withHandlers rt (handler : dynHandlers d) (applyK rt thunk []) k

-- | What a SchemeError looks like once a Scheme handler gets to see it.
errorToValue :: SchemeError -> Eval Value
errorToValue = \case
  RaisedObject v -> pure v
  UserError msg irritants -> pure (ErrObj ErrorGeneral msg irritants)
  FileError t -> pure (ErrObj ErrorFile t [])
  ParseFailure t -> pure (ErrObj ErrorRead (T.stripEnd t) [])
  UnboundVariable s -> pure (ErrObj ErrorGeneral "unbound variable" [Sym s])
  NotApplicable v -> pure (ErrObj ErrorGeneral "not applicable" [v])
  TypeError expected v -> pure (ErrObj ErrorGeneral ("expected " <> expected) [v])
  BadSyntax what form -> pure (ErrObj ErrorGeneral ("bad syntax in " <> what) [form])
  IndexOutOfRange who i -> pure (ErrObj ErrorGeneral (who <> ": index out of range") [integer (fromIntegral i)])
  MacroError t d -> do
    v <- datumToValue d
    pure (ErrObj ErrorGeneral ("syntax error: " <> t) [v])
  other -> pure (ErrObj ErrorGeneral (shallowShow other) [])

-- | call/cc. The continuation object is k and a copy of the dynamic
-- state. Nothing gets unwound or copied at capture time.
callCC :: Runtime -> Value -> K -> IO Value
callCC rt f k = do
  d <- readIORef (rtDyn rt)
  i <- freshId
  applyK rt f [Proc Continuation {contId = i, contK = k, contDyn = d}] k

-- | dynamic-wind on the normal path. Escapes and re-entries are handled
-- by rewind when a continuation is called.
dynamicWind :: Runtime -> Value -> Value -> Value -> K -> IO Value
dynamicWind rt before thunk after k =
  applyK rt before [] $ \_ -> do
    i <- freshId
    let frame = WindFrame {windId = i, windBefore = before, windAfter = after}
    modifyIORef' (rtDyn rt) (\d -> d {dynWinds = frame : dynWinds d})
    applyK rt thunk [] $ \v -> do
      modifyIORef' (rtDyn rt) (\d -> d {dynWinds = drop 1 (dynWinds d)})
      applyK rt after [] $ \_ -> k v

-- | Move from one wind list to another. Find the common tail. Run the
-- after thunks of everything above it in the old list, innermost first.
-- Then run the before thunks of everything above it in the new list,
-- outermost first. Then carry on.
rewind :: Runtime -> [WindFrame] -> [WindFrame] -> IO Value -> IO Value
rewind rt from to cont = runExits (length from - commonLen) from
  where
    commonLen = length (takeWhile id (zipWith sameFrame (reverse from) (reverse to)))
    sameFrame a b = windId a == windId b
    entries = reverse (take (length to - commonLen) to)
    -- An after thunk runs with its own frame already popped. An escape
    -- from inside it must not run it again.
    runExits :: Int -> [WindFrame] -> IO Value
    runExits n ws
      | n <= 0 = runEntries entries cont
      | otherwise = case ws of
          (f : rest) -> do
            modifyIORef' (rtDyn rt) (\d -> d {dynWinds = rest})
            applyK rt (windAfter f) [] $ \_ -> runExits (n - 1) rest
          [] -> runEntries entries cont
    runEntries [] next = next
    runEntries (f : fs) next =
      applyK rt (windBefore f) [] $ \_ -> do
        modifyIORef' (rtDyn rt) (\d -> d {dynWinds = f : dynWinds d})
        runEntries fs next

-- | Leave every dynamic-wind. For exit.
unwindAll :: Runtime -> IO Value -> IO Value
unwindAll rt cont = do
  d <- readIORef (rtDyn rt)
  rewind rt (dynWinds d) [] $ do
    writeIORef (rtDyn rt) d {dynWinds = []}
    cont
