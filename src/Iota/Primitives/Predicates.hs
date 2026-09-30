-- | Type predicates, booleans and symbols.
module Iota.Primitives.Predicates
  ( predicatePrimitives
  ) where

import Iota.Types
import Iota.Value

predicatePrimitives :: [(Symbol, Value)]
predicatePrimitives =
  [ typePred "boolean?" $ \case Boolean _ -> True; _ -> False
  , typePred "symbol?" $ \case Sym _ -> True; _ -> False
  , typePred "char?" $ \case Character _ -> True; _ -> False
  , typePred "string?" $ \case Str {} -> True; _ -> False
  , typePred "vector?" $ \case Vec {} -> True; _ -> False
  , typePred "bytevector?" $ \case Bytes {} -> True; _ -> False
  , typePred "procedure?" $ \case Proc _ -> True; _ -> False
  , typePred "port?" $ \case PortVal _ -> True; _ -> False
  , typePred "promise?" $ \case PromiseVal _ -> True; _ -> False
  , typePred "eof-object?" $ \case EofObject -> True; _ -> False
  , prim "not" (Exactly 1) $ \case
      [v] -> pure (Boolean (not (isTruthy v)))
      _ -> impossible
  , -- n-ary, every argument has to be a boolean and they all have to match
    prim "boolean=?" (AtLeast 2) $ \args -> do
      bs <- mapM wantBool args
      pure (Boolean (and (zipWith (==) bs (drop 1 bs))))
  , prim "symbol=?" (AtLeast 2) $ \args -> do
      ss <- mapM wantSymbol args
      pure (Boolean (and (zipWith (==) ss (drop 1 ss))))
  , prim "symbol->string" (Exactly 1) $ \case
      -- the result is a constant, mutating it is an error in R7RS
      [v] -> wantSymbol v >>= makeLiteralString . symbolName
      _ -> impossible
  , prim "string->symbol" (Exactly 1) $ \case
      [v] -> Sym . Symbol <$> wantString v
      _ -> impossible
  ]
  where
    prim name arity fn = (name, primitive (symbolName name) arity fn)

    typePred name p =
      prim name (Exactly 1) $ \case
        [v] -> pure (Boolean (p v))
        _ -> impossible

    wantBool = \case
      Boolean b -> pure b
      v -> throwScheme (TypeError "a boolean" v)

    impossible :: Eval a
    impossible = throwScheme (UserError "internal arity error" [])
