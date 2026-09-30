-- | Pairs and lists. Only what has to be Haskell. The cxr accessors, the
-- folds and the three-argument member and assoc are in the stdlib.
module Iota.Primitives.Pairs
  ( pairPrimitives
  ) where

import Iota.Types
import Iota.Value

pairPrimitives :: [(Symbol, Value)]
pairPrimitives =
  [ prim "cons" (Exactly 2) $ \case
      [a, b] -> cons a b
      _ -> impossible
  , prim "car" (Exactly 1) $ \case
      [v] -> car v
      _ -> impossible
  , prim "cdr" (Exactly 1) $ \case
      [v] -> cdr v
      _ -> impossible
  , prim "set-car!" (Exactly 2) $ \case
      [p, v] -> setCar p v >> pure Unspecified
      _ -> impossible
  , prim "set-cdr!" (Exactly 2) $ \case
      [p, v] -> setCdr p v >> pure Unspecified
      _ -> impossible
  , prim "pair?" (Exactly 1) $ \case
      [Pair {}] -> pure (Boolean True)
      [_] -> pure (Boolean False)
      _ -> impossible
  , prim "null?" (Exactly 1) $ \case
      [Null] -> pure (Boolean True)
      [_] -> pure (Boolean False)
      _ -> impossible
  , prim "list?" (Exactly 1) $ \case
      [v] -> Boolean <$> isList v
      _ -> impossible
  , prim "list" (AtLeast 0) list
  , prim "length" (Exactly 1) $ \case
      [v] ->
        listLength v >>= \case
          Just n -> pure (integer (fromIntegral n))
          Nothing -> throwScheme (TypeError "a proper list" v)
      _ -> impossible
  , -- the last argument is shared, not copied, and can be anything, so
    -- (append '(1) 2) gives (1 . 2)
    prim "append" (AtLeast 0) $ \case
      [] -> pure Null
      args -> do
        let (inits', lastArg) = (init args, last args)
        parts <- mapM (requireList "append") inits'
        listWithTail (concat parts) lastArg
  , prim "reverse" (Exactly 1) $ \case
      [v] -> requireList "reverse" v >>= list . reverse
      _ -> impossible
  , prim "list-tail" (Exactly 2) $ \case
      [v, kv] -> do
        k <- wantIndex kv
        dropTail k v
      _ -> impossible
  , prim "list-ref" (Exactly 2) $ \case
      [v, kv] -> do
        k <- wantIndex kv
        rest <- dropTail k v
        car rest
      _ -> impossible
  , prim "list-copy" (Exactly 1) $ \case
      [v] -> do
        (xs, tl) <- improperList v
        listWithTail xs tl
      _ -> impossible
  , -- two-argument searches. The stdlib wraps these to add the optional
    -- comparator.
    prim "memq" (Exactly 2) $ member eqp
  , prim "memv" (Exactly 2) $ member eqv
  , prim "member" (Exactly 2) $ member equal
  , prim "assq" (Exactly 2) $ assoc eqp
  , prim "assv" (Exactly 2) $ assoc eqv
  , prim "assoc" (Exactly 2) $ assoc equal
  , -- the equivalence predicates
    prim "eq?" (Exactly 2) $ \case
      [a, b] -> Boolean <$> eqp a b
      _ -> impossible
  , prim "eqv?" (Exactly 2) $ \case
      [a, b] -> Boolean <$> eqv a b
      _ -> impossible
  , prim "equal?" (Exactly 2) $ \case
      [a, b] -> Boolean <$> equal a b
      _ -> impossible
  ]
  where
    prim name arity fn = (name, primitive (symbolName name) arity fn)

    impossible :: Eval a
    impossible = throwScheme (UserError "internal arity error" [])

    dropTail 0 v = pure v
    dropTail k v = cdr v >>= dropTail (k - 1 :: Int)

    -- Fuel, so a circular list can't hang the search.
    fuel0 = 10_000_000 :: Int

    member cmp = \case
      [x, lst] -> go fuel0 lst
        where
          go _ Null = pure (Boolean False)
          go 0 v = throwScheme (TypeError "a proper list" v)
          go fuel v@(Pair {}) = do
            h <- car v
            hit <- cmp x h
            if hit then pure v else cdr v >>= go (fuel - 1)
          go _ v = throwScheme (TypeError "a proper list" v)
      _ -> impossible

    assoc cmp = \case
      [x, lst] -> go fuel0 lst
        where
          go _ Null = pure (Boolean False)
          go 0 v = throwScheme (TypeError "an association list" v)
          go fuel v@(Pair {}) = do
            entry <- car v
            key <- car entry
            hit <- cmp x key
            if hit then pure entry else cdr v >>= go (fuel - 1)
          go _ v = throwScheme (TypeError "an association list" v)
      _ -> impossible
