-- | The derived forms, cond, case, do, guard, quasiquote and the others,
-- as rewrites into simpler syntax. Mostly the definitions from chapter
-- 7.3 of the report. Every identifier a rewrite introduces is an implRef
-- alias, so the result resolves in the implementation environment no
-- matter what the use site has rebound.
module Iota.Expand.Derived
  ( rewrite
  , condClauses
  , quasiquote
  ) where

import Control.Monad (forM)
import Iota.Expand.Common
import Iota.Syntax
import Iota.Types
import Iota.Value (identBase)

-- | The name is already resolved, so a renamed import of cond still
-- lands on the cond case.
rewrite :: Ex -> SynEnv -> Symbol -> Datum -> IO Datum
rewrite ex env name form = case (name, formArgs form) of
  ("and", []) -> pure (DBool True)
  ("and", [x]) -> pure x
  ("and", x : more) -> do
    ifK <- ref "if"
    andK <- ref "and"
    pure (l [ifK, x, l (andK : more), DBool False])
  ("or", []) -> pure (DBool False)
  ("or", [x]) -> pure x
  ("or", x : more) -> do
    letK <- ref "let"
    ifK <- ref "if"
    orK <- ref "or"
    t <- ref "t"
    pure (l [letK, l [l [t, x]], l [ifK, t, t, l (orK : more)]])
  ("when", test : body@(_ : _)) -> do
    ifK <- ref "if"
    beginK <- ref "begin"
    pure (l [ifK, test, l (beginK : body)])
  ("unless", test : body@(_ : _)) -> do
    ifK <- ref "if"
    beginK <- ref "begin"
    pure (l [ifK, test, l [ifK, DBool False, DBool False], l (beginK : body)])
  ("cond", clauses) -> condClauses ex env clauses Nothing
  ("case", key : clauses) -> do
    letK <- ref "let"
    t <- ref "key"
    body <- caseClauses ex env t clauses
    pure (l [letK, l [l [t, key]], body])
  ("do", DList specs Nothing : DList (test : results) Nothing : commands) -> do
    letK <- ref "let"
    ifK <- ref "if"
    beginK <- ref "begin"
    loop <- ref "loop"
    parsed <- forM specs $ \spec -> case spec of
      DList [v, i] Nothing | isIdent v -> pure (v, i, v)
      DList [v, i, s] Nothing | isIdent v -> pure (v, i, s)
      _ -> syntaxError "do binding" spec
    let bindings = l [l [v, i] | (v, i, _) <- parsed]
        steps = [s | (_, _, s) <- parsed]
        resultExpr = case results of
          [] -> l [ifK, DBool False, DBool False]
          _ -> l (beginK : results)
    pure (l [letK, loop, bindings, l [ifK, test, resultExpr, l (beginK : commands <> [l (loop : steps)])]])
  ("let-values", DList bindings Nothing : body) -> letValues ex bindings body
  ("let*-values", DList bindings Nothing : body) -> case bindings of
    [] -> do
      letK <- ref "let"
      pure (l (letK : l [] : body))
    (b : more) -> do
      lv <- ref "let-values"
      lsv <- ref "let*-values"
      pure (l [lv, l [b], l (lsv : l more : body)])
  ("delay", [e]) -> do
    df <- ref "%delay-force"
    lam <- ref "lambda"
    mp <- ref "make-promise"
    pure (l [df, l [lam, l [], l [mp, e]]])
  ("delay-force", [e]) -> do
    df <- ref "%delay-force"
    lam <- ref "lambda"
    pure (l [df, l [lam, l [], e]])
  ("parameterize", DList bindings Nothing : body) -> do
    pairs <- mapM parameterBinding bindings
    pz <- ref "%parameterize"
    listK <- ref "list"
    lam <- ref "lambda"
    pure (l [pz, l (listK : map fst pairs), l (listK : map snd pairs), l (lam : l [] : body)])
  ("guard", DList (var : clauses) Nothing : body) | isIdent var -> do
    g <- ref "%guard"
    lam <- ref "lambda"
    reraise <- ref "reraise"
    handlerBody <- condClauses ex env clauses (Just (l [reraise]))
    pure (l [g, l (lam : l [] : body), l [lam, l [var, reraise], handlerBody]])
  ("case-lambda", clauses) -> do
    cl <- ref "%case-lambda"
    lam <- ref "lambda"
    lams <- forM clauses $ \c -> case c of
      DList (formals : cbody) Nothing -> pure (l (lam : formals : cbody))
      _ -> syntaxError "case-lambda clause" c
    pure (l (cl : lams))
  _ -> syntaxError (symbolName name) form
  where
    ref = implRef ex
    l = listOf
    parameterBinding b = case b of
      DList [p, v] Nothing -> pure (p, v)
      _ -> syntaxError "parameterize binding" b

-- | cond as nested ifs. The fallthrough is what to do when no clause
-- matches. cond has none, guard passes a call to reraise.
condClauses :: Ex -> SynEnv -> [Datum] -> Maybe Datum -> IO Datum
condClauses ex env clauses fallthrough = go clauses
  where
    ref = implRef ex
    l = listOf
    go [] = case fallthrough of
      Just d -> pure d
      Nothing -> do
        ifK <- ref "if"
        pure (l [ifK, DBool False, DBool False])
    go (clause : rest) = case clause of
      DList (test : body) Nothing -> do
        isElse <- isSpecial env test "else"
        if isElse
          then do
            beginK <- ref "begin"
            pure (l (beginK : body))
          else do
            ifK <- ref "if"
            beginK <- ref "begin"
            letK <- ref "let"
            elseCode <- go rest
            case body of
              [] -> do
                t <- ref "t"
                pure (l [letK, l [l [t, test]], l [ifK, t, t, elseCode]])
              [arrow, receiver] -> do
                isArrow <- isSpecial env arrow "=>"
                if isArrow
                  then do
                    t <- ref "t"
                    pure (l [letK, l [l [t, test]], l [ifK, t, l [receiver, t], elseCode]])
                  else pure (l [ifK, test, l (beginK : body), elseCode])
              _ -> pure (l [ifK, test, l (beginK : body), elseCode])
      _ -> syntaxError "cond clause" clause

-- | case clauses. The key sits in a temporary and each clause tests it
-- with memv.
caseClauses :: Ex -> SynEnv -> Datum -> [Datum] -> IO Datum
caseClauses ex env key = go
  where
    ref = implRef ex
    l = listOf
    go [] = do
      ifK <- ref "if"
      pure (l [ifK, DBool False, DBool False])
    go (clause : rest) = case clause of
      DList (datums : body) Nothing -> do
        isElse <- isSpecial env datums "else"
        beginK <- ref "begin"
        bodyCode <- case body of
          [arrow, receiver] -> do
            isArrow <- isSpecial env arrow "=>"
            pure (if isArrow then l [receiver, key] else l (beginK : body))
          _ -> pure (l (beginK : body))
        if isElse
          then pure bodyCode
          else case datums of
            DList ds Nothing -> do
              ifK <- ref "if"
              memv <- ref "memv"
              quote <- ref "quote"
              elseCode <- go rest
              pure (l [ifK, l [memv, key, l [quote, stripSyntax (l ds)]], bodyCode, elseCode])
            _ -> syntaxError "case clause" clause
      _ -> syntaxError "case clause" clause

-- | let-values. Nested call-with-values binding temporaries, then one
-- let that gives the temporaries the user's names. The temporaries stop
-- a later binding's expression from seeing an earlier binding's
-- variables.
letValues :: Ex -> [Datum] -> [Datum] -> IO Datum
letValues ex bindings body = do
  letK <- ref "let"
  cwv <- ref "call-with-values"
  lam <- ref "lambda"
  parsed <- forM bindings $ \b -> case b of
    DList [formals, e] Nothing -> do
      (params, rest) <- parseFormals formals
      let names = params <> maybe [] pure rest
      temps <- mapM (\n -> freshAlias (topEnv (rtImpl (exRt ex))) (DSymbol (identBase n))) names
      let (ptemps, rtemp) = splitAt (length params) temps
          tformals = DList ptemps (case rtemp of [t] -> Just t; _ -> Nothing)
      pure (e, tformals, zip names temps)
    _ -> syntaxError "let-values binding" b
  let inner = l (letK : l [l [n, t] | (_, _, pairs) <- parsed, (n, t) <- pairs] : body)
      wrap (e, tformals, _) acc = l [cwv, l [lam, l [], e], l [lam, tformals, acc]]
  pure (foldr wrap inner parsed)
  where
    ref = implRef ex
    l = listOf

-- | Quasiquote as calls to cons, append, list->vector and quote. The
-- depth counter handles nested quasiquotes. An unquote at depth greater
-- than one is rebuilt as data rather than evaluated. A subtree with no
-- unquote in it is quoted whole.
quasiquote :: Ex -> SynEnv -> Int -> Datum -> IO Datum
quasiquote ex env depth template = do
  needs <- hasUnquote depth template
  if not needs
    then quoted template
    else case template of
      DList [u, x] Nothing -> do
        isUnquote <- isSpecial env u "unquote"
        isQuasi <- isSpecial env u "quasiquote"
        if
          | isUnquote && depth == 1 -> pure x
          | isUnquote -> do
              listK <- ref "list"
              inner <- quasiquote ex env (depth - 1) x
              qu <- quoted (DSymbol "unquote")
              pure (l [listK, qu, inner])
          | isQuasi -> do
              listK <- ref "list"
              inner <- quasiquote ex env (depth + 1) x
              qq <- quoted (DSymbol "quasiquote")
              pure (l [listK, qq, inner])
          | otherwise -> listTemplate [u, x] Nothing
      DList items tl -> listTemplate items tl
      DVector items -> do
        l2v <- ref "list->vector"
        inner <- quasiquote ex env depth (l items)
        pure (l [l2v, inner])
      _ -> quoted template
  where
    ref = implRef ex
    l = listOf
    quoted d = do
      quote <- ref "quote"
      pure (l [quote, stripSyntax d])

    -- Cons up the elements. A splice becomes an append. The two empty
    -- list cases handle the tail, dotted or not, and that is what makes
    -- `(1 . ,x) work.
    listTemplate [] Nothing = quoted (l [])
    listTemplate [] (Just tl) = quasiquote ex env depth tl
    listTemplate (item : more) tl = do
      restCode <- listTemplate more tl
      splice <- case item of
        DList [us, x] Nothing -> do
          isSplice <- isSpecial env us "unquote-splicing"
          pure (if isSplice then Just x else Nothing)
        _ -> pure Nothing
      case splice of
        Just x
          | depth == 1 -> do
              appendK <- ref "append"
              pure (l [appendK, x, restCode])
          | otherwise -> do
              consK <- ref "cons"
              listK <- ref "list"
              inner <- quasiquote ex env (depth - 1) x
              qus <- quoted (DSymbol "unquote-splicing")
              pure (l [consK, l [listK, qus, inner], restCode])
        Nothing -> do
          consK <- ref "cons"
          itemCode <- quasiquote ex env depth item
          pure (l [consK, itemCode, restCode])

    hasUnquote :: Int -> Datum -> IO Bool
    hasUnquote d = \case
      DList [u, x] Nothing -> do
        isU <- isSpecial env u "unquote"
        isS <- isSpecial env u "unquote-splicing"
        isQ <- isSpecial env u "quasiquote"
        if
          | (isU || isS) && d == 1 -> pure True
          | isU || isS -> hasUnquote (d - 1) x
          | isQ -> hasUnquote (d + 1) x
          | otherwise -> (||) <$> hasUnquote d u <*> hasUnquote d x
      DList items tl -> do
        a <- or <$> mapM (hasUnquote d) items
        b <- maybe (pure False) (hasUnquote d) tl
        pure (a || b)
      DVector items -> or <$> mapM (hasUnquote d) items
      _ -> pure False
