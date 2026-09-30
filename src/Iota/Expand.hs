-- | The expander. A datum goes in and Core comes out, with macros
-- expanded, derived forms rewritten and every identifier resolved to a
-- local or a global cell. It runs once per top level form before that
-- form is evaluated. The derived forms are in "Iota.Expand.Derived" and
-- the library stuff in "Iota.Expand.Library".
module Iota.Expand
  ( -- * Contexts
    Ex (..)
  , exFor

    -- * Top level
  , expandTop
  , runTopK
  , runFormsK
  , runLocatedK
  , loadFileK
  , importInto
  , environmentFrom

    -- * Setup
  , installSpecialForms
  , featureSymbols

    -- * Files
  , readSourceFile
  , readSourceDatums
  ) where

import Control.Monad (forM, forM_, when)
import Data.IORef
import Iota.Eval
import Iota.Expand.Common
import Iota.Expand.Derived
import Iota.Expand.Library
import Iota.Syntax
import Iota.SyntaxRules
import Iota.Types
import Iota.Value
import System.FilePath (takeDirectory)

-- | A context for forms read from the given file. include uses the
-- directory.
exFor :: Runtime -> FilePath -> Ex
exFor rt path =
  Ex
    { exRt = rt
    , exDir = takeDirectory path
    , exLoc = Nothing
    , exRun = \ex genv forms -> runFormsK ex genv forms pure >> pure ()
    }

expandExpr :: Ex -> SynEnv -> Datum -> IO Core
expandExpr ex env form = case form of
  DBool _ -> constant
  DNumber _ -> constant
  DChar _ -> constant
  DString _ -> constant
  DVector _ -> constant
  DBytevector _ -> constant
  DLabel _ d -> expandExpr ex env d
  DLabelRef _ -> syntaxError "expression" form
  DList [] Nothing -> syntaxError "expression (empty application)" form
  DList _ (Just _) -> syntaxError "expression (improper list)" form
  DList (h : args) Nothing ->
    headOf env form >>= \case
      HSpecial s -> special ex env s form args
      HMacro m -> useMacro env m form >>= expandExpr ex env
      HOther -> do
        f <- expandExpr ex env h
        as <- mapM (expandExpr ex env) args
        pure (CApp f as)
  _ | isIdent form -> variable env form
  _ -> syntaxError "expression" form
  where
    -- Literals are constants. The mutators refuse them later on.
    constant = CConst <$> literalToValue (stripSyntax form)

variable :: SynEnv -> Datum -> IO Core
variable env ident =
  resolve env ident >>= \case
    Just (DenLocal v) -> pure (CLocalRef v)
    Just (DenGlobal cell) -> pure (CGlobalRef cell)
    Just (DenSpecial _) -> syntaxError "expression (syntactic keyword used as a variable)" ident
    Just (DenMacro _) -> syntaxError "expression (macro keyword used as a variable)" ident
    -- Not bound yet. Hand out a pending cell. A later define adopts it.
    Nothing -> CGlobalRef <$> pendingCell (homeEnv env ident) (identBase ident)

-- | The global table an unbound identifier belongs to. For an alias
-- that is the environment of the macro that introduced it, not the
-- place where the macro was used.
homeEnv :: SynEnv -> Datum -> GlobalEnv
homeEnv env = \case
  DAlias a -> homeEnv (aliasEnv a) (aliasIdent a)
  _ -> synGlobal env

-- | Special forms in expression position. The core ones are handled
-- right here. The derived ones get rewritten and expanded again.
special :: Ex -> SynEnv -> Symbol -> Datum -> [Datum] -> IO Core
special ex env name form args = case (name, args) of
  ("quote", [d]) -> CConst <$> literalToValue (stripSyntax d)
  ("quasiquote", [d]) -> quasiquote ex env 1 d >>= expr
  ("if", [c, t]) -> CIf <$> expr c <*> expr t <*> pure (CConst Unspecified)
  ("if", [c, t, e]) -> CIf <$> expr c <*> expr t <*> expr e
  ("set!", [target, value])
    | isIdent target -> do
        v <- expr value
        resolve env target >>= \case
          Just (DenLocal var) -> pure (CSetLocal var v)
          Just (DenGlobal cell)
            | cellOwner cell == globalId (synGlobal env) -> pure (CSetGlobal cell v)
            | otherwise -> syntaxError "set! (cannot assign an imported binding)" form
          Nothing -> CSetGlobal <$> pendingCell (homeEnv env target) (identBase target) <*> pure v
          Just _ -> syntaxError "set! (target is a syntactic keyword)" form
  ("lambda", formals : body) -> lambda ex env "lambda" formals body
  ("named-lambda", DList (n : formals) tl : body)
    | isIdent n -> lambda ex env (identBase n) (DList formals tl) body
  ("begin", body@(_ : _)) -> CSeq <$> mapM expr body
  ("let", DList bindings Nothing : body) -> letForm ex env bindings body
  ("let", loopName : bindings : body) | isIdent loopName -> namedLet ex env loopName bindings body
  ("let*", DList bindings Nothing : body) -> case bindings of
    [] -> letForm ex env [] body
    [b] -> letForm ex env [b] body
    (b : more) -> do
      letStar <- implRef ex "let*"
      letForm ex env [b] [listOf (letStar : listOf more : body)]
  ("letrec", DList bindings Nothing : body) -> letrecForm ex env bindings body
  ("letrec*", DList bindings Nothing : body) -> letrecForm ex env bindings body
  ("let-syntax", DList bindings Nothing : body) -> letSyntax ex env False bindings body
  ("letrec-syntax", DList bindings Nothing : body) -> letSyntax ex env True bindings body
  ("syntax-error", msg : irritants) -> do
    m <- case msg of
      DString t -> pure t
      _ -> syntaxError "syntax-error" form
    vs <- mapM (datumToValue . stripSyntax) irritants
    throwScheme (UserError ("syntax error: " <> m) vs)
  ("include", files) -> includeFiles ex False files >>= beginOf ex >>= expr
  ("include-ci", files) -> includeFiles ex True files >>= beginOf ex >>= expr
  ("cond-expand", clauses) -> condExpand ex env clauses >>= beginOf ex >>= expr
  ("define", _) -> syntaxError "define (not allowed in expression context)" form
  ("define-syntax", _) -> syntaxError "define-syntax (not allowed in expression context)" form
  ("define-values", _) -> syntaxError "define-values (not allowed in expression context)" form
  ("define-record-type", _) -> syntaxError "define-record-type (not allowed in expression context)" form
  ("import", _) -> syntaxError "import (only allowed at top level)" form
  ("define-library", _) -> syntaxError "define-library (only allowed at top level)" form
  _
    | name `elem` derived -> rewrite ex env name form >>= expr
    | otherwise -> syntaxError (symbolName name) form
  where
    expr = expandExpr ex env
    derived =
      [ "and", "or", "when", "unless", "cond", "case", "do", "let-values", "let*-values"
      , "delay", "delay-force", "guard", "parameterize", "case-lambda"
      ]

lambda :: Ex -> SynEnv -> Symbol -> Datum -> [Datum] -> IO Core
lambda ex env name formals body = do
  (params, rest) <- parseFormals formals
  rib <- newRib
  vars <- forM params $ \p -> do
    v <- freshLocal (identBase p)
    ribBind rib (identKey p) (DenLocal v)
    pure v
  restVar <- forM rest $ \r -> do
    v <- freshLocal (identBase r)
    ribBind rib (identKey r) (DenLocal v)
    pure v
  bodyCore <- expandBody ex (pushRib rib env) body
  pure CLambda {lamName = name, lamParams = vars, lamRest = restVar, lamBody = bodyCore}

letForm :: Ex -> SynEnv -> [Datum] -> [Datum] -> IO Core
letForm ex env bindings body = do
  pairs <- bindingPairs bindings
  inits <- forM pairs $ \(n, e) -> nameLambda (identBase n) <$> expandExpr ex env e
  rib <- newRib
  vars <- forM pairs $ \(n, _) -> do
    v <- freshLocal (identBase n)
    ribBind rib (identKey n) (DenLocal v)
    pure v
  bodyCore <- expandBody ex (pushRib rib env) body
  pure (CLet (zip vars inits) bodyCore)

letrecForm :: Ex -> SynEnv -> [Datum] -> [Datum] -> IO Core
letrecForm ex env bindings body = do
  pairs <- bindingPairs bindings
  rib <- newRib
  let env' = pushRib rib env
  vars <- forM pairs $ \(n, _) -> do
    v <- freshLocal (identBase n)
    ribBind rib (identKey n) (DenLocal v)
    pure v
  inits <- forM pairs $ \(n, e) -> nameLambda (identBase n) <$> expandExpr ex env' e
  bodyCore <- expandBody ex env' body
  pure (CLetrec (zip vars inits) bodyCore)

-- | Named let. The initial values are expanded in the outer scope,
-- before the loop name is bound.
namedLet :: Ex -> SynEnv -> Datum -> Datum -> [Datum] -> IO Core
namedLet ex env loopName bindings body = do
  pairs <- case bindings of
    DList bs Nothing -> bindingPairs bs
    _ -> syntaxError "named let bindings" bindings
  inits <- mapM (expandExpr ex env . snd) pairs
  rib <- newRib
  loopVar <- freshLocal (identBase loopName)
  ribBind rib (identKey loopName) (DenLocal loopVar)
  lam <- lambda ex (pushRib rib env) (identBase loopName) (listOf (map fst pairs)) body
  pure (CLetrec [(loopVar, lam)] (CApp (CLocalRef loopVar) inits))

letSyntax :: Ex -> SynEnv -> Bool -> [Datum] -> [Datum] -> IO Core
letSyntax ex env recursive bindings body = do
  pairs <- bindingPairs bindings
  rib <- newRib
  let env' = pushRib rib env
      macroEnv' = if recursive then env' else env
  forM_ pairs $ \(n, spec) -> do
    m <- makeMacro ex macroEnv' (identBase n) spec
    ribBind rib (identKey n) (DenMacro m)
  expandBody ex env' body

-- | A transformer spec has to be a syntax-rules form, or a macro use
-- that expands into one.
makeMacro :: Ex -> SynEnv -> Symbol -> Datum -> IO Macro
makeMacro ex env name spec =
  headOf env spec >>= \case
    HSpecial "syntax-rules" ->
      makeSyntaxRules env name spec >>= \case
        Right m -> pure m
        Left err -> throwScheme (MacroError err (stripSyntax spec))
    HMacro m -> useMacro env m spec >>= makeMacro ex env name
    _ -> syntaxError "define-syntax (expected a syntax-rules transformer)" spec

-- | A body. Internal definitions turn into one letrec* around the
-- expressions. To find them, each form's head is expanded far enough to
-- tell what it is, and every defined name is bound before any
-- initialiser is expanded. Otherwise a macro defined further down the
-- body would not be visible from an earlier initialiser. A definition
-- after the first expression is an error. The report says so, and it
-- makes the scan simpler.
expandBody :: Ex -> SynEnv -> [Datum] -> IO Core
expandBody ex env forms = do
  rib <- newRib
  let env' = pushRib rib env
  (defs, exprs) <- scanBody ex env' rib forms
  bodyCore <- case exprs of
    [] -> pure (CConst Unspecified)
    _ -> CSeq <$> mapM (expandExpr ex env') exprs
  if null defs
    then pure bodyCore
    else do
      binds <- forM defs $ \(var, n, initExpr) -> do
        c <- nameLambda n <$> expandExpr ex env' initExpr
        pure (var, c)
      pure (CLetrec binds bodyCore)

-- | Split a body into definitions and expressions. A definition is the
-- variable, its name and the initialiser.
scanBody :: Ex -> SynEnv -> Rib -> [Datum] -> IO ([(LocalVar, Symbol, Datum)], [Datum])
scanBody ex env rib = go [] []
  where
    go defs exprs [] = pure (reverse defs, reverse exprs)
    go defs exprs (form : rest) =
      headOf env form >>= \case
        HSpecial "begin" -> case form of
          DList (_ : inner) Nothing -> go defs exprs (inner <> rest)
          _ -> syntaxError "begin" form
        HSpecial "define" -> do
          definition form
          (ident, initExpr) <- defineParts ex form
          var <- bindDefined ident
          go ((var, identBase ident, initExpr) : defs) exprs rest
        HSpecial "define-values" -> do
          definition form
          forms <- defineValuesInBody ex form
          go defs exprs (forms <> rest)
        HSpecial "define-record-type" -> do
          definition form
          forms <- defineRecordType ex form
          go defs exprs (forms <> rest)
        HSpecial "define-syntax" -> do
          definition form
          case form of
            DList [_, n, spec] Nothing | isIdent n -> do
              m <- makeMacro ex env (identBase n) spec
              ribBind rib (identKey n) (DenMacro m)
              go defs exprs rest
            _ -> syntaxError "define-syntax" form
        HSpecial "include" -> includeFiles ex False (formArgs form) >>= \fs -> go defs exprs (fs <> rest)
        HSpecial "include-ci" -> includeFiles ex True (formArgs form) >>= \fs -> go defs exprs (fs <> rest)
        HSpecial "cond-expand" -> condExpand ex env (formArgs form) >>= \fs -> go defs exprs (fs <> rest)
        HSpecial "import" -> syntaxError "import (only allowed at top level)" form
        HSpecial "define-library" -> syntaxError "define-library (only allowed at top level)" form
        HMacro m -> do
          expanded <- useMacro env m form
          go defs exprs (expanded : rest)
        _ -> go defs (form : exprs) rest
      where
        definition d = when (not (null exprs)) $ syntaxError "body (definition after an expression)" d

    -- The same name defined twice in one body shares a variable.
    bindDefined ident =
      ribLookup rib (identKey ident) >>= \case
        Just (DenLocal v) -> pure v
        _ -> do
          v <- freshLocal (identBase ident)
          ribBind rib (identKey ident) (DenLocal v)
          pure v

-- | Reduce the shapes of define to a name and an initialiser. The
-- procedure shape becomes a named-lambda so the closure prints with its
-- name. Curried defines, the MIT style (define ((f a) b) ...), nest.
defineParts :: Ex -> Datum -> IO (Datum, Datum)
defineParts ex form = case form of
  DList [_, n, e] Nothing | isIdent n -> pure (n, e)
  DList [_, n] Nothing | isIdent n -> do
    -- (define x) with no value. Bound but unassigned.
    u <- implRef ex "%undefined"
    pure (n, listOf [u])
  DList (_ : DList (n : formals) tl : body@(_ : _)) Nothing -> do
    nl <- implRef ex "named-lambda"
    case n of
      _ | isIdent n -> pure (n, listOf (nl : DList (n : formals) tl : body))
      DList _ _ -> do
        lam <- implRef ex "lambda"
        defineParts ex (listOf [DSymbol "define", n, listOf (lam : DList formals tl : body)])
      _ -> syntaxError "define" form
  _ -> syntaxError "define" form

-- | define-values at top level. Define every name to #f first, then one
-- call-with-values that assigns them all. This is the definition from
-- the report.
defineValues :: Ex -> Datum -> IO [Datum]
defineValues ex form = case form of
  DList [_, formals, expr] Nothing -> do
    (params, rest) <- parseFormals formals
    define <- implRef ex "define"
    cwv <- implRef ex "call-with-values"
    lam <- implRef ex "lambda"
    setB <- implRef ex "set!"
    let names = params <> maybe [] pure rest
    temps <- mapM (\n -> freshAlias (topEnv (rtImpl (exRt ex))) (DSymbol (identBase n))) names
    let (ptemps, rtemp) = splitAt (length params) temps
        tformals = DList ptemps (case rtemp of [t] -> Just t; _ -> Nothing)
        defs = [listOf [define, n, DBool False] | n <- names]
        sets = [listOf [setB, n, t] | (n, t) <- zip names temps]
        receiver = listOf (lam : tformals : sets)
        producer = listOf [lam, listOf [], expr]
    pure (defs <> [listOf [cwv, producer, receiver]])
  _ -> syntaxError "define-values" form

-- | define-values inside a body cannot use the top level version, since
-- the set! expression would end the definition section. Instead the
-- values are collected into a list held by a temporary and each name is
-- defined from that.
defineValuesInBody :: Ex -> Datum -> IO [Datum]
defineValuesInBody ex form = case form of
  DList [_, formals, expr] Nothing -> do
    (params, rest) <- parseFormals formals
    define <- implRef ex "define"
    cwv <- implRef ex "call-with-values"
    lam <- implRef ex "lambda"
    listK <- implRef ex "list"
    listRef <- implRef ex "list-ref"
    listTail <- implRef ex "list-tail"
    tmp <- freshAlias (topEnv (rtImpl (exRt ex))) (DSymbol "values")
    let collect = listOf [define, tmp, listOf [cwv, listOf [lam, listOf [], expr], listK]]
        positional =
          [ listOf [define, n, listOf [listRef, tmp, DNumber (Exact (fromIntegral i))]]
          | (i, n) <- zip [0 :: Int ..] params
          ]
        restDef = case rest of
          Just r -> [listOf [define, r, listOf [listTail, tmp, DNumber (Exact (fromIntegral (length params)))]]]
          Nothing -> []
    pure (collect : positional <> restDef)
  _ -> syntaxError "define-values" form

-- | define-record-type becomes plain defines over the record primitives.
-- The type name is bound to the descriptor.
defineRecordType :: Ex -> Datum -> IO [Datum]
defineRecordType ex form = case form of
  DList (_ : typeName : ctorSpec : predName : fieldSpecs) Nothing | isIdent typeName -> do
    fields <- mapM fieldSpec fieldSpecs
    define <- implRef ex "define"
    quote <- implRef ex "quote"
    mkType <- implRef ex "%make-record-type"
    mkCtor <- implRef ex "%record-constructor"
    mkPred <- implRef ex "%record-predicate"
    mkAcc <- implRef ex "%record-accessor"
    mkMod <- implRef ex "%record-modifier"
    let q d = listOf [quote, stripSyntax d]
        fieldNames = [f | (f, _, _) <- fields]
        typeDef = listOf [define, typeName, listOf [mkType, q typeName, q (listOf fieldNames)]]
    ctorDefs <- case ctorSpec of
      DBool False -> pure []
      DList (c : cfields) Nothing | isIdent c, all isIdent cfields -> do
        forM_ cfields $ \f ->
          when (not (any (\g -> identBase g == identBase f) fieldNames)) $
            syntaxError "define-record-type (constructor field is not a field)" f
        pure [listOf [define, c, listOf [mkCtor, typeName, q (listOf cfields), q c]]]
      c | isIdent c -> pure [listOf [define, c, listOf [mkCtor, typeName, q (listOf fieldNames), q c]]]
      _ -> syntaxError "define-record-type (constructor)" ctorSpec
    predDefs <- case predName of
      DBool False -> pure []
      p | isIdent p -> pure [listOf [define, p, listOf [mkPred, typeName, q p]]]
      _ -> syntaxError "define-record-type (predicate)" predName
    let accessorDefs =
          concat
            [ listOf [define, acc, listOf [mkAcc, typeName, q f, q acc]]
                : [listOf [define, m, listOf [mkMod, typeName, q f, q m]] | Just m <- [modifier]]
            | (f, acc, modifier) <- fields
            ]
    pure (typeDef : ctorDefs <> predDefs <> accessorDefs)
  _ -> syntaxError "define-record-type" form
  where
    fieldSpec spec = case spec of
      DList [f, acc] Nothing | isIdent f, isIdent acc -> pure (f, acc, Nothing)
      DList [f, acc, m] Nothing | isIdent f, isIdent acc, isIdent m -> pure (f, acc, Just m)
      _ -> syntaxError "define-record-type (field)" spec

-- | One top level form. A definition binds its name in the global table
-- while being expanded, so later forms can see it. That includes the
-- rest of the same begin.
expandTop :: Ex -> GlobalEnv -> Datum -> IO Core
expandTop ex genv form = do
  let env = topEnv genv
  headOf env form >>= \case
    HSpecial "begin" -> case form of
      DList (_ : inner) Nothing -> CSeq <$> mapM (expandTop ex genv) inner
      _ -> syntaxError "begin" form
    HSpecial "define" -> do
      (ident, initExpr) <- defineParts ex form
      cell <- globalCell genv (identBase ident)
      c <- nameLambda (identBase ident) <$> expandExpr ex env initExpr
      pure (CDefineGlobal cell c)
    HSpecial "define-syntax" -> case form of
      DList [_, n, spec] Nothing | isIdent n -> do
        m <- makeMacro ex env (identBase n) spec
        globalBind genv (identBase n) (DenMacro m)
        pure (CConst Unspecified)
      _ -> syntaxError "define-syntax" form
    HSpecial "define-values" -> defineValues ex form >>= beginOf ex >>= expandTop ex genv
    HSpecial "define-record-type" -> defineRecordType ex form >>= beginOf ex >>= expandTop ex genv
    HSpecial "include" -> includeFiles ex False (formArgs form) >>= beginOf ex >>= expandTop ex genv
    HSpecial "include-ci" -> includeFiles ex True (formArgs form) >>= beginOf ex >>= expandTop ex genv
    HSpecial "cond-expand" -> condExpand ex env (formArgs form) >>= beginOf ex >>= expandTop ex genv
    HSpecial "import" -> do
      importInto ex genv (formArgs form)
      pure (CConst Unspecified)
    HSpecial "define-library" -> do
      defineLibrary ex form
      pure (CConst Unspecified)
    HMacro m -> useMacro env m form >>= expandTop ex genv
    _ -> expandExpr ex env form

-- | Expand, compile, run.
runTopK :: Ex -> GlobalEnv -> Datum -> K -> IO Value
runTopK ex genv form k = do
  writeIORef (rtLoc (exRt ex)) (exLoc ex)
  core <- expandTop ex genv form
  compile (exRt ex) [] core NoFrame k

-- | A sequence of top level forms. Each is expanded only after the one
-- before it has run, because a form may define a macro the next one uses.
runFormsK :: Ex -> GlobalEnv -> [Datum] -> K -> IO Value
runFormsK _ _ [] k = k Unspecified
runFormsK ex genv [d] k = runTopK ex genv d k
runFormsK ex genv (d : ds) k = runTopK ex genv d $ \_ -> runFormsK ex genv ds k

-- | Same, for forms that carry their source position.
runLocatedK :: Ex -> GlobalEnv -> [Located] -> K -> IO Value
runLocatedK _ _ [] k = k Unspecified
runLocatedK ex genv [d] k = runTopK (withLoc (locatedLoc d) ex) genv (locatedDatum d) k
runLocatedK ex genv (d : ds) k =
  runTopK (withLoc (locatedLoc d) ex) genv (locatedDatum d) $ \_ -> runLocatedK ex genv ds k

-- | load.
loadFileK :: Runtime -> GlobalEnv -> FilePath -> K -> IO Value
loadFileK rt genv path k = do
  ds <- readSourceDatums path
  runFormsK (exFor rt path) genv ds k
