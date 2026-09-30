-- | Scope as the expander sees it. A 'SynEnv' is a stack of ribs over a
-- global table. 'resolve' turns an identifier into a 'Denotation'. The
-- hygiene part is the alias case of 'resolve'. An identifier that came
-- out of a macro template is looked up where the macro was defined, not
-- where it was used, unless the expansion itself bound it.
module Iota.Syntax
  ( -- * Global environments
    newGlobalEnv
  , globalLookup
  , globalBind
  , globalCell
  , pendingCell
  , globalNames

    -- * Ribs
  , newRib
  , ribBind
  , ribLookup
  , pushRib
  , topEnv

    -- * Identifiers
  , isIdent
  , identKey
  , resolve
  , freeIdentEq
  , isSpecial
  , freshLocal
  , freshAlias
  , stripSyntax
  ) where

import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Iota.Counter (freshId)
import Iota.Types
import Iota.Value (identBase)

-- Global environments

newGlobalEnv :: Text -> IO GlobalEnv
newGlobalEnv name = do
  i <- freshId
  table <- newIORef Map.empty
  pending <- newIORef Map.empty
  pure GlobalEnv {globalId = i, globalName = name, globalTable = table, globalPending = pending}

globalLookup :: GlobalEnv -> Symbol -> IO (Maybe Denotation)
globalLookup env s = Map.lookup s <$> readIORef (globalTable env)

globalBind :: GlobalEnv -> Symbol -> Denotation -> IO ()
globalBind env s d = modifyIORef' (globalTable env) (Map.insert s d)

-- | The cell a top level define writes to. Only a cell this environment
-- owns will do. Defining a name that was imported makes a fresh cell, so
-- the library's own variable is shadowed rather than overwritten.
-- Defining over a keyword or a macro turns the name into a variable.
globalCell :: GlobalEnv -> Symbol -> IO GlobalCell
globalCell env s =
  globalLookup env s >>= \case
    Just (DenGlobal cell) | cellOwner cell == globalId env -> pure cell
    _ -> do
      cell <- pendingCell env s
      modifyIORef' (globalPending env) (Map.delete s)
      globalBind env s (DenGlobal cell)
      pure cell

-- | A cell for a name that has been referenced but not defined yet. It
-- isn't a binding, completion doesn't see it. A later define of the same
-- name adopts it, which is how mutually recursive top level definitions
-- find each other.
pendingCell :: GlobalEnv -> Symbol -> IO GlobalCell
pendingCell env s = do
  pending <- readIORef (globalPending env)
  case Map.lookup s pending of
    Just cell -> pure cell
    Nothing -> do
      ref <- newIORef Undefined
      let cell = GlobalCell {cellName = s, cellOwner = globalId env, cellRef = ref}
      modifyIORef' (globalPending env) (Map.insert s cell)
      pure cell

-- | For tab completion.
globalNames :: GlobalEnv -> IO [Symbol]
globalNames env = Map.keys <$> readIORef (globalTable env)

-- Ribs

newRib :: IO Rib
newRib = Rib <$> newIORef Map.empty

ribBind :: Rib -> BindKey -> Denotation -> IO ()
ribBind (Rib ref) k d = modifyIORef' ref (Map.insert k d)

ribLookup :: Rib -> BindKey -> IO (Maybe Denotation)
ribLookup (Rib ref) k = Map.lookup k <$> readIORef ref

pushRib :: Rib -> SynEnv -> SynEnv
pushRib rib env = env {synRibs = rib : synRibs env}

topEnv :: GlobalEnv -> SynEnv
topEnv g = SynEnv {synRibs = [], synGlobal = g}

-- Identifiers

isIdent :: Datum -> Bool
isIdent = \case
  DSymbol _ -> True
  DAlias _ -> True
  _ -> False

-- | Symbols are keyed by name, aliases by identity.
identKey :: Datum -> BindKey
identKey = \case
  DSymbol s -> KeySym s
  DAlias a -> KeyAlias (aliasId a)
  d -> KeySym (identBase d)

-- | 'Nothing' means unbound.
resolve :: SynEnv -> Datum -> IO (Maybe Denotation)
resolve env ident = case ident of
  DSymbol s -> do
    local <- lookupRibs (synRibs env) (KeySym s)
    case local of
      Just d -> pure (Just d)
      Nothing -> globalLookup (synGlobal env) s
  DAlias a -> do
    -- If a form from the same expansion bound this alias, that wins.
    -- Otherwise look the original identifier up in the macro's own
    -- environment.
    local <- lookupRibs (synRibs env) (KeyAlias (aliasId a))
    case local of
      Just d -> pure (Just d)
      Nothing -> resolve (aliasEnv a) (aliasIdent a)
  _ -> pure Nothing
  where
    lookupRibs [] _ = pure Nothing
    lookupRibs (r : rs) k =
      ribLookup r k >>= \case
        Just d -> pure (Just d)
        Nothing -> lookupRibs rs k

-- | free-identifier=?. Same binding, or both unbound and spelled the
-- same. Literals such as else are matched with this.
freeIdentEq :: SynEnv -> Datum -> SynEnv -> Datum -> IO Bool
freeIdentEq env1 id1 env2 id2 = do
  d1 <- resolve env1 id1
  d2 <- resolve env2 id2
  pure $ case (d1, d2) of
    (Just a, Just b) -> sameDenotation a b
    (Nothing, Nothing) -> identBase id1 == identBase id2
    _ -> False

-- | Does this identifier still mean the keyword @name@? False once the
-- user has rebound it.
isSpecial :: SynEnv -> Datum -> Symbol -> IO Bool
isSpecial env ident name
  | not (isIdent ident) = pure False
  | otherwise =
      resolve env ident >>= \case
        Just (DenSpecial s) -> pure (s == name)
        _ -> pure False

freshLocal :: Symbol -> IO LocalVar
freshLocal s = do
  i <- freshId
  pure LocalVar {localId = i, localName = s}

-- | One renaming, closed over @env@.
freshAlias :: SynEnv -> Datum -> IO Datum
freshAlias env ident = do
  i <- freshId
  pure (DAlias Alias {aliasId = i, aliasIdent = ident, aliasEnv = env})

-- | Forget the renamings. Needed before quoting anything and for error
-- messages.
stripSyntax :: Datum -> Datum
stripSyntax = \case
  d@(DAlias _) -> DSymbol (identBase d)
  DList xs tl -> DList (map stripSyntax xs) (fmap stripSyntax tl)
  DVector xs -> DVector (map stripSyntax xs)
  DLabel n d -> DLabel n (stripSyntax d)
  d -> d
