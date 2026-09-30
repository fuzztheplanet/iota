-- | syntax-rules. Match a pattern, fill in a template. Literals, the
-- wildcard, dotted and vector patterns, nested ellipses, a custom
-- ellipsis and the @(... ...)@ escape are all supported.
--
-- Hygiene is done by renaming. Every identifier in the template that is
-- not a pattern variable becomes a fresh alias pointing back at the
-- macro's environment. If the expansion binds that alias, say with a let
-- in the template, the binding is invisible to the caller's code. If it
-- does not, the alias resolves to whatever the name meant where the
-- macro was written. That takes care of capture in both directions.
module Iota.SyntaxRules
  ( makeSyntaxRules
  , expandMacro
  ) where

import Control.Monad (forM)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Iota.Counter (freshId)
import Iota.Syntax
import Iota.Types
import Iota.Value (identBase)

-- Match results

-- | A pattern variable under n ellipses is bound to a Nest n deep. The
-- template side walks the same shape back down.
data Binding
  = Leaf Datum
  | Nest [Binding]

type Bindings = Map BindKey Binding

-- Construction

-- | Take a syntax-rules form apart. The caller has already checked the
-- head. The name is only used in error messages.
makeSyntaxRules :: SynEnv -> Symbol -> Datum -> IO (Either Text Macro)
makeSyntaxRules env name spec = case spec of
  DList (_ : rest) Nothing -> do
    -- An identifier before the literals list is a custom ellipsis, as in
    -- (syntax-rules ::: (lits) rules...).
    let (ellipsis, rest') = case rest of
          (e : more@(_ : _)) | isIdent e -> (e, more)
          _ -> (DSymbol "...", rest)
    case rest' of
      (DList literals Nothing : ruleForms)
        | all isIdent literals -> case mapM asRule ruleForms >>= checkDuplicates ellipsis literals of
            Left err -> pure (Left err)
            Right rules -> do
              i <- freshId
              pure
                ( Right
                    Macro
                      { macroId = i
                      , macroName = name
                      , macroEllipsis = ellipsis
                      , macroLiterals = literals
                      , macroRules = rules
                      , macroEnv = env
                      }
                )
        | otherwise -> pure (Left "syntax-rules: literals must be identifiers")
      _ -> pure (Left "syntax-rules: expected a list of literals")
  _ -> pure (Left "expected a (syntax-rules ...) transformer")
  where
    asRule (DList [pat, tmpl] Nothing) = Right (pat, tmpl)
    asRule _ = Left "syntax-rules: expected (pattern template)"

    -- The same variable twice in one pattern is an error rather than a
    -- silent overwrite.
    checkDuplicates ellipsis literals rules
      | any (dup . fst) rules = Left "syntax-rules: duplicate pattern variable"
      | otherwise = Right rules
      where
        dup pat = let vs = vars pat in length vs /= Set.size (Set.fromList vs)
        vars d
          | isIdent d =
              if identBase d == identBase ellipsis || identBase d == "_" || any (\l -> identKey l == identKey d) literals
                then []
                else [identKey d]
          | otherwise = case d of
              DList ps ptail -> concatMap vars ps <> maybe [] vars ptail
              DVector ps -> concatMap vars ps
              _ -> []

-- Expansion

-- | The first rule that matches wins. @useEnv@ is where the form was
-- written. Literals are compared across the two environments.
expandMacro :: Macro -> SynEnv -> Datum -> IO (Either Text Datum)
expandMacro macro useEnv form = go (macroRules macro)
  where
    ctx =
      Ctx
        { ctxEllipsis = identBase (macroEllipsis macro)
        , ctxLiterals = macroLiterals macro
        , ctxMacroEnv = macroEnv macro
        , ctxUseEnv = useEnv
        }

    go [] = pure (Left ("no matching syntax rule for " <> symbolName (macroName macro)))
    go ((pat, tmpl) : rest) = do
      m <- matchTop pat form
      case m of
        Just bindings -> do
          renames <- newIORef Map.empty
          expandTemplate ctx renames bindings tmpl
        Nothing -> go rest

    -- The keyword is skipped on both sides. The macro may have been
    -- imported under another name.
    matchTop (DList (_ : ps) ptail) (DList (_ : is) itail) =
      matchSeq ctx ps ptail is itail
    matchTop _ _ = pure Nothing

data Ctx = Ctx
  { ctxEllipsis :: Symbol
  , ctxLiterals :: [Datum]
  , ctxMacroEnv :: SynEnv
  , ctxUseEnv :: SynEnv
  }

isEllipsis :: Ctx -> Datum -> Bool
isEllipsis ctx d = isIdent d && identBase d == ctxEllipsis ctx

isLiteral :: Ctx -> Datum -> Bool
isLiteral ctx d = isIdent d && any (\l -> identKey l == identKey d) (ctxLiterals ctx)

isWildcard :: Ctx -> Datum -> Bool
isWildcard ctx d = isIdent d && identBase d == "_" && not (isLiteral ctx d)

-- Pattern matching

match :: Ctx -> Datum -> Datum -> IO (Maybe Bindings)
match ctx pat input
  | isIdent pat =
      if
        | isWildcard ctx pat -> pure (Just Map.empty)
        | isEllipsis ctx pat -> pure Nothing -- a lone ellipsis matches nothing
        | isLiteral ctx pat ->
            if isIdent input
              then do
                same <- freeIdentEq (ctxMacroEnv ctx) pat (ctxUseEnv ctx) input
                pure (if same then Just Map.empty else Nothing)
              else pure Nothing
        | otherwise -> pure (Just (Map.singleton (identKey pat) (Leaf input)))
  | otherwise = case pat of
      DList ps ptail -> case input of
        DList is itail -> matchSeq ctx ps ptail is itail
        _ -> pure Nothing
      DVector ps -> case input of
        DVector is -> matchSeq ctx ps Nothing is Nothing
        _ -> pure Nothing
      -- Numbers and strings in a pattern have to appear verbatim.
      _ -> pure (if pat == input then Just Map.empty else Nothing)

-- | A sequence of patterns against a sequence of forms. At most one
-- ellipsis, possibly a dotted tail.
--
-- The dotted case is the awkward one. @(_ a . rest)@ has to match the
-- proper list @(m 1 2 3)@, so whatever the fixed patterns leave over is
-- built back into a list and handed to the tail pattern.
matchSeq :: Ctx -> [Datum] -> Maybe Datum -> [Datum] -> Maybe Datum -> IO (Maybe Bindings)
matchSeq ctx pats ptail inputs itail =
  case splitEllipsis ctx pats of
    Nothing
      | length inputs >= length pats -> do
          let (used, leftover) = splitAt (length pats) inputs
          parts <- zipWithM' m pats used
          tailBinds <- matchRest leftover
          pure (unions (tailBinds : parts))
      | otherwise -> pure Nothing
    Just (before, repeated, after)
      | length inputs >= length before + length after -> do
          let (beforeIn, restIn) = splitAt (length before) inputs
              (repIn, afterIn) = splitAt (length restIn - length after) restIn
          beforeBinds <- zipWithM' m before beforeIn
          repBinds <- mapM (m repeated) repIn
          afterBinds <- zipWithM' m after afterIn
          tailBinds <- matchRest []
          let vars = patternVars ctx repeated
          pure $ do
            reps <- sequence repBinds
            let nested =
                  Map.fromList
                    [ (v, Nest (map (Map.findWithDefault (Leaf emptyList) v) reps))
                    | v <- Set.toList vars
                    ]
            unions ([Just nested, tailBinds] <> beforeBinds <> afterBinds)
      | otherwise -> pure Nothing
  where
    m = match ctx
    emptyList = DList [] Nothing

    -- The leftover forms plus the input's own tail, if it has one.
    matchRest leftover = case ptail of
      Nothing
        | null leftover && itail == Nothing -> pure (Just Map.empty)
        | otherwise -> pure Nothing
      Just p -> case (leftover, itail) of
        ([], Nothing) -> m p emptyList
        ([], Just i) -> m p i
        _ -> m p (DList leftover itail)

    zipWithM' f xs ys = sequence (zipWith f xs ys)

    unions :: [Maybe Bindings] -> Maybe Bindings
    unions ms = Map.unions <$> sequence ms

-- | The patterns before the ellipsis, the repeated one, and the ones
-- after, if there is an ellipsis at all.
splitEllipsis :: Ctx -> [Datum] -> Maybe ([Datum], Datum, [Datum])
splitEllipsis ctx pats = case break (isEllipsis ctx) pats of
  (before, _ : after)
    | not (null before) -> Just (init before, last before, after)
  _ -> Nothing

patternVars :: Ctx -> Datum -> Set BindKey
patternVars ctx = go
  where
    go d
      | isIdent d =
          if isEllipsis ctx d || isWildcard ctx d || isLiteral ctx d
            then Set.empty
            else Set.singleton (identKey d)
      | otherwise = case d of
          DList ps ptail -> Set.unions (maybe Set.empty go ptail : map go ps)
          DVector ps -> Set.unions (map go ps)
          _ -> Set.empty

-- Template expansion

-- | One table per expansion. The same template identifier has to become
-- the same alias everywhere, or @(let ((tmp x)) tmp)@ would bind one tmp
-- and reference another.
type Renames = IORef (Map BindKey Datum)

expandTemplate :: Ctx -> Renames -> Bindings -> Datum -> IO (Either Text Datum)
expandTemplate ctx renames bindings template
  | isIdent template = case Map.lookup (identKey template) bindings of
      Just (Leaf d) -> pure (Right d)
      Just (Nest _) ->
        pure (Left ("pattern variable " <> symbolName (identBase template) <> " used at the wrong ellipsis depth"))
      Nothing -> Right <$> rename template
  | otherwise = case template of
      -- (... template). Nothing inside is treated as an ellipsis. This is
      -- for macros that define macros.
      DList [e, escaped] Nothing | isEllipsis ctx e -> Right <$> renameAll escaped
      DList ts ttail -> do
        ts' <- expandSeq ctx renames bindings ts
        ttail' <- traverse (expandTemplate ctx renames bindings) ttail
        pure (DList <$> ts' <*> sequenceA ttail')
      DVector ts -> fmap DVector <$> expandSeq ctx renames bindings ts
      other -> pure (Right other)
  where
    rename ident = do
      table <- readIORef renames
      case Map.lookup (identKey ident) table of
        Just a -> pure a
        Nothing -> do
          a <- freshAlias (ctxMacroEnv ctx) ident
          modifyIORef' renames (Map.insert (identKey ident) a)
          pure a
    -- Inside the escape there are no pattern variables but the renaming
    -- still applies.
    renameAll d
      | isIdent d = rename d
      | otherwise = case d of
          DList xs tl -> DList <$> mapM renameAll xs <*> traverse renameAll tl
          DVector xs -> DVector <$> mapM renameAll xs
          other -> pure other

expandSeq :: Ctx -> Renames -> Bindings -> [Datum] -> IO (Either Text [Datum])
expandSeq _ _ _ [] = pure (Right [])
expandSeq ctx renames bindings (t : rest) =
  case span (isEllipsis ctx) rest of
    ([], _) -> do
      d <- expandTemplate ctx renames bindings t
      ds <- expandSeq ctx renames bindings rest
      pure ((:) <$> d <*> ds)
    (ells, rest') -> do
      items <- expandRepeated ctx renames bindings t (length ells)
      ds <- expandSeq ctx renames bindings rest'
      pure ((<>) <$> items <*> ds)

-- | The template t followed by depth ellipses.
expandRepeated :: Ctx -> Renames -> Bindings -> Datum -> Int -> IO (Either Text [Datum])
expandRepeated ctx renames bindings t depth
  | depth <= 0 = fmap (: []) <$> expandTemplate ctx renames bindings t
  | otherwise = case repetitionCount bindings vars t of
      Left err -> pure (Left err)
      Right n -> do
        -- Peel one row off every nested binding per iteration. Indexing
        -- into each list would be quadratic.
        let nested = [(v, xs) | v <- Set.toList vars, Just (Nest xs) <- [Map.lookup v bindings]]
            rowsOf 0 _ = []
            rowsOf i cols = Map.fromList [(v, x) | (v, x : _) <- cols] : rowsOf (i - 1 :: Int) [(v, xs) | (v, _ : xs) <- cols]
        rows <-
          forM (rowsOf n nested) $ \row ->
            expandRepeated ctx renames (Map.union row bindings) t (depth - 1)
        pure (concat <$> sequence rows)
  where
    vars = templateVars t

-- | How many times to repeat. It is the length of the nested bindings the
-- template mentions. If they disagree the macro is zipping lists of
-- different lengths, and that is an error.
repetitionCount :: Bindings -> Set BindKey -> Datum -> Either Text Int
repetitionCount bindings vars _ =
  case counts of
    [] -> Left "no pattern variable to repeat in an ellipsis template"
    (n : ns)
      | all (== n) ns -> Right n
      | otherwise -> Left "ellipsis templates repeat sequences of differing lengths"
  where
    counts =
      mapMaybe
        ( \v -> case Map.lookup v bindings of
            Just (Nest xs) -> Just (length xs)
            _ -> Nothing
        )
        (Set.toList vars)

templateVars :: Datum -> Set BindKey
templateVars d
  | isIdent d = Set.singleton (identKey d)
  | otherwise = case d of
      DList ts ttail -> Set.unions (maybe Set.empty templateVars ttail : map templateVars ts)
      DVector ts -> Set.unions (map templateVars ts)
      _ -> Set.empty
