-- | The documentation, run. Repl transcripts and Scheme blocks in the
-- Org pages are evaluated and compared with what the page says, the
-- export lists and the index of names in docs/procedures.org are
-- compared with the export lists in "Iota.Library" and with what the
-- interpreter binds the names to, the links between the pages are followed,
-- the files a page includes have to exist, the examples page has to show
-- every example program, the example in the header of the Iota module is
-- compared with the program it copies, and the list of pages in
-- iota.cabal is compared with the pages on disk. A page that drifts from
-- the interpreter fails here.
--
-- The rules, which docs/hacking.org repeats for people writing pages:
--
-- * An example block with a line starting @iota> @ is a repl
--   transcript. Lines before the first prompt are ignored. An input is
--   the text after the prompt plus the following lines that start with
--   the continuation prompt, two spaces, three dots and a space, for as
--   long as the repl would ask for one. The lines up to the next prompt
--   are the expected output. One block is one interpreter. Inputs
--   starting with a colon are repl commands and are not run.
--
-- * A @scheme@ source block containing @; =>@ is run form by form. A top
--   level form followed by @; => EXPECTED@, anywhere before the next
--   form starts, must evaluate to something that writes as EXPECTED. The
--   other forms only have to run without an uncaught error.
--
-- * A line @# doctest: skip@ directly above a block leaves it out.
--
-- The dangerous failure of a check like this is to stop checking
-- without saying so. So whatever looks like it was meant to be tested
-- and does not fit the rules is a failure, not a block that is quietly
-- passed over: a prompt in the wrong column or the wrong case, a result
-- marker with the wrong spacing, a block whose end line is missing.
--
-- The prompts, the rule for what gets echoed and the rule for when an
-- input is complete are taken from "Iota.Repl", so a transcript is held
-- to what the repl does and not to a description of it.
--
-- The scanner is not an Org parser. It knows block markers, level one
-- headings, CUSTOM_ID properties and bracket links, and nothing else,
-- which is all these checks need.
module Test.Docs
  ( tests
  ) where

import Control.Exception (IOException, SomeException, displayException, evaluate, try)
import Control.Monad (filterM, unless)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Char (isDigit, isSpace)
import Data.Either (partitionEithers)
import Data.List (dropWhileEnd, sort)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Iota.Interpreter
import Iota.Library (iotaExtrasExports, libNameText, standardLibraryExports)
import Iota.Printer (writeText)
import Iota.Reader (readDatumsLocated)
import Iota.Repl (continuationPrompt, echoedResult, parenBalance, replPrompt)
import Iota.Types
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode)
import System.FilePath (joinPath, normalise, replaceExtension, splitDirectories, takeDirectory, takeExtension, takeFileName, (</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util (discardErrorOutput, withCommandLine, withSandbox)

-- Blocks

data Kind
  = Example
  | -- | With the language, in lower case.
    Source Text
  deriving stock (Eq, Show)

data Block = Block
  { blockLine :: Int
  -- ^ of the begin marker, counting from 1
  , blockKind :: Kind
  , blockSkip :: Bool
  -- ^ the line above the begin marker was @# doctest: skip@
  , blockBody :: [Text]
  -- ^ with the common indentation removed
  }
  deriving stock (Eq, Show)

-- | A line of a page with its number, counting from 1.
type Numbered = (Int, Text)

-- | The lines of a block with their numbers in the page.
numberedBody :: Block -> [Numbered]
numberedBody b = zip [blockLine b + 1 ..] (blockBody b)

-- | The example and source blocks of a page, in order.
--
-- A page whose markers do not pair up is an error. A block that is never
-- closed would swallow the rest of the page. A block whose end line is
-- missing or misspelled runs on to the end line of the next block of
-- its kind, and the two transcripts merged into one are no longer
-- recognised as either. An end line without a begin line means the begin
-- line was misspelled and the block was never seen. In all three cases
-- tests would vanish without a word.
scanBlocks :: Text -> Either Text [Block]
scanBlocks = outside Nothing . zip [1 ..] . T.lines
  where
    -- Between blocks. The first argument is the line above.
    outside :: Maybe Text -> [Numbered] -> Either Text [Block]
    outside _ [] = Right []
    outside above ((n, line) : rest)
      | Just kind <- beginMarker line = inside (n, line) kind (maybe False isSkipLine above) [] rest
      | isEndMarker Example line || isEndMarker (Source "") line =
          Left (at n <> T.strip line <> " does not end a block. Is the begin line above it missing or misspelled?")
      | otherwise = outside (Just line) rest

    -- In a block, with its lines so far in reverse.
    inside :: Numbered -> Kind -> Bool -> [Text] -> [Numbered] -> Either Text [Block]
    inside (n, begin) _ _ _ [] = Left (at n <> T.strip begin <> " has no matching end line")
    inside open@(n, _) kind skip body ((m, line) : rest)
      | isEndMarker kind line =
          (Block {blockLine = n, blockKind = kind, blockSkip = skip, blockBody = dedent (map unescape (reverse body))} :)
            <$> outside (Just line) rest
      | "#+begin_" `T.isPrefixOf` T.toLower (T.stripStart line) =
          Left (at m <> T.strip line <> " inside the block that starts on line " <> tshow n <> ". Is the end line of that block missing or misspelled?")
      | otherwise = inside open kind skip (line : body) rest

    at n = "line " <> tshow n <> ": "

-- | Markers may be indented and in either case, as Org allows.
beginMarker :: Text -> Maybe Kind
beginMarker line
  | Just rest <- T.stripPrefix "#+begin_example" lower, atWordEnd rest = Just Example
  | Just rest <- T.stripPrefix "#+begin_src" lower, atWordEnd rest = Just (Source (firstWord rest))
  | otherwise = Nothing
  where
    lower = T.toLower (T.stripStart line)
    atWordEnd = maybe True (isSpace . fst) . T.uncons
    firstWord = T.takeWhile (not . isSpace) . T.stripStart

isEndMarker :: Kind -> Text -> Bool
isEndMarker kind line = T.toLower (T.strip line) == end
  where
    end = case kind of
      Example -> "#+end_example"
      Source _ -> "#+end_src"

-- | An Org comment line saying @doctest: skip@.
isSkipLine :: Text -> Bool
isSkipLine line = T.words (T.toLower line) == ["#", "doctest:", "skip"]

-- | Inside a block Org wants a comma in front of a line that would
-- otherwise read as a heading or a keyword. The comma is not content.
unescape :: Text -> Text
unescape line
  | ",*" `T.isPrefixOf` rest || ",#+" `T.isPrefixOf` rest = indent <> T.drop 1 rest
  | otherwise = line
  where
    (indent, rest) = T.span (== ' ') line

-- | Remove the indentation every non-blank line shares.
dedent :: [Text] -> [Text]
dedent ls = map (T.drop common) ls
  where
    common = case [T.length (T.takeWhile (== ' ') l) | l <- ls, not (T.null (T.strip l))] of
      [] -> 0
      widths -> minimum widths

-- Repl transcripts

-- | One input and the lines of output the page shows for it.
data Exchange = Exchange Text [Text]
  deriving stock (Eq, Show)

promptText, continuationText :: Text
promptText = T.pack replPrompt
continuationText = T.pack continuationPrompt

-- | The text after the repl prompt, if the line has one. A prompt with
-- nothing after it may have lost its trailing space to an editor.
promptInput :: Text -> Maybe Text
promptInput line
  | Just rest <- T.stripPrefix promptText line = Just rest
  | T.stripEnd line == T.stripEnd promptText = Just ""
  | otherwise = Nothing

-- | The same for the continuation prompt.
continuationInput :: Text -> Maybe Text
continuationInput line
  | Just rest <- T.stripPrefix continuationText line = Just rest
  | T.stripEnd line == T.stripEnd continuationText = Just ""
  | otherwise = Nothing

-- | A line somebody meant as a prompt, whether or not it is one. The
-- prompt in any case, after any indentation, with anything behind it.
looksLikePrompt :: Text -> Bool
looksLikePrompt line = T.toLower (T.stripEnd promptText) `T.isPrefixOf` T.toLower (T.stripStart line)

-- | An example block is a transcript as soon as one line looks like a
-- prompt. Whether it is a well formed one is for 'transcript' to say.
isTranscript :: Block -> Bool
isTranscript b = blockKind b == Example && any looksLikePrompt (blockBody b)

-- | Split a transcript at the prompts. Whatever comes before the first
-- one, a shell command and the banner as a rule, is dropped.
--
-- Left is what is wrong with the block as a transcript, before anything
-- is run. Every line that looks like a prompt has to be one. An input
-- continues on the next line exactly when the repl would have asked for
-- more, which is while 'parenBalance' is positive, and such a line
-- starts with the continuation prompt. So an input with an open
-- parenthesis and no continuation line is malformed, and so is a
-- continuation line where the repl would not have printed one.
transcript :: [Numbered] -> Either [Text] [Exchange]
transcript ls
  | not (null badPrompts) = Left badPrompts
  | not (null badStructure) = Left badStructure
  | otherwise = Right found
  where
    badPrompts =
      [ malformed n line why
      | (n, line) <- ls
      , looksLikePrompt line
      , isNothing (promptInput line)
      , let why
              | T.stripStart line /= line = "the prompt is indented more than another line of the block. Every line of a transcript starts in the same column"
              | otherwise = "a prompt is written \"" <> promptText <> "\", in lower case with one space after it"
      ]
    (found, badStructure) = go (dropWhile (isNothing . promptInput . snd) ls)

    go :: [Numbered] -> ([Exchange], [Text])
    go [] = ([], [])
    go (prompt@(_, line) : rest) = case promptInput line of
      Nothing -> go rest
      Just start ->
        let (input, afterInput, unfinished)
              -- The repl takes a command as one line whatever is in it.
              | isCommand start = (start, rest, [])
              | otherwise = continue prompt start rest
            (expected, next) = break (isJust . promptInput . snd) afterInput
            stray =
              [ malformed m l "a continuation line, but the input before it is complete, so the repl would not have asked for one"
              | (m, l) <- expected
              , isJust (continuationInput l)
              ]
            (exchanges, problems) = go next
         in (Exchange input (map snd expected) : exchanges, unfinished <> stray <> problems)

    -- Mirrors collect in "Iota.Repl".
    continue :: Numbered -> Text -> [Numbered] -> (Text, [Numbered], [Text])
    continue prompt@(n, line) acc rest
      | parenBalance acc <= 0 = (acc, rest, [])
      | ((_, l) : more) <- rest, Just text <- continuationInput l = continue prompt (acc <> "\n" <> text) more
      | otherwise =
          ( acc
          , rest
          , [malformed n line ("the input has an unclosed parenthesis, so the repl would ask for more, but the next line is not a continuation line. Those start with \"" <> continuationText <> "\"")]
          )

-- | An input the repl treats as one of its own commands.
isCommand :: Text -> Bool
isCommand = (":" `T.isPrefixOf`)

-- | Run a transcript in a fresh interpreter. The result is one report
-- per input whose output differs from the page, or the reasons the
-- block is not a transcript at all. Empty means it passed.
checkTranscript :: [Numbered] -> IO [Text]
checkTranscript ls = case transcript ls of
  Left problems -> pure problems
  Right exchanges -> do
    interp <- newInterpreter
    concat <$> mapM (checkExchange interp) exchanges

checkExchange :: Interpreter -> Exchange -> IO [Text]
checkExchange interp (Exchange input expected)
  -- Commands belong to "Iota.Repl", not to the interpreter.
  | isCommand input = pure []
  | otherwise = do
      actual <- replOutput interp input
      let want = comparable expected
          got = comparable (T.splitOn "\n" actual)
      pure
        [ report [("input", input), ("expected", T.intercalate "\n" want), ("actual", T.intercalate "\n" got)]
        | want /= got
        ]

-- | What the repl would show for an input. Whatever it displayed, then
-- the result if the repl echoes it.
replOutput :: Interpreter -> Text -> IO Text
replOutput interp input = do
  r <- try (captureOutput interp (discardErrorOutput interp (runTextForOutput interp "<repl>" input)))
  pure $ case r of
    Left (e :: ExitCode) -> "<the input left the interpreter: " <> tshow e <> ">"
    Right (result, displayed) -> displayed <> fromMaybe "" (echoedResult result)

-- | Trailing blanks on a line and blank lines at either end do not
-- count. Editors remove the first and the block layout decides the
-- second.
comparable :: [Text] -> [Text]
comparable = dropWhileEnd T.null . dropWhile T.null . map T.stripEnd

-- Scheme blocks

-- | A scheme block is tested as soon as one line has something that
-- looks like a result marker. Whether the markers are well formed is
-- for 'checkScheme' to say.
isSchemeTest :: Block -> Bool
isSchemeTest b = blockKind b == Source "scheme" && any marked (blockBody b)
  where
    marked line = let (near, exact) = lineMarkers line in exact || not (null near)

marker :: Text
marker = "; =>"

-- | What on a line looks like a result marker: a semicolon, blanks or
-- none, and an arrow, @=>@ or @==>@. The Bool says whether the line has
-- a real marker, and the list has the spellings in front of it that
-- are not one. What follows a real marker is the expected value and is
-- not looked at.
lineMarkers :: Text -> ([Text], Bool)
lineMarkers line = case T.breakOn ";" line of
  (_, rest)
    | T.null rest -> ([], False)
    | otherwise ->
        let afterSemicolon = T.drop 1 rest
            (gap, body) = T.span (\c -> c == ' ' || c == '\t') afterSemicolon
            arrow
              | "==>" `T.isPrefixOf` body = Just "==>"
              | "=>" `T.isPrefixOf` body = Just "=>"
              | otherwise = Nothing
         in case arrow of
              Nothing -> lineMarkers afterSemicolon
              Just a
                | ";" <> gap <> a == marker -> ([], True)
                | otherwise ->
                    let (near, exact) = lineMarkers (T.drop (T.length gap + T.length a) afterSemicolon)
                     in ((";" <> gap <> a) : near, exact)

-- | Run a block form by form in a fresh interpreter and compare the
-- marked forms. An uncaught error ends the run, since everything after
-- it would only fail for the same reason. A block with a marker that is
-- nearly one is not run at all. Its form would have gone unchecked.
checkScheme :: [Numbered] -> IO [Text]
checkScheme ls
  | not (null nearMisses) = pure nearMisses
  | otherwise = case readDatumsLocated "<doc>" src of
      Left err -> pure [report [("problem", "the block does not read"), ("reader", err)]]
      Right forms -> do
        interp <- newInterpreter
        run interp (formTexts src (map locatedLoc forms))
  where
    nearMisses =
      [ malformed n line ("\"" <> near <> "\" looks like a result marker. A result marker is written \"" <> marker <> "\", with one space and a single =")
      | (n, line) <- ls
      , near <- fst (lineMarkers line)
      ]
    body = map snd ls
    -- The reader counts a tab as up to eight columns. One space each
    -- keeps its columns equal to character offsets.
    src = T.replace "\t" " " (T.unlines body)
    run _ [] = pure []
    run interp (form : rest) =
      evalForm interp form >>= \case
        Left err -> pure [report [("form", T.strip form), ("problem", "uncaught error, the rest of the block was not run"), ("actual", err)]]
        Right actual -> case expectation form of
          Left problem -> (report [("form", T.strip form), ("problem", problem)] :) <$> run interp rest
          Right (Just expected)
            | expected /= actual ->
                (report [("form", T.strip form), ("expected", expected), ("actual", actual)] :) <$> run interp rest
          Right _ -> run interp rest

-- | The source text of each top level form, from where it starts to
-- where the next one starts, so a trailing comment belongs to the form
-- in front of it.
formTexts :: Text -> [SrcLoc] -> [Text]
formTexts src locs = zipWith slice starts (drop 1 starts <> [T.length src])
  where
    lineStarts = scanl (\acc l -> acc + T.length l + 1) 0 (T.lines src)
    offset loc = sum (take 1 (drop (locLine loc - 1) lineStarts)) + locColumn loc - 1
    starts = map offset locs
    slice a b = T.take (b - a) (T.drop a src)

-- | What the page claims the form evaluates to, if it claims anything.
expectation :: Text -> Either Text (Maybe Text)
expectation form = case T.splitOn marker form of
  [_] -> Right Nothing
  [_, claim] -> Right (Just (T.strip (T.takeWhile (/= '\n') claim)))
  _ -> Left ("more than one \"" <> marker <> "\" in one top level form")

-- | Evaluate one form. Right is the written value, with several values
-- separated by spaces. Left is the message of an uncaught error.
evalForm :: Interpreter -> Text -> IO (Either Text Text)
evalForm interp form = do
  r <- try (captureOutput interp (discardErrorOutput interp (runText interp "<doc>" form)))
  case r of
    Left (e :: ExitCode) -> pure (Left ("the form left the interpreter: " <> tshow e))
    Right (Left err, _) -> pure (Left err)
    Right (Right v, _) -> do
      w <- try (writeText v >>= \t -> evaluate (T.length t `seq` t))
      pure $ case w of
        Left (e :: SomeException) -> Left (T.pack (displayException e))
        Right t -> Right (T.unwords (T.lines t))

-- Reports

-- | Labelled fields, one per line, with the later lines of a value
-- lined up under its first.
report :: [(Text, Text)] -> Text
report = T.intercalate "\n" . concatMap field
  where
    field (label, value) = case T.splitOn "\n" value of
      [] -> []
      (l : ls) -> ("  " <> T.justifyLeft 10 ' ' (label <> ":") <> l) : map (T.replicate 12 " " <>) ls

-- | A line of a page that does not fit the rules, and why.
malformed :: Int -> Text -> Text -> Text
malformed n line why = report [("line", tshow n <> ": " <> T.strip line), ("problem", why)]

tshow :: Show a => a -> Text
tshow = T.pack . show

-- Pages

-- | One test per testable block, named after where the block starts.
blockTest :: FilePath -> Block -> Maybe TestTree
blockTest path b
  | blockSkip b = Nothing
  | isTranscript b = Just (check "repl transcript" checkTranscript)
  | isSchemeTest b = Just (check "scheme block" checkScheme)
  | otherwise = Nothing
  where
    here = path <> ":" <> show (blockLine b)
    -- The repl and iota eval both see ("iota") in (command-line). A block
    -- that asks gets the same, in turn with the other tests that replace
    -- the process arguments. The rest run in parallel.
    asIota
      | any ("command-line" `T.isInfixOf`) (blockBody b) = withCommandLine ["iota"]
      | otherwise = id
    check what run = testCase (here <> " " <> what) $ do
      problems <- asIota (run (numberedBody b))
      unless (null problems) $
        assertFailure (T.unpack (T.intercalate "\n\n" (T.pack (here <> ": " <> what) : problems)))

-- | Source files are UTF-8 whatever the locale says, like the files iota
-- itself reads.
readUtf8 :: FilePath -> IO (Either Text Text)
readUtf8 path = do
  r <- try (BS.readFile path)
  pure $ case r of
    Left (e :: IOException) -> Left ("cannot be read: " <> T.pack (displayException e))
    Right bytes -> first (const "is not valid UTF-8") (TE.decodeUtf8' bytes)

pageTests :: FilePath -> IO TestTree
pageTests path = do
  r <- readUtf8 path
  pure $ testGroup path $ case r >>= \src -> (,) src <$> scanBlocks src of
    Left problem -> [testCase "can be scanned" (assertFailure (path <> ": " <> T.unpack problem))]
    Right (src, blocks) ->
      mapMaybe (blockTest path) blocks
        <> [ testCase (path <> " links") $ do
              broken <- brokenLinks path src blocks
              unless (null broken) $
                assertFailure (T.unpack (T.intercalate "\n" (T.pack (path <> ": links that lead nowhere") : map (describeLink path) broken)))
           ]
        <> [ testCase (path <> " includes") $ do
              broken <- brokenIncludes path src blocks
              unless (null broken) $
                assertFailure (T.unpack (T.intercalate "\n" (T.pack (path <> ": includes that cannot work") : broken)))
           | not (null (includeLines src blocks))
           ]

-- | The pages that are not under docs/.
fixedPages :: [FilePath]
fixedPages = ["README.org", "examples/README.org"]

-- | Every page under docs/. setup.org is the shared export header, not
-- a page.
docsPages :: IO (Either Text [FilePath])
docsPages = do
  r <- try (listDirectory docsDir)
  pure $ case r of
    Left (e :: IOException) -> Left (T.pack (docsDir <> " cannot be listed: " <> displayException e))
    Right names -> Right [docsDir </> n | n <- sort names, takeExtension n == ".org", n /= "setup.org"]

docsDir :: FilePath
docsDir = "docs"

-- Links

-- | The targets of the bracket links on a line, the text between @[[@
-- and the first @]@. A link written as an example, inside verbatim or
-- code markup, has the opening marker right in front of it and is left
-- out.
linkTargets :: Text -> [Text]
linkTargets line = case T.breakOn "[[" line of
  (_, rest) | T.null rest -> []
  (before, rest) ->
    let inside = T.drop 2 rest
        (target, close) = T.break (== ']') inside
        quoted = maybe False ((`elem` ['=', '~']) . snd) (T.unsnoc before)
     in [target | not quoted, not (T.null close), not (T.null target)] <> linkTargets inside

-- | Where a link leads, as far as these checks care.
data Target
  = -- | A file, relative to the page, and the CUSTOM_ID after @::#@.
    ToFile FilePath (Maybe Text)
  | -- | A CUSTOM_ID of the page itself.
    ToId Text
  | -- | A URL, or a kind of link the pages do not use.
    Elsewhere
  deriving stock (Eq, Show)

linkTarget :: Text -> Target
linkTarget t
  | Just rest <- T.stripPrefix "file:" t = toFile rest
  | "./" `T.isPrefixOf` t || "../" `T.isPrefixOf` t = toFile t
  | Just i <- T.stripPrefix "#" t = ToId i
  | otherwise = Elsewhere
  where
    toFile rest =
      let (path, search) = T.breakOn "::" rest
       in ToFile (T.unpack path) (T.stripPrefix "::#" search)

-- | The CUSTOM_ID properties of a page.
customIds :: Text -> Set.Set Text
customIds src =
  Set.fromList
    [ T.strip (T.drop (T.length key) line)
    | line <- map T.strip (T.lines src)
    , key `T.isPrefixOf` T.toLower line
    ]
  where
    key = ":custom_id:"

-- | The Haddock pages are generated into docs/api/ by make docs. They
-- are neither in the repository nor in the source distribution, so
-- links into them are not followed.
intoApi :: FilePath -> Bool
intoApi path = take 2 (splitDirectories (normalise path)) == [docsDir, "api"]

-- | The links of a page that lead nowhere, each with its line, its
-- target and what is wrong. A @file:@ link is relative to the page. Its
-- file has to exist, and if it names a CUSTOM_ID of an Org file, so does
-- that. A @#@ link names a CUSTOM_ID of the page itself. The text of
-- example and source blocks is not markup, so there are no links in it.
brokenLinks :: FilePath -> Text -> [Block] -> IO [(Int, Text, Text)]
brokenLinks path src blocks = concat <$> mapM check links
  where
    literal = Set.fromList (concat [[blockLine b .. blockLine b + length (blockBody b) + 1] | b <- blocks])
    links = [(n, t) | (n, line) <- zip [1 ..] (T.lines src), n `Set.notMember` literal, t <- linkTargets line]
    own = customIds src
    check (n, t) = map (\why -> (n, t, why)) <$> case linkTarget t of
      Elsewhere -> pure []
      ToId i -> pure ["no heading of this page has the CUSTOM_ID " <> i | i `Set.notMember` own]
      ToFile relative search
        | intoApi file -> pure []
        | otherwise -> do
            isFile <- doesFileExist file
            isDirectory <- doesDirectoryExist file
            case search of
              _ | not (isFile || isDirectory) -> pure [T.pack file <> " does not exist"]
              Just i | isFile, takeExtension file == ".org" -> do
                target <- readUtf8 file
                pure $ case target of
                  Left problem -> [T.pack file <> " " <> problem]
                  Right text -> ["no heading of " <> T.pack file <> " has the CUSTOM_ID " <> i | i `Set.notMember` customIds text]
              _ -> pure []
        where
          file = normalise (takeDirectory path </> relative)

describeLink :: FilePath -> (Int, Text, Text) -> Text
describeLink path (n, target, why) = "  " <> T.pack path <> ":" <> tshow n <> ": [[" <> target <> "]]: " <> why

-- Includes

-- | The @#+INCLUDE:@ lines of a page outside its blocks, each with its
-- line number and what follows the keyword.
includeLines :: Text -> [Block] -> [(Int, Text)]
includeLines src blocks =
  [ (n, T.strip (T.drop (T.length keyword) stripped))
  | (n, line) <- zip [1 ..] (T.lines src)
  , n `Set.notMember` literal
  , let stripped = T.stripStart line
  , keyword `T.isPrefixOf` T.toLower stripped
  ]
  where
    keyword = "#+include:"
    literal = Set.fromList (concat [[blockLine b .. blockLine b + length (blockBody b) + 1] | b <- blocks])

-- | The file an include line names: the first thing after the keyword,
-- in double quotes.
includedFile :: Text -> Either Text FilePath
includedFile rest = case T.stripPrefix "\"" rest of
  Just more
    | (file, close) <- T.breakOn "\"" more
    , not (T.null close)
    , not (T.null file) ->
        Right (T.unpack file)
  _ -> Left ("the file name has to come first, in double quotes: " <> rest)

-- | A relative path with its @.@ and @..@ parts worked out, so that
-- @docs/../examples/a@ is @examples/a@.
collapse :: FilePath -> FilePath
collapse = joinPath . reverse . foldl step [] . splitDirectories . normalise
  where
    step (d : ds) ".." | d /= ".." = ds
    step ds "." = ds
    step ds d = d : ds

-- | The files a page includes, relative to the top of the repository.
includedFiles :: FilePath -> Text -> [Block] -> [FilePath]
includedFiles path src blocks =
  [collapse (takeDirectory path </> file) | (_, rest) <- includeLines src blocks, Right file <- [includedFile rest]]

-- | The include lines of a page that Org could not carry out. The file
-- is relative to the page and has to exist.
brokenIncludes :: FilePath -> Text -> [Block] -> IO [Text]
brokenIncludes path src blocks = concat <$> mapM check (includeLines src blocks)
  where
    check (n, rest) = map (\why -> "  " <> T.pack path <> ":" <> tshow n <> ": " <> why) <$> case includedFile rest of
      Left problem -> pure [problem]
      Right relative -> do
        let file = collapse (takeDirectory path </> relative)
        exists <- doesFileExist file
        pure [T.pack file <> " does not exist" | not exists]

-- The examples page

examplesPage, examplesDir, examplesGoldenDir :: FilePath
examplesPage = "docs/examples.org"
examplesDir = "examples"
examplesGoldenDir = "test/golden/examples"

-- | The files the examples page has to show: every program directly
-- under examples/, the recorded output of each, and every library in a
-- directory below examples/.
exampleFiles :: IO [FilePath]
exampleFiles = do
  entries <- sort <$> listDirectory examplesDir
  programs <- filterM (doesFileExist . (examplesDir </>)) [e | e <- entries, takeExtension e == ".iota"]
  directories <- filterM (doesDirectoryExist . (examplesDir </>)) entries
  libraries' <- concat <$> mapM inDirectory directories
  pure $
    [examplesDir </> name | name <- programs]
      <> [examplesGoldenDir </> replaceExtension name ".golden" | name <- programs]
      <> libraries'
  where
    inDirectory d = do
      names <- sort <$> listDirectory (examplesDir </> d)
      pure [examplesDir </> d </> name | name <- names, takeExtension name == ".iota"]

-- | The files that should be on the page and are not included by it.
notIncluded :: [FilePath] -> [FilePath] -> [FilePath]
notIncluded wanted included = [f | f <- wanted, collapse f `notElem` included]

examplesPageTests :: TestTree
examplesPageTests =
  testGroup
    "examples page"
    [ testCase (examplesPage <> " shows every example, its output and the libraries") $ do
        r <- readUtf8 examplesPage
        case r >>= \src -> (,) src <$> scanBlocks src of
          Left problem -> assertFailure (examplesPage <> " " <> T.unpack problem)
          Right (src, blocks) -> do
            wanted <- try exampleFiles
            case wanted of
              Left (e :: IOException) -> assertFailure ("the examples cannot be listed: " <> displayException e)
              Right files -> case notIncluded files (includedFiles examplesPage src blocks) of
                [] -> pure ()
                missing -> assertFailure (unlines ((examplesPage <> " does not include:") : map ("  " <>) missing))
    ]

-- The example of using iota from Haskell

iotaModule, embedExample, embedExpected :: FilePath
iotaModule = "src/Iota.hs"
embedExample = "examples/haskell/Embed.hs"
embedExpected = "examples/haskell/Embed.expected"

-- | The code blocks of the comments in a Haskell source: each run of
-- lines that start with @-- >@, without that marker.
haddockCodeBlocks :: Text -> [[Text]]
haddockCodeBlocks = go . T.lines
  where
    go ls = case dropWhile (isNothing . code) ls of
      [] -> []
      rest -> let (block, more) = span (isJust . code) rest in mapMaybe code block : go more
    code line
      | line == "-- >" = Just ""
      | otherwise = T.stripPrefix "-- > " line

-- | The header of the Iota module shows the example program and what it
-- prints. The program itself is built and run by the test suite
-- iota-embed-example. This holds the copy in the header to the files.
embeddingTests :: TestTree
embeddingTests =
  testGroup
    "embedding example"
    [ testCase (iotaModule <> " shows " <> embedExample) (sameAsBlock 0 embedExample)
    , testCase (iotaModule <> " shows the output in " <> embedExpected) (sameAsBlock 1 embedExpected)
    ]
  where
    sameAsBlock n file = do
      header <- readUtf8 iotaModule
      text <- readUtf8 file
      case (header, text) of
        (Left problem, _) -> assertFailure (iotaModule <> " " <> T.unpack problem)
        (_, Left problem) -> assertFailure (file <> " " <> T.unpack problem)
        (Right h, Right t) -> case drop n (haddockCodeBlocks h) of
          [] -> assertFailure (iotaModule <> " has no code block number " <> show (n + 1 :: Int))
          (block : _) -> block @?= T.lines t

-- The source distribution

cabalFile :: FilePath
cabalFile = "iota.cabal"

-- | The pages that are not under docs/ and belong in the source
-- distribution.
topLevelPages :: [FilePath]
topLevelPages = fixedPages <> ["CHANGELOG.org"]

-- | The entries of a field of a package description: what is left of
-- the field's own line and the indented lines after it, split at blanks
-- and commas, without the comment lines.
cabalField :: Text -> Text -> [Text]
cabalField name src = case dropWhile (not . starts) (T.lines src) of
  [] -> []
  (line : rest) -> concatMap entries (T.drop (T.length name + 1) line : takeWhile continues rest)
  where
    starts line = (T.toLower name <> ":") `T.isPrefixOf` T.toLower line
    continues line = T.null (T.strip line) || maybe False (isSpace . fst) (T.uncons line)
    entries line
      | "--" `T.isPrefixOf` T.stripStart line = []
      | otherwise = filter (not . T.null) (map (T.dropAround (== ',')) (T.words line))

-- | The files an entry stands for. An entry with a star in its file
-- name is a pattern over one directory, as cabal has them.
entryFiles :: Text -> IO [FilePath]
entryFiles entry
  | "*" `T.isInfixOf` name = do
      r <- try (listDirectory dir)
      pure $ case r of
        Left (_ :: IOException) -> []
        Right found -> [normalise (dir </> f) | f <- sort found, prefix `T.isPrefixOf` T.pack f, suffix `T.isSuffixOf` T.pack f]
  | otherwise = pure [normalise path]
  where
    path = T.unpack entry
    dir = takeDirectory path
    name = T.pack (takeFileName path)
    (prefix, star) = T.breakOn "*" name
    suffix = T.drop 1 star

-- | The extra-doc-files of the package, or a failed test.
docFileEntries :: IO [Text]
docFileEntries = do
  r <- readUtf8 cabalFile
  case cabalField "extra-doc-files" <$> r of
    Left problem -> assertFailure (cabalFile <> " " <> T.unpack problem)
    Right [] -> assertFailure (cabalFile <> " has no extra-doc-files field")
    Right entries -> pure entries

-- | The doc tests scan the pages they find, so a page that is not there
-- cannot fail them. The package description is the other list of pages.
-- It decides what goes into the source distribution, where these tests
-- run as well. Holding the two lists against each other catches a page
-- that was deleted or renamed and a new page that was never listed.
distributionTests :: TestTree
distributionTests =
  testGroup
    "source distribution"
    [ testCase "every file in extra-doc-files exists" $ do
        entries <- docFileEntries
        missing <- filterM (fmap null . (\entry -> entryFiles entry >>= filterM doesFileExist)) entries
        unless (null missing) $
          assertFailure (cabalFile <> ": extra-doc-files names files that do not exist: " <> T.unpack (T.unwords missing))
    , testCase "extra-doc-files lists every page and no other Org file" $ do
        entries <- docFileEntries
        listed <- Set.fromList . filter ((== ".org") . takeExtension) . concat <$> mapM entryFiles entries
        onDisk <- docsPagesOnDisk
        let pages = Set.fromList (topLevelPages <> onDisk)
            problems =
              namedFiles "pages that are not in extra-doc-files, and so not in the source distribution" (pages `Set.difference` listed)
                <> namedFiles "Org files in extra-doc-files that are not pages" (listed `Set.difference` pages)
        unless (null problems) $
          assertFailure (T.unpack (T.intercalate "\n  " (T.pack cabalFile <> ":" : problems)))
    ]
  where
    docsPagesOnDisk = do
      r <- try (listDirectory docsDir)
      case r of
        Left (e :: IOException) -> assertFailure (docsDir <> " cannot be listed: " <> displayException e)
        Right names -> pure [docsDir </> n | n <- names, takeExtension n == ".org"]
    namedFiles what = named what . Set.map T.pack

-- The library index

proceduresPage :: FilePath
proceduresPage = "docs/procedures.org"

extrasName :: Text
extrasName = libNameText (LibName ["iota", "extras"])

-- | Every library the interpreter registers at startup, with its exports.
libraries :: [(Text, [Text])]
libraries = [(libNameText name, names) | (name, names) <- standardLibraryExports] <> [(extrasName, iotaExtrasExports)]

-- | The level one headings of a page with the lines under each. Only a
-- star in the first column starts a heading, so an indented export list
-- that begins with @*@ is safe.
sections :: Text -> [(Text, [Text])]
sections = go . T.lines
  where
    go [] = []
    go (line : rest) = case heading line of
      Nothing -> go rest
      Just title ->
        let (body, more) = break (isJust . heading) rest
         in (title, body) : go more
    heading = fmap T.strip . T.stripPrefix "* "

-- | Compare the first example block of a library's section with what
-- the library exports.
exportListProblems :: [Text] -> [Text] -> [Text]
exportListProblems exports body = case scanBlocks (T.unlines body) of
  Left problem -> [problem]
  Right blocks -> case [blockBody b | b <- blocks, blockKind b == Example] of
    [] -> ["there is no #+begin_example block with the export list under the heading"]
    (block : _) ->
      let listed = concatMap T.words block
          want = Set.fromList exports
          have = Set.fromList listed
          twice = Set.fromList [n | (n, m) <- zip (sort listed) (drop 1 (sort listed)), n == m]
       in named "exported by the library but missing from the page" (want `Set.difference` have)
            <> named "on the page but not exported by the library" (have `Set.difference` want)
            <> named "listed more than once" twice

-- | The names that have a description entry, a list item starting
-- @- =(NAME@ with a space or a closing parenthesis after the name.
entryNames :: [Text] -> [Text]
entryNames body =
  [ name
  | line <- body
  , Just rest <- [T.stripPrefix "- =(" (T.stripStart line)]
  , let (name, arguments) = T.break (\c -> c == ' ' || c == ')') rest
  , not (T.null name)
  , not (T.null arguments)
  ]

entryProblems :: [Text] -> [Text] -> [Text]
entryProblems exports body =
  named "exported but without a description entry" (want `Set.difference` have)
    <> named "described but not exported" (have `Set.difference` want)
  where
    want = Set.fromList exports
    have = Set.fromList (entryNames body)

named :: Text -> Set.Set Text -> [Text]
named what names
  | Set.null names = []
  | otherwise = [what <> ": " <> T.unwords (Set.toList names)]

-- | A heading that looks like a library name but is not a library the
-- interpreter has. Left over after a library was renamed or removed.
staleHeadings :: [Text] -> [Text]
staleHeadings titles =
  [ t
  | t <- titles
  , "(" `T.isPrefixOf` t
  , ")" `T.isSuffixOf` t
  , t `notElem` map fst libraries
  ]

-- The index of names

-- | The heading of the index on the procedures page.
indexTitle :: Text
indexTitle = "Index of names"

-- | A library the way the index abbreviates it: @(scheme base)@ is
-- @base@ and @(iota extras)@ is @extras@.
shortLibrary :: Text -> Text
shortLibrary name = case T.words (T.dropAround (\c -> c == '(' || c == ')') name) of
  [] -> name
  parts -> last parts

-- | The names the report calls auxiliary syntax. The interpreter cannot
-- tell them from other syntax, so they are listed here.
auxiliarySyntax :: [Text]
auxiliarySyntax = ["...", "=>", "_", "else", "unquote", "unquote-splicing"]

data IndexRow = IndexRow
  { rowAnchor :: Text
  -- ^ The id of the row's anchor, with its entities resolved.
  , rowName :: Text
  , rowKind :: Text
  , rowLibraries :: [Text]
  , rowWhere :: Text
  }
  deriving stock (Eq, Show)

-- | The rows of the index table. A table line that is not the header,
-- a rule or a well-formed row is a problem, not a line to pass over.
indexRows :: [Text] -> Either [Text] [IndexRow]
indexRows body = case partitionEithers (mapMaybe row body) of
  ([], rows) -> Right rows
  (problems, _) -> Left problems
  where
    row line
      | not ("|" `T.isPrefixOf` l) = Nothing
      | "|-" `T.isPrefixOf` l = Nothing
      | cells == ["Name", "Kind", "Libraries", "Described in"] = Nothing
      | [c1, kind, libs, place] <- cells
      , Just (anchor, name) <- nameCell c1 =
          Just (Right (IndexRow anchor name kind (map T.strip (T.splitOn "," libs)) place))
      | otherwise = Just (Left ("not a row of the index: " <> l))
      where
        l = T.strip line
        cells = map T.strip (dropWhileEnd T.null (drop 1 (T.splitOn "|" l)))
    nameCell c = do
      rest <- T.stripPrefix anchorOpen c
      let (anchor, more) = T.breakOn anchorClose rest
      code <- T.stripPrefix anchorClose more
      name <- verbatim code
      pure (entities anchor, name)
    anchorOpen = "@@html:<a id=\""
    anchorClose = "\"></a>@@ "
    verbatim code = do
      (open, more) <- T.uncons code
      (name, close) <- T.unsnoc more
      if (open == '=' || open == '~') && close == open && not (T.null name) then Just name else Nothing
    entities = T.replace "&amp;" "&" . T.replace "&quot;" "\"" . T.replace "&gt;" ">" . T.replace "&lt;" "<"

-- | Every exported name has one row, the rows are in order, and the
-- anchor of a row is its name.
indexNameProblems :: [Text] -> [IndexRow] -> [Text]
indexNameProblems exported rows =
  named "exported by a library but not in the index" (want `Set.difference` have)
    <> named "in the index but not exported by any library" (have `Set.difference` want)
    <> named "in the index more than once" (Set.fromList [n | (n, m) <- zip (sort listed) (drop 1 (sort listed)), n == m])
    <> ["the rows are not in the order of the names: " <> a <> " comes before " <> b | (a, b) <- zip listed (drop 1 listed), a > b]
    <> [rowName r <> ": the anchor of the row is " <> rowAnchor r | r <- rows, rowAnchor r /= rowName r]
  where
    listed = map rowName rows
    want = Set.fromList exported
    have = Set.fromList listed

-- | The Libraries column against the libraries that export the name,
-- in the order the libraries are given.
indexLibraryProblems :: [(Text, [Text])] -> [IndexRow] -> [Text]
indexLibraryProblems libs rows =
  [ rowName r <> ": the index says " <> T.intercalate ", " (rowLibraries r) <> ", the libraries that export it are " <> T.intercalate ", " want
  | r <- rows
  , let want = [shortLibrary lib | (lib, exports) <- libs, rowName r `elem` exports]
  , not (null want)
  , rowLibraries r /= want
  ]

-- | The kind the index should give a name: a procedure when the
-- interpreter binds it to one, syntax otherwise.
kindOf :: Set.Set Text -> Text -> Text
kindOf procedures name
  | name `Set.member` procedures = "procedure"
  | name `elem` auxiliarySyntax = "auxiliary syntax"
  | otherwise = "syntax"

indexKindProblems :: Set.Set Text -> [IndexRow] -> [Text]
indexKindProblems procedures rows =
  [ rowName r <> ": the index says " <> rowKind r <> ", the interpreter has it as " <> want
  | r <- rows
  , let want = kindOf procedures (rowName r)
  , rowKind r /= want
  ]

-- | The names among these that a new interpreter binds to a procedure.
procedureNames :: [Text] -> IO (Either Text (Set.Set Text))
procedureNames names = do
  interp <- newInterpreter
  r <- evalForm interp ("(list " <> T.unwords (map ask names) <> ")")
  pure $ case r of
    Left err -> Left err
    Right written ->
      let answers = T.words (T.dropAround (\c -> c == '(' || c == ')') written)
       in if length answers == length names
            then Right (Set.fromList [n | (n, "#t") <- zip names answers])
            else Left ("expected one answer for each name, got: " <> written)
  where
    ask name = "(guard (e (#t #f)) (procedure? (eval '" <> name <> " (interaction-environment))))"

-- | The description entries of a section, each with the CUSTOM_ID of
-- the heading it is under.
entryPlaces :: [Text] -> [(Text, Text)]
entryPlaces = go Nothing
  where
    go _ [] = []
    go place (line : rest) = case Set.toList (customIds line) of
      (cid : _) -> go (Just cid) rest
      [] -> [(name, cid) | name <- entryNames [line], Just cid <- [place]] <> go place rest

-- | The Described in column. A name of a standard library has section
-- numbers of the report there. A name that only the extras have links
-- to the part of the page with its entry.
indexPlaceProblems :: Set.Set Text -> [(Text, Text)] -> [IndexRow] -> [Text]
indexPlaceProblems standard places rows = concatMap check rows
  where
    check r
      | rowName r `Set.member` standard =
          [rowName r <> ": expected section numbers of the report, found " <> shown r | not (sectionNumbers (rowWhere r))]
      | otherwise = case lookup (rowName r) places of
          Nothing -> [rowName r <> ": there is no description entry to link to"]
          Just cid ->
            [ rowName r <> ": expected a link to #" <> cid <> ", found " <> shown r
            | not (("[[#" <> cid <> "][") `T.isPrefixOf` rowWhere r)
            ]
    shown r = if T.null (rowWhere r) then "nothing" else rowWhere r
    sectionNumbers t = not (T.null t) && all number (map T.strip (T.splitOn "," t))
    number t = not (T.null t) && all (\part -> not (T.null part) && T.all isDigit part) (T.splitOn "." t)

libraryIndexTests :: IO TestTree
libraryIndexTests = do
  r <- readUtf8 proceduresPage
  pure $ testGroup "library index" $ case r of
    Left problem -> [testCase proceduresPage (assertFailure (proceduresPage <> " " <> T.unpack problem))]
    Right src ->
      let found = sections src
          section name = lookup name found
          inSection name check = case section name of
            Nothing -> failWith name ["there is no level one heading \"* " <> name <> "\""]
            Just body -> failWith name (check body)
          failWith name problems =
            unless (null problems) $
              assertFailure (T.unpack (T.intercalate "\n  " (T.pack proceduresPage <> ", " <> name <> ":" : problems)))
          inIndex check = inSection indexTitle (either id check . indexRows)
       in [ testCase (T.unpack name <> " lists its exports") (inSection name (exportListProblems exports))
          | (name, exports) <- libraries
          ]
            <> [ testCase (T.unpack extrasName <> " describes every export") (inSection extrasName (entryProblems iotaExtrasExports))
               , testCase "every library heading names a library" $
                  failWith "headings" (named "not a library of the interpreter" (Set.fromList (staleHeadings (map fst found))))
               , testCase "the index has every exported name once, in order, under its own anchor" $
                  inIndex (indexNameProblems exported)
               , testCase "the index names the libraries that export each name" $
                  inIndex (indexLibraryProblems libraries)
               , testCase "the index says procedure or syntax as the interpreter has it" $ do
                  asked <- procedureNames exported
                  case asked of
                    Left problem -> failWith indexTitle ["the interpreter could not be asked: " <> problem]
                    Right procedures -> inIndex (indexKindProblems procedures)
               , testCase "the index says where each name is described" $
                  inIndex (indexPlaceProblems standardNames (maybe [] entryPlaces (section extrasName)))
               , testCase "no anchor of the index is also the id of a heading" $
                  failWith indexTitle (named "both a name and a CUSTOM_ID" (Set.fromList exported `Set.intersection` customIds src))
               ]
  where
    exported = Set.toList (Set.fromList (concatMap snd libraries))
    standardNames = Set.fromList (concat [names | (name, names) <- libraries, name /= extrasName])

-- The scanner and the checkers, on fixtures

fixture :: [Text] -> Text
fixture = T.unlines

-- | Fixture lines, numbered as if they were a whole page.
numbered :: [Text] -> [Numbered]
numbered = zip [1 ..]

-- | The line numbers of the problems a malformed block was rejected
-- for, taken from the reports.
problemLines :: [Text] -> [Text]
problemLines = map (T.takeWhile (/= ':') . T.drop (T.length "  line:     "))

scannerTests :: TestTree
scannerTests =
  testGroup
    "scanner"
    [ testCase "finds example and source blocks with their line numbers" $
        scanBlocks
          ( fixture
              [ "* Heading"
              , "  Text."
              , "  #+begin_example"
              , "    iota> 1"
              , "    1"
              , "  #+end_example"
              , "#+BEGIN_SRC scheme"
              , "(+ 1 2) ; => 3"
              , "#+END_SRC"
              , "   #+begin_src sh"
              , "     iota --version"
              , "   #+end_src"
              ]
          )
          @?= Right
            [ Block 3 Example False ["iota> 1", "1"]
            , Block 7 (Source "scheme") False ["(+ 1 2) ; => 3"]
            , Block 10 (Source "sh") False ["iota --version"]
            ]
    , testCase "the lines of a block keep their numbers in the page" $
        fmap (map numberedBody) (scanBlocks (fixture ["text", "#+begin_example", "a", "b", "#+end_example"]))
          @?= Right [[(3, "a"), (4, "b")]]
    , testCase "removes only the indentation every line shares" $
        scanBlocks (fixture ["  #+begin_example", "    a", "", "      b", "    c", "  #+end_example"])
          @?= Right [Block 1 Example False ["a", "", "  b", "c"]]
    , testCase "a skip line directly above the block marks it" $
        fmap (map blockSkip) (scanBlocks (fixture ["# doctest: skip", "#+begin_example", "#+end_example", "  # doctest: skip", "", "#+begin_example", "#+end_example"]))
          @?= Right [True, False]
    , testCase "an escaping comma is not content" $
        fmap (map blockBody) (scanBlocks (fixture ["#+begin_example", ",* + -", "  ,#+begin_src", ", stays", "#+end_example"]))
          @?= Right [["* + -", "  #+begin_src", ", stays"]]
    , testCase "a source block is not closed by an example end line" $
        fmap (map blockBody) (scanBlocks (fixture ["#+begin_src scheme", "#+end_example", "#+end_src"]))
          @?= Right [["#+end_example"]]
    , testCase "a block that is never closed is an error" $
        scanBlocks (fixture ["text", "  #+begin_src scheme", "(+ 1 2)"])
          @?= Left "line 2: #+begin_src scheme has no matching end line"
    , -- Without this the first block would run on to line 7 and neither
      -- transcript would be tested.
      testCase "a block whose end line is missing does not swallow the next one" $
        scanBlocks (fixture ["#+begin_example", "iota> 1", "1", "", "  #+BEGIN_EXAMPLE", "iota> 2", "2", "#+end_example"])
          @?= Left "line 5: #+BEGIN_EXAMPLE inside the block that starts on line 1. Is the end line of that block missing or misspelled?"
    , testCase "a misspelled end line is caught at the next block of any kind" $
        scanBlocks (fixture ["#+begin_example", "iota> 1", "1", "#+end_exmaple", "#+begin_src scheme", "(+ 1 2) ; => 3", "#+end_src"])
          @?= Left "line 5: #+begin_src scheme inside the block that starts on line 1. Is the end line of that block missing or misspelled?"
    , testCase "a misspelled begin line is caught at its end line" $
        scanBlocks (fixture ["#+begin_examples", "iota> 1", "1", "#+end_example"])
          @?= Left "line 4: #+end_example does not end a block. Is the begin line above it missing or misspelled?"
    , testCase "a transcript is split at the prompts" $
        transcript (numbered ["$ iota", "banner", "iota> (define (f x)", "  ...   (* x 2))", "iota> (f 2)", "4", "iota> :help", "ignored", "iota>", "iota> (values 1 2)", "1", "2", ""])
          @?= Right
            [ Exchange "(define (f x)\n  (* x 2))" []
            , Exchange "(f 2)" ["4"]
            , Exchange ":help" ["ignored"]
            , Exchange "" []
            , Exchange "(values 1 2)" ["1", "2", ""]
            ]
    , testCase "an input goes on for as long as the repl would ask for more" $
        transcript (numbered ["iota> (list 1", "  ... 2", "  ...", "  ... \"(\" #\\( ; (", "  ... 3)", "(1 2 \"(\" #\\( 3)"])
          @?= Right [Exchange "(list 1\n2\n\n\"(\" #\\( ; (\n3)" ["(1 2 \"(\" #\\( 3)"]]
    , testCase "a repl command is one line whatever is in it" $
        transcript (numbered ["iota> :ast (a", "(a", "iota> 1", "1"])
          @?= Right [Exchange ":ast (a" ["(a"], Exchange "1" ["1"]]
    , testCase "a prompt indented more than another line is malformed" $
        fmap (const ()) (transcript (numbered ["$ iota", "  iota> (+ 1 2)", "  3"]))
          @?= Left ["  line:     2: iota> (+ 1 2)\n  problem:  the prompt is indented more than another line of the block. Every line of a transcript starts in the same column"]
    , testCase "a prompt without its space, in capitals or before a tab is malformed" $ do
        let rejected ls = either problemLines (const []) (transcript (numbered ls))
        rejected ["iota>(+ 1 2)", "3"] @?= ["1"]
        rejected ["IOTA> (+ 1 2)", "3"] @?= ["1"]
        rejected ["iota>\t(+ 1 2)", "3"] @?= ["1"]
        rejected ["iota> 1", "1", "Iota>2", "2", "iota> 3", "3"] @?= ["3"]
        fmap (const ()) (transcript (numbered ["iota>(+ 1 2)"]))
          @?= Left ["  line:     1: iota>(+ 1 2)\n  problem:  a prompt is written \"iota> \", in lower case with one space after it"]
    , testCase "an unfinished input needs a continuation line" $ do
        let rejected ls = either problemLines (const []) (transcript (numbered ls))
        -- at the next prompt
        rejected ["iota> (define (f x)", "iota> (f 2)", "4"] @?= ["1"]
        -- at the end of the block
        rejected ["iota> 1", "1", "iota> (list 1", "  ... 2"] @?= ["3"]
        -- the continuation prompt has two spaces in front, not three and not none
        rejected ["iota> (list 1", "   ... 2)", "(1 2)"] @?= ["1"]
        rejected ["iota> (list 1", "... 2)", "(1 2)"] @?= ["1"]
        fmap (const ()) (transcript (numbered ["iota> (list 1", "2)"]))
          @?= Left ["  line:     1: iota> (list 1\n  problem:  the input has an unclosed parenthesis, so the repl would ask for more, but the next line is not a continuation line. Those start with \"  ... \""]
    , testCase "a continuation line after a complete input is malformed" $ do
        let rejected ls = either problemLines (const []) (transcript (numbered ls))
        rejected ["iota> (+ 1 2)", "  ... 3", "3"] @?= ["2"]
        rejected ["iota> (list 1", "  ... 2)", "  ... 3", "(1 2)"] @?= ["3"]
        rejected ["iota> 1", "1", "  ... 2"] @?= ["3"]
        fmap (const ()) (transcript (numbered ["iota> 1", "  ... 2"]))
          @?= Left ["  line:     2: ... 2\n  problem:  a continuation line, but the input before it is complete, so the repl would not have asked for one"]
    , testCase "anything that looks like a prompt makes the block a transcript" $
        map (\l -> isTranscript (Block 1 Example False [l])) ["iota> 1", "iota>1", "  IOTA> 1", "iota>", "$ iota", "iota 0.1", "the iota> prompt"]
          @?= [True, True, True, True, False, False, False]
    , testCase "result markers and what only looks like one" $
        map lineMarkers ["(f) ; => 1", "(f) ;; => 1", "(f) ; =>1", "(f)", "(f) ; a comment", "(f) ;=> 1", "(f) ; ==> 1", "(f) ;  => 1", "(f) ;\t=> 1", "(f) ;==> 1", "(f) ; => \";=>\"", "(f) ;=> 1 ; => 2"]
          @?= [ ([], True)
              , ([], True)
              , ([], True)
              , ([], False)
              , ([], False)
              , ([";=>"], False)
              , (["; ==>"], False)
              , ([";  =>"], False)
              , ([";\t=>"], False)
              , ([";==>"], False)
              , ([], True)
              , ([";=>"], True)
              ]
    , testCase "anything that looks like a result marker makes the block a test" $
        map (\l -> isSchemeTest (Block 1 (Source "scheme") False [l])) ["(f) ; => 1", "(f) ;=> 1", "(f) ; ==> 1", "(f) ; no marker", "(cond (x => f))"]
          @?= [True, True, True, False, False]
    , testCase "form texts run from one form to the next" $
        fmap (formTexts src . map locatedLoc) (readDatumsLocated "<t>" src)
          @?= Right ["(define x 1) ; a comment\n", "(list x\n      2) ; => (1 2)\n\n", "x\n"]
    , testCase "an expectation is the rest of the marker's line" $
        map expectation ["(f)", "(f) ; =>  (1 2)  \n", "(f ; => 1\n g) ; => 2"]
          @?= [Right Nothing, Right (Just "(1 2)"), Left "more than one \"; =>\" in one top level form"]
    , testCase "link targets are what is between the brackets" $
        map linkTargets ["see [[file:guide.org][the guide]] and [[#errors][Errors]].", "[[file:a.org::#b]] [[https://example.org/x][x]]", "written =[[file:quoted.org][text]]= or ~[[#quoted]]~", "a [[ that is not closed", "(vector-ref v [[]])", "no link"]
          @?= [["file:guide.org", "#errors"], ["file:a.org::#b", "https://example.org/x"], [], [], [], []]
    , testCase "links are to a file, to an id of the page, or elsewhere" $
        map linkTarget ["file:guide.org", "file:guide.org::#errors", "file:../README.org", "./tutorial.org", "file:guide.org::*Errors", "#errors", "https://example.org", "Errors"]
          @?= [ ToFile "guide.org" Nothing
              , ToFile "guide.org" (Just "errors")
              , ToFile "../README.org" Nothing
              , ToFile "./tutorial.org" Nothing
              , ToFile "guide.org" Nothing
              , ToId "errors"
              , Elsewhere
              , Elsewhere
              ]
    , testCase "custom ids are read from the property drawers" $
        customIds (fixture ["* One", "  :PROPERTIES:", "  :CUSTOM_ID: one", "  :END:", "** Two", ":properties:", ":custom_id:   two-b  ", ":end:", "text :CUSTOM_ID: no"])
          @?= Set.fromList ["one", "two-b"]
    , testCase "links into the generated API pages are not followed" $
        map intoApi ["docs/api/index.html", "./docs/api/Iota.html", "docs/apiary.org", "docs/guide.org", "api/index.html"]
          @?= [True, True, False, False, False]
    , testCase "a field of the package description is its line and the indented ones after it" $
        cabalField
          "extra-doc-files"
          ( fixture
              [ "name: x"
              , "Extra-Doc-Files:    README.org"
              , "                    docs/*.org,"
              , "  -- a comment"
              , ""
              , "                    CHANGELOG.org"
              , "tested-with: GHC == 9.6.7"
              , "extra-source-files: Makefile"
              ]
          )
          @?= ["README.org", "docs/*.org", "CHANGELOG.org"]
    , testCase "a field that is not there has no entries" $
        cabalField "extra-doc-files" (fixture ["name: x", "extra-source-files: Makefile"]) @?= []
    ]
  where
    src = "; leading comment\n(define x 1) ; a comment\n(list x\n      2) ; => (1 2)\n\nx\n"

checkerTests :: TestTree
checkerTests =
  testGroup
    "checkers"
    [ testCase "a correct transcript passes" $ do
        problems <-
          checkTranscript $
            numbered
              [ "$ iota"
              , "iota, an R7RS-small Scheme. Type :help for help."
              , "iota> (define (twice x)"
              , "  ...   (* 2 x))"
              , "iota> (twice 21)"
              , "42   "
              , "iota> (display \"hi\")"
              , "hi"
              , "iota> (display \"dropped\" (current-error-port))"
              , "iota> (begin (display \"a\") (newline) 'b)"
              , "a"
              , "b"
              , "iota> (values 1 2)"
              , "1"
              , "2"
              , "iota> (values)"
              , "iota> (car 1)"
              , "expected a pair, got: 1"
              , "iota> :env twi"
              , "this output is not checked"
              , "iota> \"#<unspecified>\""
              , "\"#<unspecified>\""
              , ""
              ]
        problems @?= []
    , testCase "a wrong transcript is reported with input, expected and actual" $ do
        problems <- checkTranscript (numbered ["iota> (+ 1 2)", "4", "iota> (+ 2 2)", "4"])
        problems @?= ["  input:    (+ 1 2)\n  expected: 4\n  actual:   3"]
    , testCase "output the page leaves out is reported" $ do
        problems <- checkTranscript (numbered ["iota> (+ 1 2)", "iota> (define x 1)", "1"])
        length problems @?= 2
    , testCase "a transcript that leaves the interpreter is reported" $ do
        problems <- checkTranscript (numbered ["iota> (exit 3)"])
        length problems @?= 1
    , testCase "a malformed transcript is reported and not run" $ do
        problems <- checkTranscript (numbered ["iota> (display \"ran\")", "ran", "iota>(car 1)"])
        problemLines problems @?= ["3"]
    , testCase "a correct scheme block passes" $ do
        problems <-
          checkScheme $
            numbered
              [ "(define (square x) (* x x))"
              , "(square 4)            ; => 16"
              , "(map square"
              , "     '(1 2 3))        ; => (1 4 9)"
              , "(display \"not checked\")"
              , "(string-append \"a\" \"b\") ; => \"ab\""
              , "(values 1 2)          ; => 1 2"
              , "(guard (e (#t (error-object-message e)))"
              , "  (error \"boom\" 1))   ; => \"boom\""
              ]
        problems @?= []
    , testCase "a wrong value is reported and the block carries on" $ do
        problems <- checkScheme (numbered ["(+ 1 2) ; => 4", "(+ 2 2) ; => 5"])
        problems
          @?= [ "  form:     (+ 1 2) ; => 4\n  expected: 4\n  actual:   3"
              , "  form:     (+ 2 2) ; => 5\n  expected: 5\n  actual:   4"
              ]
    , testCase "an uncaught error ends the block" $ do
        problems <- checkScheme (numbered ["(car 1)", "(+ 1 2) ; => 4"])
        problems
          @?= ["  form:     (car 1)\n  problem:  uncaught error, the rest of the block was not run\n  actual:   expected a pair, got: 1"]
    , testCase "a block that does not read is reported" $ do
        problems <- checkScheme (numbered ["(+ 1 2 ; => 3"])
        length problems @?= 1
    , testCase "two markers in one form are reported" $ do
        problems <- checkScheme (numbered ["(list 1 ; => 1", "      2) ; => (1 2)"])
        length problems @?= 1
    , testCase "a marker with the wrong spacing or arrow is reported, not skipped" $ do
        -- each of these would pass unnoticed with a wrong value otherwise
        missingSpace <- checkScheme (numbered ["(+ 1 2) ; => 3", "(+ 2 2) ;=> 5"])
        missingSpace
          @?= ["  line:     2: (+ 2 2) ;=> 5\n  problem:  \";=>\" looks like a result marker. A result marker is written \"; =>\", with one space and a single ="]
        longArrow <- checkScheme (numbered ["(+ 2 2) ; ==> 5"])
        problemLines longArrow @?= ["1"]
        twoSpaces <- checkScheme (numbered ["(+ 2 2)", "(+ 2 2) ;  => 5"])
        problemLines twoSpaces @?= ["2"]
    , testCase "links are followed from the page they are on" $ withSandbox "links" $ \dir -> do
        let page = dir </> "a.org"
            write name ls = BS.writeFile (dir </> name) (TE.encodeUtf8 (fixture ls))
            src =
              fixture
                [ "* One"
                , "  :PROPERTIES:"
                , "  :CUSTOM_ID: one"
                , "  :END:"
                , "  [[file:b.org][ok]] [[file:b.org::#there][ok]] [[#one][ok]] [[https://example.org][ok]]"
                , "  [[file:missing.org][gone]] and [[file:b.org::#nowhere][no such id]]"
                , "  [[#two][no such id]] and [[file:sub][a directory]] and [[file:sub/c.txt::#x][not an org file]]"
                , "  #+begin_example"
                , "  [[file:literal.org][not a link]]"
                , "  #+end_example"
                , "  [[./b.org][ok]] [[file:sub/missing.txt][gone]]"
                ]
        write "b.org" ["* There", "  :PROPERTIES:", "  :CUSTOM_ID: there", "  :END:"]
        createDirectory (dir </> "sub")
        write "sub/c.txt" ["text"]
        blocks <- either (assertFailure . T.unpack) pure (scanBlocks src)
        broken <- brokenLinks page src blocks
        [(n, t) | (n, t, _) <- broken]
          @?= [ (6, "file:missing.org")
              , (6, "file:b.org::#nowhere")
              , (7, "#two")
              , (11, "file:sub/missing.txt")
              ]
    , testCase "an export list is compared as a set" $ do
        exportListProblems ["a", "b", "*"] [":PROPERTIES:", ":END:", "  #+begin_example", "    * a", "    b", "  #+end_example"] @?= []
        exportListProblems ["a", "b", "c"] ["#+begin_example", "a d", "a", "#+end_example"]
          @?= [ "exported by the library but missing from the page: b c"
              , "on the page but not exported by the library: d"
              , "listed more than once: a"
              ]
        exportListProblems ["a"] ["no block here"]
          @?= ["there is no #+begin_example block with the export list under the heading"]
    , testCase "description entries are found by their head" $ do
        entryNames ["  - =(fold f acc list)= :: folds", "- =(id x)= :: x", "  - =(print x ...)= ::", "- =(nullary)= :: none", "  - plain item", "- =(broken"]
          @?= ["fold", "id", "print", "nullary"]
        entryProblems ["fold", "id"] ["- =(fold f acc list)= :: folds", "- =(gone x)= :: stale"]
          @?= ["exported but without a description entry: id", "described but not exported: gone"]
    , testCase "sections are the level one headings" $
        sections (fixture ["intro", "* (scheme base)", "  a", "** not a section", "  * neither is this", "* Other  ", "b"])
          @?= [("(scheme base)", ["  a", "** not a section", "  * neither is this"]), ("Other", ["b"])]
    , testCase "a heading that looks like a library must be one" $
        staleHeadings ["(scheme base)", "(scheme gone)", "Introduction", extrasName] @?= ["(scheme gone)"]
    , testCase "the rows of the index are read from its table" $ do
        indexRows indexFixture
          @?= Right
            [ IndexRow "<=" "<=" "procedure" ["base", "r5rs"] "6.2.6"
            , IndexRow "car" "car" "procedure" ["base"] "6.4"
            , IndexRow "id" "id" "procedure" ["extras"] "[[#combinators][Combinators]]"
            , IndexRow "if" "if" "syntax" ["base"] "4.1.5"
            ]
        indexRows ["  | =car= | procedure | base | 6.4 |", "  | @@html:<a id=\"cdr\"></a>@@ cdr | procedure | base | 6.4 |", "  | @@html:<a id=\"x\"></a>@@ =x= | procedure | base |"]
          @?= Left
            [ "not a row of the index: | =car= | procedure | base | 6.4 |"
            , "not a row of the index: | @@html:<a id=\"cdr\"></a>@@ cdr | procedure | base | 6.4 |"
            , "not a row of the index: | @@html:<a id=\"x\"></a>@@ =x= | procedure | base |"
            ]
    , testCase "a correct index passes" $ do
        let rows = either (const []) id (indexRows indexFixture)
        indexNameProblems ["<=", "car", "id", "if"] rows @?= []
        indexLibraryProblems indexLibraries rows @?= []
        indexKindProblems (Set.fromList ["<=", "car", "id"]) rows @?= []
        indexPlaceProblems (Set.fromList ["<=", "car", "if"]) [("id", "combinators")] rows @?= []
    , testCase "names missing from the index, extra, repeated, out of order or under the wrong anchor are reported" $
        indexNameProblems ["a", "b", "c"] [IndexRow "b" "b" "" [] "", IndexRow "a" "a" "" [] "", IndexRow "a" "a" "" [] "", IndexRow "x" "d" "" [] ""]
          @?= [ "exported by a library but not in the index: c"
              , "in the index but not exported by any library: d"
              , "in the index more than once: a"
              , "the rows are not in the order of the names: b comes before a"
              , "d: the anchor of the row is x"
              ]
    , testCase "a wrong list of libraries is reported" $
        indexLibraryProblems indexLibraries [IndexRow "car" "car" "procedure" ["base", "r5rs"] "6.4", IndexRow "<=" "<=" "procedure" ["r5rs", "base"] "6.2.6"]
          @?= [ "car: the index says base, r5rs, the libraries that export it are base"
              , "<=: the index says r5rs, base, the libraries that export it are base, r5rs"
              ]
    , testCase "a wrong kind is reported" $ do
        indexKindProblems (Set.fromList ["car"]) [IndexRow "car" "car" "syntax" [] "", IndexRow "if" "if" "procedure" [] "", IndexRow "else" "else" "syntax" [] ""]
          @?= [ "car: the index says syntax, the interpreter has it as procedure"
              , "if: the index says procedure, the interpreter has it as syntax"
              , "else: the index says syntax, the interpreter has it as auxiliary syntax"
              ]
        map (kindOf Set.empty) auxiliarySyntax @?= replicate (length auxiliarySyntax) "auxiliary syntax"
    , testCase "a missing or misplaced description is reported" $
        indexPlaceProblems
          (Set.fromList ["car", "let"])
          [("id", "combinators")]
          [ IndexRow "car" "car" "" [] ""
          , IndexRow "let" "let" "" [] "4.2.2, 4.2.4"
          , IndexRow "car" "car" "" [] "6.4, see there"
          , IndexRow "id" "id" "" [] "[[#folds][Folds]]"
          , IndexRow "gone" "gone" "" [] "[[#folds][Folds]]"
          ]
          @?= [ "car: expected section numbers of the report, found nothing"
              , "car: expected section numbers of the report, found 6.4, see there"
              , "id: expected a link to #combinators, found [[#folds][Folds]]"
              , "gone: there is no description entry to link to"
              ]
    , testCase "an entry is placed under the nearest heading with an id above it" $
        entryPlaces ["  :CUSTOM_ID: extras", "  - =(early x)= :: e", "** Folds", "   :PROPERTIES:", "   :CUSTOM_ID: folds", "   :END:", "   - =(fold f acc list)= :: f", "   - plain item", "** Lists", "   :CUSTOM_ID: lists", "   - =(first list)= :: f"]
          @?= [("early", "extras"), ("fold", "folds"), ("first", "lists")]
    , testCase "include lines are found outside blocks, and the file is what is in the quotes" $ do
        let page = fixture ["#+INCLUDE: \"../examples/fib.iota\" src scheme", "#+begin_example", "#+INCLUDE: \"not/this.one\"", "#+end_example", "  #+include: \"b.txt\" example", "#+INCLUDE: no-quotes.txt"]
            blocks = either (const []) id (scanBlocks page)
        includeLines page blocks @?= [(1, "\"../examples/fib.iota\" src scheme"), (5, "\"b.txt\" example"), (6, "no-quotes.txt")]
        includedFiles "docs/page.org" page blocks @?= ["examples/fib.iota", "docs/b.txt"]
        includedFile "no-quotes.txt" @?= Left "the file name has to come first, in double quotes: no-quotes.txt"
        includedFile "\"unfinished" @?= Left "the file name has to come first, in double quotes: \"unfinished"
    , testCase "an include of a file that is not there is reported" $ do
        let page = fixture ["#+INCLUDE: \"../iota.cabal\" example", "#+INCLUDE: \"../no-such-file\" example"]
        broken <- brokenIncludes "docs/page.org" page []
        broken @?= ["  docs/page.org:2: no-such-file does not exist"]
    , testCase "a path is compared with its dots worked out" $
        map collapse ["docs/../examples/a.iota", "./docs/./b.org", "../outside", "a/b/../../c", "plain"]
          @?= ["examples/a.iota", "docs/b.org", "../outside", "c", "plain"]
    , testCase "an example the page leaves out is reported" $
        notIncluded ["examples/a.iota", "test/golden/examples/a.golden", "examples/my/lib.iota"] ["examples/a.iota", "examples/my/lib.iota"]
          @?= ["test/golden/examples/a.golden"]
    , testCase "the code blocks of a Haskell comment are the lines marked with -- >" $
        haddockCodeBlocks (fixture ["-- | Text.", "--", "-- > one", "-- >", "-- >   two", "--", "-- More text.", "-- > three", "module M where", "x = 1 -- > not this"])
          @?= [["one", "", "  two"], ["three"]]
    , testCase "a library is abbreviated to the last part of its name" $
        map shortLibrary ["(scheme base)", "(scheme process-context)", extrasName] @?= ["base", "process-context", "extras"]
    ]
  where
    indexFixture =
      [ "  Some text, which is not a row."
      , "  | Name | Kind | Libraries | Described in |"
      , "  |------+------+-----------+--------------|"
      , "  | @@html:<a id=\"&lt;=\"></a>@@ ~<=~ | procedure | base, r5rs | 6.2.6 |"
      , "  | @@html:<a id=\"car\"></a>@@ =car=   | procedure | base       | 6.4   |"
      , "  | @@html:<a id=\"id\"></a>@@ =id=     | procedure | extras     | [[#combinators][Combinators]] |"
      , "  | @@html:<a id=\"if\"></a>@@ =if=     | syntax    | base       | 4.1.5 |"
      ]
    indexLibraries = [("(scheme base)", ["<=", "car", "if"]), ("(scheme r5rs)", ["<="]), (extrasName, ["id"])]

-- | The checks on the pages, then the checks on the checks.
--
-- Everything here gets a time limit. A page can hold a loop that never
-- ends as easily as any program can, and without a limit that is a test
-- run that never ends. The slowest block takes well under a second, so
-- a minute is a hang and not a slow machine.
tests :: IO TestTree
tests = do
  listed <- docsPages
  pages <- mapM pageTests (fixedPages <> either (const []) id listed)
  index <- libraryIndexTests
  pure $
    localOption (mkTimeout (60 * 1000000)) $
      testGroup
        "Documentation"
        [ testGroup "doc tests" (pages <> either (\problem -> [testCase docsDir (assertFailure (T.unpack problem))]) (const []) listed)
        , index
        , examplesPageTests
        , embeddingTests
        , distributionTests
        , scannerTests
        , checkerTests
        ]
