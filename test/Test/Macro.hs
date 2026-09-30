-- | syntax-rules on its own, with no interpreter around it. The tests are
-- in IO because identifier resolution goes through mutable ribs. Each
-- one builds the small environment it needs and throws it away.
module Test.Macro (tests) where

import Data.Text (Text)
import Iota.Reader (readDatum)
import Iota.Syntax
import Iota.SyntaxRules
import Iota.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | An environment where else is auxiliary syntax and nothing else is bound.
defEnv :: IO SynEnv
defEnv = do
  g <- newGlobalEnv "test"
  globalBind g "else" (DenSpecial "else")
  pure (topEnv g)

parse :: Text -> Datum
parse t = either (error . show) id (readDatum "<test>" t)

-- | Build the macro from the spec, expand the use once and strip the
-- renaming so the result can be compared as plain data.
expandOnce :: Text -> Text -> IO (Either Text Datum)
expandOnce spec use = do
  env <- defEnv
  m <- makeSyntaxRules env "m" (parse spec) >>= either (assertFailure . show) pure
  r <- expandMacro m env (parse use)
  pure (fmap stripSyntax r)

expandsTo :: TestName -> Text -> Text -> Text -> TestTree
expandsTo name spec use expected =
  testCase name $ do
    r <- expandOnce spec use
    r @?= Right (parse expected)

failsWith :: TestName -> Text -> Text -> TestTree
failsWith name spec use =
  testCase name $ do
    r <- expandOnce spec use
    case r of
      Left _ -> pure ()
      Right d -> assertFailure ("unexpectedly expanded to " <> show d)

tests :: TestTree
tests =
  testGroup
    "syntax-rules"
    [ expandsTo "pattern variables" "(syntax-rules () ((_ a b) (b a)))" "(m 1 2)" "(2 1)"
    , expandsTo "ellipsis" "(syntax-rules () ((_ x ...) (list x ...)))" "(m 1 2 3)" "(list 1 2 3)"
    , expandsTo "ellipsis with trailing patterns" "(syntax-rules () ((_ x ... y) (y x ...)))" "(m 1 2 3)" "(3 1 2)"
    , expandsTo "nested ellipsis" "(syntax-rules () ((_ (a b ...) ...) ((a ...) (b ... ...))))" "(m (1 2 3) (4 5))" "((1 4) (2 3 5))"
    , expandsTo "improper pattern" "(syntax-rules () ((_ a . rest) (a rest)))" "(m 1 2 3)" "(1 (2 3))"
    , expandsTo "vector pattern" "(syntax-rules () ((_ #(a b)) (a b)))" "(m #(1 2))" "(1 2)"
    , expandsTo "wildcard" "(syntax-rules () ((_ _ b) b))" "(m 1 2)" "2"
    , expandsTo "literal matched by binding" "(syntax-rules (else) ((_ else x) x))" "(m else 5)" "5"
    , expandsTo "custom ellipsis" "(syntax-rules ::: () ((_ x :::) (list x :::)))" "(m 1 2)" "(list 1 2)"
    , expandsTo "escaped ellipsis" "(syntax-rules () ((_ x) (x (... ...))))" "(m y)" "(y ...)"
    , expandsTo "empty repetition" "(syntax-rules () ((_ x ...) (list x ...)))" "(m)" "(list)"
    , expandsTo "constant in pattern" "(syntax-rules () ((_ 1 x) x))" "(m 1 ok)" "ok"
    , failsWith "no matching rule" "(syntax-rules () ((_ a) a))" "(m 1 2)"
    , failsWith "constant mismatch" "(syntax-rules () ((_ 1 x) x))" "(m 2 ok)"
    , failsWith "differing repetition lengths" "(syntax-rules () ((_ (a ...) (b ...)) ((a b) ...)))" "(m (1 2) (3))"
    , testCase "duplicate pattern variables are rejected" $ do
        env <- defEnv
        r <- makeSyntaxRules env "m" (parse "(syntax-rules () ((_ a a) a))")
        case r of
          Left _ -> pure ()
          Right _ -> assertFailure "accepted a duplicate pattern variable"
    , testCase "introduced identifiers are renamed consistently" $ do
        env <- defEnv
        m <- makeSyntaxRules env "m" (parse "(syntax-rules () ((_ x) (let ((t x)) (if t t #f))))") >>= either (assertFailure . show) pure
        r <- expandMacro m env (parse "(m 1)")
        case r of
          Right (DList [letK, DList [DList [t1, DNumber _] Nothing] Nothing, DList [ifK, t2, t3, DBool False] Nothing] Nothing) -> do
            assertBool "let is renamed" (isAlias letK)
            assertBool "if is renamed" (isAlias ifK)
            assertBool "t is renamed" (isAlias t1)
            assertBool "every t is the same alias" (t1 == t2 && t2 == t3)
            assertBool "t and if are different aliases" (t1 /= ifK)
          other -> assertFailure ("unexpected expansion: " <> show other)
    ]
  where
    isAlias (DAlias _) = True
    isAlias _ = False
