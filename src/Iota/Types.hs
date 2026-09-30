-- The Procedure selectors are only used with RecordWildCards, so the
-- partial-fields warning is not telling me anything.
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | All the data declarations.
--
-- They are in one file because the types refer to each other in a
-- circle. Environments hold values, closures hold code, macros hold
-- syntactic environments, errors hold values. Splitting them up means
-- hs-boot files, and I would rather not. The functions on these types
-- are in "Iota.Value", "Iota.Syntax", "Iota.Printer" and "Iota.Eval".
--
-- The stages, for reference:
--
-- > Text --Reader--> Datum --Expand--> Core --Compile--> Code --run--> Value
--
-- 'Datum' is what the reader produces. 'Core' is the same program after
-- expansion, with every identifier resolved. 'Code' is a closure in
-- continuation passing style.
module Iota.Types
  ( -- * Symbols
    Symbol (..)

    -- * Numbers
  , Number (..)

    -- * Syntax
    -- $datum
  , Datum (..)
  , Alias (..)

    -- * Syntactic environments
  , SynEnv (..)
  , Rib (..)
  , BindKey (..)
  , GlobalEnv (..)
  , GlobalCell (..)
  , LocalVar (..)
  , Denotation (..)
  , Macro (..)
  , sameDenotation

    -- * The core language
  , Core (..)

    -- * Values
  , Value (..)
  , Procedure (..)
  , Arity (..)
  , RecordType (..)
  , Record (..)
  , PromiseBox (..)
  , Port (..)
  , PortState (..)
  , ErrorKind (..)

    -- * Runtime
  , Frame (..)
  , K
  , Code
  , DynState (..)
  , WindFrame (..)
  , Runtime (..)
  , Library (..)
  , LibName (..)

    -- * The evaluator monad
    -- $eval
  , Eval

    -- * Literal constants
  , isMutableId
  , literalId

    -- * Source locations
  , SrcLoc (..)
  , Located (..)
  , showLoc

    -- * Errors
  , SchemeError (..)
  , throwScheme
  , describeArity
  , shallowShow
  ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Ratio (denominator, numerator)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Mutable (IOVector)
import Data.Vector.Unboxed.Mutable qualified as UMV
import Data.Word (Word8)
import System.IO (Handle)

-- Symbols

-- | Symbols are case sensitive, so this is plain Text. The newtype is
-- there to stop me passing a string where a symbol belongs.
newtype Symbol = Symbol {symbolName :: Text}
  deriving newtype (Eq, Ord, IsString)

instance Show Symbol where
  show = T.unpack . symbolName

-- Numbers

-- | Exact rationals, doubles, and complex numbers built from those. An
-- exact integer is a rational with denominator 1. Having one exact
-- constructor means the exactness rules are written once. A Complex
-- always has real parts inside, never another Complex, and its
-- imaginary part is never an exact zero. Iota.Number keeps that true
-- through makeRectangular.
data Number
  = Exact !Rational
  | Inexact !Double
  | Complex !Number !Number
  deriving stock (Eq, Show)

-- Syntax

-- $datum
-- The reader produces a 'Datum', which is immutable. The expander works
-- on the same type and adds 'DAlias' nodes for identifiers that came
-- out of a syntax-rules template. An alias remembers where its macro
-- was defined, and that is the whole hygiene mechanism.
-- 'Iota.Value.datumToValue' turns a datum into a fresh mutable value
-- and resolves datum labels into shared or cyclic structure on the way.

-- | One datum, as read.
data Datum
  = DBool !Bool
  | DNumber !Number
  | DChar !Char
  | DString !Text
  | DSymbol !Symbol
  | -- | @(e1 e2 . tl)@. A proper list has a Nothing tail. The empty
    -- list is @DList [] Nothing@.
    DList [Datum] !(Maybe Datum)
  | DVector [Datum]
  | DBytevector [Word8]
  | -- | @#n=datum@
    DLabel !Int Datum
  | -- | @#n#@
    DLabelRef !Int
  | -- | An identifier a macro expansion renamed.
    DAlias !Alias
  deriving stock (Eq, Show)

-- | A renamed identifier. aliasIdent is what the template said. It can
-- itself be an alias when a macro defines a macro. aliasEnv is where
-- the macro was defined, and that is where the identifier resolves
-- unless a binding form from the same expansion bound the alias.
data Alias = Alias
  { aliasId :: !Int
  , aliasIdent :: Datum
  , aliasEnv :: SynEnv
  }

instance Eq Alias where
  a == b = aliasId a == aliasId b

instance Show Alias where
  show a = show (aliasIdent a)

-- Syntactic environments

-- | Scope as the expander sees it. Local ribs, innermost first, on top
-- of a global table.
data SynEnv = SynEnv
  { synRibs :: [Rib]
  , synGlobal :: GlobalEnv
  }

-- | One lexical contour. Mutable because internal definitions are
-- added to a body's rib as they turn up, and a macro defined earlier in
-- the same body has to see them.
newtype Rib = Rib (IORef (Map BindKey Denotation))

-- | How a rib is keyed. Symbols by name, aliases by identity.
data BindKey
  = KeySym !Symbol
  | KeyAlias !Int
  deriving stock (Eq, Ord, Show)

-- | The global table of a library or a program.
data GlobalEnv = GlobalEnv
  { globalId :: !Int
  , globalName :: !Text
  , globalTable :: !(IORef (Map Symbol Denotation))
  , -- | Cells for names that were referenced before being defined.
    -- See 'Iota.Syntax.pendingCell'.
    globalPending :: !(IORef (Map Symbol GlobalCell))
  }

-- | Where a global variable lives. It holds Undefined until something
-- defines it. cellOwner is the globalId of the environment that made
-- the cell, which is how a program tells its own definitions from
-- imported ones.
data GlobalCell = GlobalCell
  { cellName :: !Symbol
  , cellOwner :: !Int
  , cellRef :: !(IORef Value)
  }

-- | A lexical variable. The compiler turns it into a frame address.
data LocalVar = LocalVar
  { localId :: !Int
  , localName :: !Symbol
  }
  deriving stock (Eq, Show)

-- | What an identifier means.
data Denotation
  = DenGlobal !GlobalCell
  | DenLocal !LocalVar
  | -- | Core or auxiliary syntax such as if, else or the ellipsis.
    DenSpecial !Symbol
  | DenMacro !Macro

-- | free-identifier=? on two resolved identifiers.
sameDenotation :: Denotation -> Denotation -> Bool
sameDenotation a b = case (a, b) of
  (DenGlobal x, DenGlobal y) -> cellRef x == cellRef y
  (DenLocal x, DenLocal y) -> localId x == localId y
  (DenSpecial x, DenSpecial y) -> x == y
  (DenMacro x, DenMacro y) -> macroId x == macroId y
  _ -> False

-- | A @syntax-rules@ transformer.
data Macro = Macro
  { macroId :: !Int
  , macroName :: !Symbol
  , -- | Normally the ellipsis. The four-element syntax-rules form can
    -- pick a different identifier.
    macroEllipsis :: Datum
  , macroLiterals :: [Datum]
  , -- | Pattern and template pairs, in order.
    macroRules :: [(Datum, Datum)]
  , -- | Where the macro was defined. Free template identifiers resolve here.
    macroEnv :: SynEnv
  }

-- The core language

-- | The program after expansion. No macros, no derived forms, and
-- every identifier resolved.
data Core
  = CConst Value
  | CLocalRef !LocalVar
  | CGlobalRef !GlobalCell
  | CSetLocal !LocalVar Core
  | CSetGlobal !GlobalCell Core
  | -- | Top level define. Unlike CSetGlobal the cell may still be unbound.
    CDefineGlobal !GlobalCell Core
  | CIf Core Core Core
  | CLambda
      { lamName :: !Symbol
      , lamParams :: [LocalVar]
      , lamRest :: !(Maybe LocalVar)
      , lamBody :: Core
      }
  | CSeq [Core]
  | CApp Core [Core]
  | -- | let. The initialisers see the enclosing scope.
    CLet [(LocalVar, Core)] Core
  | -- | letrec*. The variables exist, holding Undefined, before any
    -- initialiser runs. Internal definitions become this.
    CLetrec [(LocalVar, Core)] Core

-- Values

-- | A Scheme object.
--
-- Anything the report calls mutable is behind an IORef or in a mutable
-- vector. That way set-car! is visible through every reference and eq?
-- can compare identity. Compound objects carry an id from
-- "Iota.Counter", which equal? and write use to spot sharing. A
-- negative id marks a literal constant that the mutators must refuse.
-- See 'isMutableId'.
data Value
  = -- | @()@
    Null
  | Boolean !Bool
  | Num !Number
  | Character !Char
  | Sym !Symbol
  | -- | A mutable unboxed vector of characters, so string-ref and
    -- string-set! are O(1). The read-only operations copy it out to
    -- Text first, see 'Iota.Value.stringText'.
    Str !Int !(UMV.IOVector Char)
  | Pair !Int !(IORef Value) !(IORef Value)
  | Vec !Int !(IOVector Value)
  | Bytes !Int !(UMV.IOVector Word8)
  | Proc !Procedure
  | PortVal !Port
  | RecordVal !Record
  | -- | The descriptor that define-record-type binds the type name to.
    RecordTypeVal !RecordType
  | -- | A pointer to a box. The extra indirection is so a delay-force
    -- chain can end up sharing a single box, as in R7RS 7.3.
    PromiseVal !(IORef (IORef PromiseBox))
  | -- | What the environment procedure returns.
    EnvVal !GlobalEnv
  | -- | values with any count other than one. A single value is itself.
    MultipleValues [Value]
  | -- | The object from error, and the form the interpreter's own
    -- errors take once Scheme code can catch them.
    ErrObj !ErrorKind !Text [Value]
  | EofObject
  | -- | The result of anything the report leaves unspecified.
    Unspecified
  | -- | The contents of a variable nothing has assigned yet. Reading
    -- it is an error, so user code never gets hold of one.
    Undefined

-- | The error classes that have their own predicate.
data ErrorKind
  = ErrorGeneral
  | ErrorFile
  | ErrorRead
  deriving stock (Eq, Show)

-- | Checked before the body runs, so an arity error can name the
-- procedure.
data Arity
  = Exactly !Int
  | AtLeast !Int
  | Between !Int !Int
  deriving stock (Eq, Show)

-- | Anything that can be applied.
data Procedure
  = -- | Haskell code that returns a value. It may throw a SchemeError.
    -- 'Iota.Eval.applyK' catches that and turns it into a Scheme
    -- condition.
    Primitive
      { primName :: !Text
      , primArity :: !Arity
      , primFn :: [Value] -> Eval Value
      }
  | -- | Haskell code that needs the continuation. apply, call/cc,
    -- dynamic-wind, anything that calls back into Scheme. These must
    -- not throw for Scheme-level errors. Use 'Iota.Eval.raiseError' or
    -- 'Iota.Eval.guardIO' instead.
    PrimitiveK
      { primName :: !Text
      , primArity :: !Arity
      , primFnK :: [Value] -> K -> Eval Value
      }
  | -- | A lambda.
    Closure
      { closureId :: !Int
      , closureName :: !Text
      , closureNParams :: !Int
      , closureHasRest :: !Bool
      , closureBody :: Code
      , closureEnv :: !Frame
      }
  | -- | A captured continuation, with the wind list and handler stack
    -- to reinstate when it is invoked.
    Continuation
      { contId :: !Int
      , contK :: K
      , contDyn :: !DynState
      }
  | -- | Made by make-parameter.
    Parameter
      { paramId :: !Int
      , paramCell :: !(IORef Value)
      , paramConverter :: !(Maybe Value)
      }

-- | Made by define-record-type. The id is why two identical
-- definitions still produce different types.
data RecordType = RecordType
  { rtdId :: !Int
  , rtdName :: !Symbol
  , rtdFields :: [Symbol]
  }

-- | A record instance. Fields are mutable because there may be modifiers.
data Record = Record
  { recordId :: !Int
  , recordType :: !RecordType
  , recordFields :: !(IOVector Value)
  }

-- | Either forced and holding the value, or not and holding the thunk.
data PromiseBox = PromiseBox
  { promiseDone :: !Bool
  , promiseValue :: !Value
  }

-- | A textual or binary port.
data Port = Port
  { portId :: !Int
  , portName :: !Text
  , portTextual :: !Bool
  , portState :: !(IORef PortState)
  }

-- | Ports can be closed, which is why 'Port' keeps its state in an
-- IORef. Every operation reads it again and refuses a ClosedPort.
data PortState
  = -- | A handle and a pushback buffer. The buffer lets read take one
    -- datum at a time instead of reading the whole handle.
    InputHandle !Handle !(IORef Text)
  | OutputHandle !Handle
  | -- | Whatever has not been read yet.
    InputString !(IORef Text)
  | -- | Chunks, most recent first.
    OutputString !(IORef [Text])
  | InputBytes !(IORef ByteString)
  | OutputBytes !(IORef [ByteString])
  | ClosedPort

-- Runtime

-- | Frames of slots, innermost first. A variable is a depth and an index.
data Frame
  = Frame !(IOVector Value) Frame
  | NoFrame

-- | A continuation.
type K = Value -> IO Value

-- | Compiled code. It takes a frame and a continuation.
type Code = Frame -> K -> IO Value

-- | What a continuation captures besides the K. The wind stack and the
-- handler stack, both innermost first.
data DynState = DynState
  { dynWinds :: [WindFrame]
  , dynHandlers :: [Value]
  }

-- | A dynamic-wind that is currently in effect.
data WindFrame = WindFrame
  { windId :: !Int
  , windBefore :: Value
  , windAfter :: Value
  }

-- | A library name such as (scheme base). Numeric parts are kept as text.
newtype LibName = LibName [Text]
  deriving newtype (Eq, Ord)

instance Show LibName where
  show (LibName parts) = "(" <> T.unpack (T.unwords parts) <> ")"

-- | A loaded library. All that matters about it is its export table.
data Library = Library
  { libName :: !LibName
  , libExports :: Map Symbol Denotation
  }

-- | The shared state of one interpreter.
data Runtime = Runtime
  { -- | The current dynamic state. Continuations capture and restore it.
    rtDyn :: !(IORef DynState)
  , -- | The implementation environment. Core syntax, primitives and
    -- the Scheme stdlib in one table. The standard libraries are
    -- views onto it.
    rtImpl :: !GlobalEnv
  , rtLibraries :: !(IORef (Map LibName Library))
  , -- | What interaction-environment returns.
    rtInteraction :: !(IORef (Maybe GlobalEnv))
  , -- | The standard ports, as parameter objects.
    rtStdin :: Value
  , rtStdout :: Value
  , rtStderr :: Value
  , -- | Directories to search for library files.
    rtLibraryPath :: [FilePath]
  , -- | Where the top level form being run came from. For error messages.
    rtLoc :: !(IORef (Maybe SrcLoc))
  }

-- Literal constants

-- | Literals in program text are constants and the mutators refuse
-- them. The flag is the sign of the identity rather than a separate
-- field. 'Iota.Counter.freshId' only ever returns non-negative ones.
isMutableId :: Int -> Bool
isMutableId i = i >= 0

-- | The negative counterpart of a fresh id.
literalId :: Int -> Int
literalId i = negate i - 1

-- Source locations

-- | Where a top level form started.
data SrcLoc = SrcLoc
  { locFile :: !FilePath
  , locLine :: !Int
  , locColumn :: !Int
  }
  deriving stock (Eq, Show)

-- | A datum and where it came from.
data Located = Located
  { locatedLoc :: !SrcLoc
  , locatedDatum :: Datum
  }
  deriving stock (Eq, Show)

-- | @file:line:col@.
showLoc :: SrcLoc -> Text
showLoc loc = T.pack (locFile loc <> ":" <> show (locLine loc) <> ":" <> show (locColumn loc))

-- The evaluator monad

-- $eval
-- Eval is IO. The continuation passing happens in compiled code on top
-- of it. docs\/architecture.org has the reasoning.
type Eval = IO

-- Errors

-- | Everything that can go wrong in the reader, the expander or a
-- primitive.
--
-- These are thrown from Haskell code. The evaluator catches them at
-- the call site and hands the matching condition object to the current
-- Scheme handler. One only reaches the interpreter boundary when no
-- handler is installed.
data SchemeError
  = ParseFailure !Text
  | UnboundVariable !Symbol
  | NotApplicable !Value
  | ArityMismatch !Text !Arity !Int
  | -- | expected, got
    TypeError !Text !Value
  | -- | what, form
    BadSyntax !Text !Value
  | DivideByZero !Text
  | -- | who, index
    IndexOutOfRange !Text !Int
  | FileError !Text
  | PortError !Text
  | MacroError !Text !Datum
  | ImportError !Text
  | -- | @(error message irritant ...)@
    UserError !Text [Value]
  | -- | A raised object that nothing caught.
    RaisedObject !Value

instance Exception SchemeError

instance Show SchemeError where
  show = T.unpack . shallowShow

throwScheme :: SchemeError -> Eval a
throwScheme = throwIO

-- | Wording for arity errors.
describeArity :: Arity -> Text
describeArity = \case
  Exactly n -> "exactly " <> plural n
  AtLeast n -> "at least " <> plural n
  Between lo hi -> "between " <> tshow lo <> " and " <> plural hi
  where
    plural n = tshow n <> (if n == 1 then " argument" else " arguments")
    tshow :: Int -> Text
    tshow = T.pack . show

-- | Pure rendering for the Show instance. It cannot follow IORefs, so
-- compound values print as placeholders. 'Iota.Printer.formatError' is
-- the real one.
shallowShow :: SchemeError -> Text
shallowShow = \case
  ParseFailure t -> "read error:\n" <> t
  UnboundVariable s -> "unbound variable: " <> symbolName s
  NotApplicable v -> "not applicable: " <> brief v
  ArityMismatch who ar n ->
    who <> ": expected " <> describeArity ar <> ", got " <> T.pack (show n)
  TypeError expected v -> "expected " <> expected <> ", got " <> brief v
  BadSyntax what _ -> "bad syntax in " <> what
  DivideByZero who -> who <> ": division by zero"
  IndexOutOfRange who i -> who <> ": index out of range: " <> T.pack (show i)
  FileError t -> "file error: " <> t
  PortError t -> "port error: " <> t
  MacroError t _ -> "syntax error: " <> t
  ImportError t -> "import error: " <> t
  UserError msg _ -> msg
  RaisedObject v -> "uncaught exception: " <> brief v
  where
    brief :: Value -> Text
    brief = \case
      Null -> "()"
      Boolean True -> "#t"
      Boolean False -> "#f"
      Num (Exact r)
        | denominator r == 1 -> T.pack (show (numerator r))
        | otherwise -> T.pack (show (numerator r) <> "/" <> show (denominator r))
      Num (Inexact d) -> T.pack (show d)
      Num n@(Complex _ _) -> T.pack (show n)
      Character c -> T.pack ['#', '\\', c]
      Sym s -> symbolName s
      Str {} -> "#<string>"
      Pair {} -> "#<pair>"
      Vec {} -> "#<vector>"
      Bytes {} -> "#<bytevector>"
      Proc _ -> "#<procedure>"
      PortVal _ -> "#<port>"
      RecordVal _ -> "#<record>"
      RecordTypeVal t -> "#<record-type " <> symbolName (rtdName t) <> ">"
      PromiseVal _ -> "#<promise>"
      EnvVal _ -> "#<environment>"
      MultipleValues _ -> "#<values>"
      ErrObj _ m _ -> "#<error " <> m <> ">"
      EofObject -> "#<eof>"
      Unspecified -> "#<unspecified>"
      Undefined -> "#<undefined>"
