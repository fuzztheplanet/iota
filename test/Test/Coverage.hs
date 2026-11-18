-- | The cases behind rows of docs/r7rs-coverage.org that no other test
-- covered. The page names a group of this module in its Tests column,
-- so each group is a chapter or a section of the report. Most cases are
-- an expression and its written value, as in "Test.Eval". The ones at
-- the end need files, and each gets a directory of its own.
--
-- A case here records what the report asks for, or a choice the page
-- lists under its deviations. What the page lists as a known defect has
-- no case. A test must not hold a wrong answer in place.
module Test.Coverage (tests) where

import Control.Exception (try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Iota.Interpreter (captureOutput, evalText, newInterpreter, runFile, runText)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util (evalCases, withCommandLine, withSandbox)

tests :: TestTree
tests =
  testGroup
    "Coverage"
    [ -- Chapter 2.
      evalCases
        "lexical syntax"
        [ ("#true", "#t")
        , ("(map char->integer (list #\\alarm #\\backspace #\\delete #\\escape #\\null #\\return #\\tab))", "(7 8 127 27 0 13 9)")
        , ("(map char->integer (string->list \"\\a\\b\\t\\r\\\\\"))", "(7 8 9 13 92)")
        , -- a backslash at the end of a line continues the string on the next
          -- one. The string is built here so that the test name stays on one line
          ("(string-length (read (open-input-string (string #\\\" #\\a #\\b #\\\\ #\\newline #\\space #\\space #\\c #\\d #\\\"))))", "4")
        , ("(symbol->string '|a\\x41;b|)", "\"aAb\"")
        , ("(list #d10 #e#x1F (char->integer #\\x10ffff))", "(10 31 1114111)")
        , ("(guard (e ((read-error? e) 'read-error)) (read (open-input-string \"#\\\\xd800\")))", "read-error")
        ]
    , -- Section 3.4. Literals are constants. What read returns is not.
      evalCases
        "literal constants"
        [ ("(bytevector-u8-set! #u8(1 2) 0 9)", "expected a mutable object in bytevector-u8-set!, not a literal constant, got: #u8(1 2)")
        , ("(let ((x `(1 2))) (set-car! x 9) x)", "expected a mutable object in set-car!, not a literal constant, got: (1 2)")
        , ("(let ((x `(1 ,(+ 1 1)))) (set-car! x 9) x)", "(9 2)")
        , ("(let ((v (read (open-input-string \"(1 2)\")))) (set-car! v 9) v)", "(9 2)")
        ]
    , -- Chapter 4.
      evalCases
        "expressions"
        [ ("(set! car cdr)", "bad syntax in set! (cannot assign an imported binding): (set! car cdr)")
        , ("(letrec* ((a 1) (b (+ a 1))) (list a b))", "(1 2)")
        , ("(letrec* ((a b) (b 1)) a)", "unbound variable: b")
        , -- going back into a parameterize body through a continuation puts the
          -- inner value back
          ("(define p (make-parameter 1)) (define k #f) (define n 0) (define seen '()) (parameterize ((p 2)) (call/cc (lambda (c) (set! k c))) (set! seen (cons (p) seen))) (set! seen (cons (p) seen)) (set! n (+ n 1)) (if (< n 2) (k #f) (reverse seen))", "(2 1 2 1)")
        , ("(guard (e ((assq 'a e) => cdr) ((assq 'b e))) (raise (list (cons 'a 42))))", "42")
        , ("(guard (e ((assq 'a e) => cdr) ((assq 'b e))) (raise (list (cons 'b 23))))", "(b . 23)")
        , ("(letrec-syntax ((my-or (syntax-rules () ((_) #f) ((_ e) e) ((_ e r ...) (let ((t e)) (if t t (my-or r ...))))))) (my-or #f 7))", "7")
        , -- with let-syntax the macro cannot see itself
          ("(let-syntax ((my-or (syntax-rules () ((_) #f) ((_ e) e) ((_ e r ...) (let ((t e)) (if t t (my-or r ...))))))) (my-or #f 7))", "unbound variable: my-or")
        , ("(list (promise? (delay 1)) (promise? (make-promise 1)) (promise? 1))", "(#t #t #f)")
        , ("(let ((p (delay 1))) (eq? p (make-promise p)))", "#t")
        , -- a long loop through cond, and, or, when and apply. It shows that they
          -- work there, not that the calls are tail calls: recursion that is not
          -- in tail position gets this deep as well
          ("(define (loop n) (cond ((= n 0) 'done) (else (and #t (or #f (when #t (apply loop (list (- n 1))))))))) (loop 100000)", "done")
        ]
    , -- Chapter 5, and the extensions the page lists for it.
      evalCases
        "program structure"
        [ ("(define ((add a) b) (+ a b)) ((add 1) 2)", "3")
        , ("((named-lambda (f x) (* x 2)) 4)", "8")
        , ("(named-lambda (f x) x)", "#<procedure f>")
        , ("(let () (define x) (set! x 5) x)", "5")
        , -- a program may define a name it imported
          ("(define car cdr) (car '(1 2))", "(2)")
        , ("(define-record-type p #f p?) (p? 1)", "#f")
        , ("(define-record-type p mk p? (x px) (y py)) (let ((r (mk 1 2))) (list (px r) (py r)))", "(1 2)")
        , ("(define-record-type a (mka) a?) (define-record-type b (mkb) b?) (list (a? (mka)) (a? (mkb)))", "(#t #f)")
        , -- the same definition evaluated twice makes two types
          ("(define (make) (define-record-type point (mk) point?) (cons mk point?)) (let ((a (make)) (b (make))) (list ((cdr a) ((car a))) ((cdr b) ((car a)))))", "(#t #f)")
        , ("(eval 'car (environment '(except (scheme base) car)))", "unbound variable: car")
        , ("(eval '(cdr (list 1 2)) (environment '(except (scheme base) car)))", "(2)")
        , ("(eval '(x:car (x:list 1 2)) (environment '(prefix (scheme base) x:)))", "1")
        , -- a library sees what it imports and nothing else
          ("(define-library (e lib) (import (only (scheme base) define)) (export f) (begin (define (f) (car '(1))))) (import (e lib)) (f)", "unbound variable: car")
        , ("(define-library (t lib) (import (scheme base)) (export nope))", "import error: exported name is not defined: nope")
        , ("(define-library (t lib) (import (scheme base)) (cond-expand (iota (export a) (begin (define a 1))) (else (export b) (begin (define b 2))))) (import (t lib)) a", "1")
        , ("(import (only (scheme base) nope))", "import error: import set refers to a name the library does not export: nope")
        ]
    , -- Section 6.1.
      evalCases
        "equivalence"
        [ ("(list (eqv? (vector) (vector)) (eqv? \"\" \"\") (eqv? '() '()) (eqv? 1/2 1/2) (equal? #u8(1 2) #u8(1 2)) (equal? 2 2.0))", "(#f #f #t #t #t #f)")
        ]
    , -- Section 6.2.
      evalCases
        "numbers"
        [ ("(list (floor 2.5) (floor -5/2) (ceiling 2.5) (ceiling -5/2) (truncate -2.5) (truncate 5/2))", "(2.0 -3 3.0 -2 -2.0 2)")
        , ("(list (floor-remainder -7 2) (truncate-quotient -7 2) (truncate-remainder -7 2))", "(1 -3 -1)")
        , ("(list (gcd 12 18) (gcd) (gcd 0 5) (lcm 4 6) (lcm) (lcm 3 0))", "(6 0 5 12 1 0)")
        , ("(list (numerator 6/4) (denominator 6/4) (numerator 0.5) (denominator 0.5))", "(3 2 1.0 2.0)")
        , ("(list (min 3 1 2) (min 1 2.0) (abs -5) (abs -1/2) (abs -2.5))", "(1 1.0 5 1/2 2.5)")
        , ("(list (positive? 1) (positive? -1) (negative? -1) (negative? 0))", "(#t #f #t #f)")
        , ("(list (rational? 1/2) (rational? 0.5) (rational? +inf.0) (exact-integer? 5) (exact-integer? 5.0))", "(#t #t #f #t #f)")
        , ("(exact-integer-sqrt 17)", "4\n1")
        , -- only values every libm agrees on
          ("(list (exp 0) (log 1) (sin 0) (cos 0) (tan 0) (asin 0) (acos 1) (atan 0) (atan 1 1))", "(1.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.7853981633974483)")
        , ("(exact (round (log 8 2)))", "3")
        , ("(list (log -1) (real? (asin 2)))", "(0.0+3.141592653589793i #f)")
        , ("(inexact->exact 0.25)", "1/4")
        , -- positional from 1e-6 up to, not including, 1e21
          ("(list 1e-6 9.99e-7 1e20 1e21)", "(0.000001 9.99e-7 100000000000000000000.0 1.0e21)")
        ]
    , -- Sections 6.3 and 6.5.
      evalCases
        "booleans and symbols"
        [ ("(list (boolean? #f) (boolean? 0) (boolean? '()) (boolean=? #t #t) (boolean=? #t #f) (boolean=? #f #f #f))", "(#t #f #f #t #f #t)")
        , ("(list (symbol=? 'a 'a) (symbol=? 'a 'b) (symbol=? 'a 'a 'a))", "(#t #f #t)")
        ]
    , -- Section 6.4, with every accessor of (scheme cxr).
      evalCases
        "pairs and lists"
        [ ("(list (list-tail '(a b c d) 2) (list-copy '(1 2 3)) (memv 2 '(1 2 3)) (memv 9 '(1 2 3)))", "((c d) (1 2 3) (2 3) #f)")
        , ("(let* ((a (list 1 2)) (b (list-copy a))) (set-car! b 9) (list a b))", "((1 2) (9 2))")
        , ("(let ((t '((a b) (c d) e f))) (list (caar t) (cadr t) (cdar t) (cddr t) (caadr t) (cadar t) (caddr t) (cdadr t) (cddar t) (cdddr t) (cadddr t) (cddddr t)))", "(a (c d) (b) (e f) c b e (d) () (f) f ())")
        , ("(let ((t '(((1 2) 3) 4))) (list (caaar t) (cdaar t) (cadaar t) (cddaar t)))", "(1 (2) 2 ())")
        , ("(let ((t '((((1) 2) 3) (4 (5)) 6 7 8))) (list (caaaar t) (caaadr '(1 ((2)))) (caadar '((1 (2)))) (caaddr '(1 2 (3))) (cadadr t) (caddar '((1 2 3))) (cdaaar t) (cdaadr '(1 ((2) 3))) (cdadar '((1 (2 3)))) (cdaddr '(1 2 (3 4))) (cddadr '(1 (2 3 4))) (cdddar '((1 2 3 4)))))", "(1 2 2 3 (5) 3 () () (3) (4) (4) (4))")
        ]
    , -- Sections 6.6 and 6.7.
      evalCases
        "characters and strings"
        [ ("(list (char? #\\a) (char? \"a\") (char=? #\\a #\\a) (char<=? #\\a #\\a #\\b) (char>? #\\b #\\a) (char>=? #\\a #\\b))", "(#t #f #t #t #t #f)")
        , ("(list (char-ci=? #\\a #\\A) (char-ci<? #\\a #\\B) (char-ci<=? #\\B #\\a) (char-ci>? #\\B #\\a) (char-ci>=? #\\a #\\A))", "(#t #t #f #t #t)")
        , ("(list (char-alphabetic? #\\a) (char-alphabetic? #\\1) (char-whitespace? #\\space) (char-whitespace? #\\a) (char-upper-case? #\\A) (char-lower-case? #\\A) (char-downcase #\\A))", "(#t #f #t #f #t #f #\\a)")
        , ("(list (string<? \"abc\" \"abd\") (string<? \"abc\" \"ab\") (string<=? \"a\" \"a\" \"b\") (string>? \"b\" \"a\") (string>=? \"a\" \"b\"))", "(#t #f #t #t #f)")
        , ("(list (string-ci<? \"abc\" \"ABD\") (string-ci<=? \"ABC\" \"abc\") (string-ci>? \"b\" \"A\") (string-ci>=? \"a\" \"B\") (string-downcase \"AbC\"))", "(#t #t #t #f \"abc\")")
        , ("(let ((s (make-string 4 #\\a))) (string-fill! s #\\z 1 3) s)", "\"azza\"")
        , -- copying inside one string, to the right and to the left
          ("(let ((s (string-copy \"abcde\"))) (string-copy! s 1 s 0 3) s)", "\"aabce\"")
        , ("(let ((s (string-copy \"abcde\"))) (string-copy! s 0 s 1 4) s)", "\"bcdde\"")
        ]
    , -- Sections 6.8 and 6.9.
      evalCases
        "vectors and bytevectors"
        [ ("(list (vector? #(1)) (vector? '(1)) (vector->string #(#\\a #\\b #\\c) 1) (vector-append #(1) #(2 3)) (vector-copy #(1 2 3) 1) (vector-copy #(1 2 3) 0 2))", "(#t #f \"bc\" #(1 2 3) #(2 3) #(1 2))")
        , ("(let ((v (vector 1 2 3 4))) (vector-fill! v 0 1 3) v)", "#(1 0 0 4)")
        , ("(list (bytevector? #u8(1)) (bytevector? #(1)) (bytevector-length #u8(1 2 3)) (bytevector-append #u8(1) #u8(2 3)) (bytevector-copy #u8(1 2 3) 1))", "(#t #f 3 #u8(1 2 3) #u8(2 3))")
        , ("(let ((b (bytevector 1 2 3))) (bytevector-u8-set! b 1 255) b)", "#u8(1 255 3)")
        ]
    , -- Section 6.10.
      evalCases
        "control"
        [ ("(call-with-current-continuation (lambda (k) (+ 1 (k 42))))", "42")
        , ("(let ((acc '())) (string-for-each (lambda (a b) (set! acc (cons (list a b) acc))) \"ab\" \"xyz\") acc)", "((#\\b #\\y) (#\\a #\\x))")
        , ("(let ((acc '())) (vector-for-each (lambda (x) (set! acc (cons x acc))) #(1 2 3)) acc)", "(3 2 1)")
        , ("(string-map (lambda (a b) b) \"abc\" \"xy\")", "\"xy\"")
        , -- several sequences, and the shortest one decides
          ("(let ((acc '())) (for-each (lambda (a b) (set! acc (cons (+ a b) acc))) '(1 2 3) '(10 20)) acc)", "(22 11)")
        , ("(let ((acc '())) (vector-for-each (lambda (a b) (set! acc (cons (+ a b) acc))) #(1 2 3) #(10 20)) acc)", "(22 11)")
        , ("(vector-map + #(1 2 3) #(10 20))", "#(11 22)")
        ]
    , -- Section 6.12 and the (scheme r5rs) library. Of null-environment only what it
      -- gets right, see the known defects.
      evalCases
        "environments"
        [ ("(eval '(if #t 1 2) (null-environment 5))", "1")
        , ("(eval '(car (list 1 2)) (environment '(scheme r5rs)))", "1")
        , ("(define-library (r lib) (import (scheme r5rs)) (export half) (begin (define (half x) (exact->inexact (/ x 2))))) (import (r lib)) (half 1)", "0.5")
        , -- syntax-rules is among the exports of (scheme r5rs), so a macro can be
          -- written with nothing else imported
          ("(define-library (r mac) (import (scheme r5rs)) (export twice-21) (begin (define-syntax twice (syntax-rules () ((_ x) (* 2 x)))) (define (twice-21) (twice 21)))) (import (r mac)) (twice-21)", "42")
        , ("(eval '(let-syntax ((add1 (syntax-rules () ((_ x) (+ x 1))))) (add1 41)) (scheme-report-environment 5))", "42")
        ]
    , -- Section 6.13, on string and bytevector ports. The file ports are further down.
      evalCases
        "ports"
        [ ("(let ((p (open-input-string \"ab\"))) (list (peek-char p) (read-char p) (read-char p) (eof-object? (peek-char p)) (char-ready? (open-input-string \"x\"))))", "(#\\a #\\a #\\b #t #t)")
        , ("(list (eof-object? (eof-object)) (eof-object))", "(#t #<eof>)")
        , ("(let ((p (open-output-string))) (write-char #\\a p) (write-char #\\b p) (get-output-string p))", "\"ab\"")
        , ("(let ((p (open-output-string))) (let ((x (list 1))) (write-simple (list x x \"s\" #\\c) p)) (get-output-string p))", "\"((1) (1) \\\"s\\\" #\\\\c)\"")
        , -- the kind of a port, asked while it is open
          ("(let ((i (open-input-string \"x\")) (o (open-output-string))) (list (input-port? i) (input-port? o) (output-port? o) (output-port? i) (textual-port? i) (textual-port? (open-input-bytevector #u8())) (input-port? 5)))", "(#t #f #t #f #t #f #f)")
        , -- port? is true of every port, open or closed, and of nothing else
          ("(let ((c (open-input-string \"x\"))) (close-port c) (list (port? (current-input-port)) (port? (open-output-string)) (port? (open-input-bytevector (bytevector 1))) (port? c) (port? 5) (port? \"x\")))", "(#t #t #t #t #f #f)")
        , ("(define-library (p lib) (import (scheme base)) (export is-port) (begin (define (is-port x) (port? x)))) (import (p lib)) (is-port (current-output-port))", "#t")
        , ("(let ((i (open-input-string \"x\")) (o (open-output-string))) (close-input-port i) (close-output-port o) (list (input-port-open? i) (output-port-open? o) (output-port-open? (open-output-string))))", "(#f #f #t)")
        , ("(let ((p (open-input-string \"abc\"))) (list (call-with-port p read-char) (input-port-open? p)))", "(#\\a #f)")
        , ("(let ((p (open-output-string))) (parameterize ((current-error-port p)) (display \"e\" (current-error-port))) (get-output-string p))", "\"e\"")
        , ("(let ((p (open-output-string))) (display \"x\" p) (flush-output-port p) (get-output-string p))", "\"x\"")
        ]
    , systemInterface
    , files
    ]

-- | Section 6.14, apart from the files.
systemInterface :: TestTree
systemInterface =
  testGroup
    "system interface"
    [ testCase "command-line returns the arguments of the process" $ do
        out <- withCommandLine ["prog.iota", "a", "b"] (evalText "(command-line)")
        out @?= "(\"prog.iota\" \"a\" \"b\")"
    , -- Nothing here depends on what the environment holds. A name with an
      -- equals sign in it cannot be set, and whatever is set has to come
      -- back the same from both procedures. A name can be in the environment
      -- twice: cabal test passes iota_datadir two times, with different
      -- values. get-environment-variable then returns the first, which is
      -- also the one assoc finds in the list.
      testCase "get-environment-variable agrees with get-environment-variables" $ do
        out <-
          evalText
            "(let ((vars (get-environment-variables))) (list (get-environment-variable \"A=B\") (list? vars) (every (lambda (p) (equal? (get-environment-variable (car p)) (cdr (assoc (car p) vars)))) vars)))"
        out @?= "(#f #t #t)"
    , testCase "exit turns #t, #f and no argument into a status" $ do
        exitStatus "(exit #t)" >>= (@?= Just ExitSuccess)
        exitStatus "(exit #f)" >>= (@?= Just (ExitFailure 1))
        exitStatus "(exit)" >>= (@?= Just ExitSuccess)
        exitStatus "(+ 1 2)" >>= (@?= Nothing)
    ]

-- | The status a program exits with, if it calls exit at all.
exitStatus :: Text -> IO (Maybe ExitCode)
exitStatus src = do
  interp <- newInterpreter
  r <- try @ExitCode (runText interp "<t>" src)
  pure (either Just (const Nothing) r)

-- | Write the source to the path and run it as a script. Right is what
-- it displayed, Left the error.
runProgram :: FilePath -> Text -> IO (Either Text Text)
runProgram path src = do
  TIO.writeFile path src
  interp <- newInterpreter
  (r, out) <- captureOutput interp (runFile interp path)
  pure (out <$ r)

-- | A path as a Scheme string literal. The sandbox paths have nothing in
-- them that would need an escape.
quoted :: FilePath -> Text
quoted path = "\"" <> T.pack path <> "\""

-- | include-ci, the library declarations that read files, load and the
-- procedures of (scheme file).
files :: TestTree
files =
  testGroup
    "files"
    [ testCase "include-ci folds identifiers and leaves strings and characters alone" $ withSandbox "include-ci" $ \dir -> do
        TIO.writeFile (dir </> "inc.iota") "(define FOO \"ABC\")\n(define Ch #\\A)\n"
        r <- runProgram (dir </> "main.iota") "(import (scheme base) (scheme write))\n(include-ci \"inc.iota\")\n(write (list foo ch))\n"
        r @?= Right "(\"ABC\" #\\A)"
    , testCase "a library file can include, include-ci and include-library-declarations" $ withSandbox "declarations" $ \dir -> do
        createDirectoryIfMissing True (dir </> "t")
        TIO.writeFile (dir </> "t" </> "decls.iota") "(export from-decls from-include from-ci from-cond)\n(begin (define from-decls 1))\n"
        TIO.writeFile (dir </> "t" </> "body.iota") "(define from-include 2)\n"
        TIO.writeFile (dir </> "t" </> "body-ci.iota") "(define FROM-CI 3)\n"
        TIO.writeFile
          (dir </> "t" </> "lib.iota")
          "(define-library (t lib)\n  (import (scheme base))\n  (include-library-declarations \"decls.iota\")\n  (include \"body.iota\")\n  (include-ci \"body-ci.iota\")\n  (cond-expand (iota (begin (define from-cond 'iota))) (else (begin (define from-cond 'other)))))\n"
        r <- runProgram (dir </> "main.iota") "(import (scheme base) (scheme write) (t lib))\n(write (list from-decls from-include from-ci from-cond))\n"
        r @?= Right "(1 2 3 iota)"
    , testCase "load runs a file in the interaction environment" $ withSandbox "load" $ \dir -> do
        TIO.writeFile (dir </> "defs.iota") "(define loaded 41)\n(set! loaded (+ loaded 1))\n"
        out <- evalText ("(load " <> quoted (dir </> "defs.iota") <> ") loaded")
        out @?= "42"
    , testCase "load takes an environment" $ withSandbox "load-environment" $ \dir -> do
        TIO.writeFile (dir </> "defs.iota") "(define loaded 41)\n(set! loaded (+ loaded 1))\n"
        out <-
          evalText $
            "(define e (environment '(scheme base))) (load " <> quoted (dir </> "defs.iota") <> " e) (list (eval 'loaded e) (guard (x (#t 'unbound)) loaded))"
        out @?= "(42 unbound)"
    , testCase "an import in a loaded file is looked up next to that file" $ withSandbox "load-import" $ \dir -> do
        createDirectoryIfMissing True (dir </> "sub" </> "my")
        TIO.writeFile (dir </> "sub" </> "my" </> "util.iota") "(define-library (my util) (import (scheme base)) (export twice) (begin (define (twice x) (* 2 x))))\n"
        TIO.writeFile (dir </> "sub" </> "main.iota") "(import (my util))\n(define result (twice 21))\n"
        out <- evalText ("(load " <> quoted (dir </> "sub" </> "main.iota") <> ") result")
        out @?= "42"
    , testCase "call-with-output-file, call-with-input-file, file-exists? and delete-file" $ withSandbox "file-procedures" $ \dir -> do
        out <-
          evalText $
            "(define f " <> quoted (dir </> "data.txt") <> ") (call-with-output-file f (lambda (p) (write '(1 \"two\" #\\3) p))) (define a (list (file-exists? f) (call-with-input-file f read))) (delete-file f) (list a (file-exists? f) (guard (e ((file-error? e) 'file-error)) (delete-file f)))"
        out @?= "((#t (1 \"two\" #\\3)) #f file-error)"
    , testCase "with-output-to-file and with-input-from-file redirect the current ports" $ withSandbox "redirect" $ \dir -> do
        out <-
          evalText $
            "(define f " <> quoted (dir </> "data.txt") <> ") (with-output-to-file f (lambda () (display \"hi\") (newline) (display \"there\"))) (with-input-from-file f (lambda () (list (read-line) (read-line) (eof-object? (read-line)))))"
        out @?= "(\"hi\" \"there\" #t)"
    , testCase "open-output-file and open-input-file, closed by hand" $ withSandbox "open-close" $ \dir -> do
        out <-
          evalText $
            "(define f " <> quoted (dir </> "data.txt") <> ") (define o (open-output-file f)) (write-string \"abc\" o) (close-output-port o) (define i (open-input-file f)) (define s (read-string 10 i)) (close-input-port i) (list s (output-port-open? o) (input-port-open? i))"
        out @?= "(\"abc\" #f #f)"
    , testCase "binary file ports" $ withSandbox "binary" $ \dir -> do
        out <-
          evalText $
            "(define f " <> quoted (dir </> "data.bin") <> ") (define o (open-binary-output-file f)) (write-bytevector (bytevector 1 2 255) o) (close-port o) (define i (open-binary-input-file f)) (define b (read-bytevector 10 i)) (close-port i) (list b (binary-port? i) (textual-port? i))"
        out @?= "(#u8(1 2 255) #t #f)"
    ]
