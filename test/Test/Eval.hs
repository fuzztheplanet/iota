-- | The evaluator, feature by feature. An expression in, the expected
-- write output out, with a fresh interpreter each time.
module Test.Eval (tests) where

import Test.Tasty
import Test.Util (evalCases)

tests :: TestTree
tests =
  testGroup
    "Eval"
    [ evalCases
        "self-evaluation and quote"
        [ ("42", "42")
        , ("#t", "#t")
        , ("\"hi\"", "\"hi\"")
        , ("'foo", "foo")
        , ("'(1 2 3)", "(1 2 3)")
        , ("''x", "'x")
        , ("(quote (a . b))", "(a . b)")
        ]
    , evalCases
        "arithmetic"
        [ ("(+ 1 2 3)", "6")
        , ("(- 10 1 2)", "7")
        , ("(- 5)", "-5")
        , ("(* 2 3 4)", "24")
        , ("(/ 1 2)", "1/2")
        , ("(/ 10 2)", "5")
        , ("(+ 1/3 1/6)", "1/2")
        , ("(expt 2 100)", "1267650600228229401496703205376")
        , ("(* 2 3.5)", "7.0")
        , ("(quotient 7 2)", "3")
        , ("(remainder 7 -2)", "1")
        , ("(modulo 7 -2)", "-1")
        , ("(floor-quotient 7 -2)", "-4")
        , ("(floor/ 7 -2)", "-4\n-1")
        , ("(truncate/ 7 -2)", "-3\n1")
        , ("(max 1 2.0 3)", "3.0")
        , ("(exact 2.5)", "5/2")
        , ("(inexact 1/2)", "0.5")
        , ("(number->string 255 16)", "\"ff\"")
        , ("(string->number \"1/2\")", "1/2")
        , ("(string->number \"nope\")", "#f")
        ]
    , evalCases
        "predicates and equivalence"
        [ ("(eqv? 1 1)", "#t")
        , ("(eqv? 1 1.0)", "#f")
        , ("(equal? '(1 (2 3)) '(1 (2 3)))", "#t")
        , ("(eq? 'a 'a)", "#t")
        , ("(let ((x '(1))) (eq? x x))", "#t")
        , -- zero-length allocations still compare as themselves
          ("(let ((v (vector))) (eqv? v v))", "#t")
        , ("(let ((b (bytevector))) (eqv? b b))", "#t")
        , ("(equal? \"ab\" \"ab\")", "#t")
        , ("(symbol? 'x)", "#t")
        , ("(string? \"x\")", "#t")
        , ("(procedure? car)", "#t")
        , ("(not #f)", "#t")
        , ("(not 0)", "#f")
        ]
    , evalCases
        "conditionals"
        [ ("(if #t 1 2)", "1")
        , ("(if #f 1 2)", "2")
        , ("(if 0 'zero-is-true 'no)", "zero-is-true")
        , ("(cond (#f 1) (#t 2) (else 3))", "2")
        , ("(cond (#f 1) (else 3))", "3")
        , ("(cond ((assv 2 '((1 a) (2 b))) => cadr) (else 'none))", "b")
        , ("(case 3 ((1 2) 'small) ((3 4) 'medium) (else 'big))", "medium")
        , ("(case 9 ((1 2) 'small) (else 'big))", "big")
        , ("(and 1 2 3)", "3")
        , ("(and 1 #f 3)", "#f")
        , ("(and)", "#t")
        , ("(or #f 2)", "2")
        , ("(or #f #f)", "#f")
        , ("(when #t 1 2)", "2")
        , ("(unless #t 1 2)", "#<unspecified>")
        ]
    , evalCases
        "binding forms"
        [ ("(let ((x 1) (y 2)) (+ x y))", "3")
        , ("(let* ((x 1) (y (+ x 1))) y)", "2")
        , ("(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))", "#t")
        , ("(define x 5) (set! x 6) x", "6")
        , ("(let ((x 1)) (let ((x 2)) x))", "2")
        , ("(let ((x 1)) (let ((y 2)) x))", "1")
        , ("(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))", "120")
        , ("(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) sum))", "10")
        , ("(let-values (((a b) (values 1 2))) (+ a b))", "3")
        , ("(define-values (p q) (values 3 4)) (* p q)", "12")
        ]
    , evalCases
        "lambdas and closures"
        [ ("((lambda (x) (* x x)) 7)", "49")
        , ("((lambda (a . rest) rest) 1 2 3)", "(2 3)")
        , ("((lambda args args) 1 2)", "(1 2)")
        , ("(define (add a b) (+ a b)) (add 2 3)", "5")
        , ("(define (counter) (define n 0) (lambda () (set! n (+ n 1)) n)) (define c (counter)) (c) (c) (c)", "3")
        , -- two closures over one binding cell
          ("(define (mk) (define n 0) (values (lambda () (set! n (+ n 1))) (lambda () n))) (let-values (((inc get) (mk))) (inc) (inc) (get))", "2")
        , ("(define (f x) (define y 10) (+ x y)) (f 1)", "11")
        ]
    , evalCases
        "tail calls"
        [ -- would blow the stack without proper tail calls
          ("(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (loop 1000000)", "done")
        , ("(let loop ((n 100000) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc 1))))", "100000")
        ]
    , evalCases
        "pairs and lists"
        [ ("(cons 1 2)", "(1 . 2)")
        , ("(car '(1 2))", "1")
        , ("(cdr '(1 2))", "(2)")
        , ("(list 1 2 3)", "(1 2 3)")
        , ("(length '(a b c))", "3")
        , ("(append '(1) '(2 3) '(4))", "(1 2 3 4)")
        , ("(append '(1) 2)", "(1 . 2)")
        , ("(reverse '(1 2 3))", "(3 2 1)")
        , ("(list-ref '(a b c) 1)", "b")
        , ("(memq 'c '(a b c))", "(c)")
        , ("(member \"b\" '(\"a\" \"b\"))", "(\"b\")")
        , ("(assq 'b '((a 1) (b 2)))", "(b 2)")
        , -- R7RS three argument forms with an explicit comparator
          ("(member 2.0 '(1 2 3) =)", "(2 3)")
        , ("(assoc 2.0 '((1 a) (2 b)) =)", "(2 b)")
        , ("(define l (list 1 2 3)) (list-set! l 1 99) l", "(1 99 3)")
        , ("(define l (list 1 2)) (set-car! l 99) l", "(99 2)")
        , ("(define l (list 1 2)) (set-cdr! l '(x)) l", "(1 x)")
        , ("(list? '(1 2))", "#t")
        , ("(list? '(1 . 2))", "#f")
        ]
    , evalCases
        "quasiquote"
        [ ("`(1 ,(+ 1 1) 3)", "(1 2 3)")
        , ("`(1 ,@(list 2 3) 4)", "(1 2 3 4)")
        , -- the nested quasiquote example from the report. only the
          -- innermost unquote gets evaluated
          ("`(a `(b ,(c ,(+ 1 2))))", "(a `(b ,(c 3)))")
        , ("`#(1 ,(+ 1 1))", "#(1 2)")
        , -- unquote in the cdr position: `(1 . ,x) reads as (1 unquote x)
          ("`(1 . ,(+ 1 1))", "(1 . 2)")
        , ("`(1 2 . ,(list 3 4))", "(1 2 3 4)")
        ]
    , evalCases
        "strings and chars"
        [ ("(string-append \"foo\" \"bar\")", "\"foobar\"")
        , ("(string-length \"hello\")", "5")
        , ("(string-ref \"abc\" 1)", "#\\b")
        , ("(substring \"hello\" 1 3)", "\"el\"")
        , ("(string->list \"ab\")", "(#\\a #\\b)")
        , ("(list->string '(#\\a #\\b))", "\"ab\"")
        , ("(string->symbol \"foo\")", "foo")
        , ("(symbol->string 'foo)", "\"foo\"")
        , ("(string-upcase \"aBc\")", "\"ABC\"")
        , ("(char->integer #\\A)", "65")
        , ("(integer->char 97)", "#\\a")
        , ("(char<? #\\a #\\b #\\c)", "#t")
        , ("(define s (make-string 3 #\\x)) (string-set! s 1 #\\y) s", "\"xyx\"")
        , ("(string=? \"a\" \"a\" \"a\")", "#t")
        , ("(define s (make-string 5 #\\.)) (string-copy! s 1 \"ab\") s", "\".ab..\"")
        , -- literal constants are immutable (R7RS 3.4)
          ("(set-car! '(1 2) 9)", "expected set-car! expects a mutable object, not a literal constant, got: (1 2)")
        , ("(string-set! \"abc\" 0 #\\z)", "expected string-set! expects a mutable object, not a literal constant, got: \"abc\"")
        , ("(vector-set! #(1 2) 0 9)", "expected vector-set! expects a mutable object, not a literal constant, got: #(1 2)")
        , ("(let ((s (string-copy \"abc\"))) (string-set! s 0 #\\z) s)", "\"zbc\"")
        , ("(string-set! (symbol->string 'abc) 0 #\\z)", "expected string-set! expects a mutable object, not a literal constant, got: \"abc\"")
        , ("(eqv? (bytevector) (bytevector))", "#f")
        , -- a definition after an expression in a body is an error
          ("(let () (define a 1) (display a) (define b 2) b)", "bad syntax in body (definition after an expression): (define b 2)")
        , ("(let () (define-values (a b) (values 1 2)) (define c (+ a b)) c)", "3")
        , -- renamed imports of derived forms still work
          ("(import (rename (scheme base) (cond my-cond) (do my-do))) (list (my-cond (#t 1)) (my-do ((i 0 (+ i 1))) ((= i 2) i)))", "(1 2)")
        , -- read leaves the character after the datum
          ("(let* ((p (open-input-string \"1\\n\")) (a (read p)) (b (read-char p))) (list a b))", "(1 #\\newline)")
        , ("(char-foldcase #\\A)", "#\\a")
        , ("(string-foldcase \"AbC\")", "\"abc\"")
        , ("(string->vector \"abc\" 1)", "#(#\\b #\\c)")
        ]
    , evalCases
        "vectors and bytevectors"
        [ ("(vector 1 2 3)", "#(1 2 3)")
        , ("(make-vector 2 'x)", "#(x x)")
        , ("(vector-ref #(a b c) 2)", "c")
        , ("(define v (vector 1 2)) (vector-set! v 0 99) v", "#(99 2)")
        , ("(vector->list #(1 2))", "(1 2)")
        , ("(list->vector '(1 2))", "#(1 2)")
        , ("(vector-length #(1 2 3))", "3")
        , ("(vector-map + #(1 2) #(10 20))", "#(11 22)")
        , ("(bytevector 1 2 3)", "#u8(1 2 3)")
        , ("(bytevector-u8-ref #u8(9 8) 1)", "8")
        , ("(utf8->string (string->utf8 \"héllo\"))", "\"héllo\"")
        , ("(define b (make-bytevector 4 0)) (bytevector-copy! b 1 #u8(7 8)) b", "#u8(0 7 8 0)")
        , ("(utf8->string (string->utf8 \"hello\") 1 3)", "\"el\"")
        ]
    , evalCases
        "higher order procedures"
        [ ("(map (lambda (x) (* x x)) '(1 2 3))", "(1 4 9)")
        , ("(map + '(1 2) '(10 20))", "(11 22)")
        , ("(map + '(1 2 3) '(10 20))", "(11 22)") -- shortest list wins
        , ("(apply + 1 2 '(3 4))", "10")
        , ("(apply max '(3 1 2))", "3")
        , ("(string-map char-upcase \"ab\")", "\"AB\"")
        ]
    , evalCases
        "multiple values and call/cc"
        [ ("(call-with-values (lambda () (values 1 2)) +)", "3")
        , ("(values 42)", "42")
        , ("(call/cc (lambda (k) (+ 1 (k 42))))", "42")
        , ("(+ 1 (call/cc (lambda (k) 10)))", "11")
        , ("(call/cc (lambda (return) (for-each (lambda (x) (if (odd? x) (return x) #f)) '(2 4 5 6)) 'none))", "5")
        ]
    , evalCases
        "exceptions"
        [ ("(guard (e (#t 'caught)) (error \"boom\"))", "caught")
        , ("(guard (e ((error-object? e) (error-object-message e))) (error \"boom\" 1 2))", "\"boom\"")
        , ("(guard (e ((symbol? e) e)) (raise 'oops))", "oops")
        , ("(guard (outer (#t 'outer)) (guard (inner ((number? inner) 'wrong)) (raise 'sym)))", "outer")
        , -- returning from a handler for a plain raise is itself an error.
          -- the outer guard catches that second one
          ( "(guard (e (#t 'secondary)) (with-exception-handler (lambda (e) 'ignored) (lambda () (raise 'x))))"
          , "secondary"
          )
        , -- raise-continuable resumes at the raise point with the handler's value
          ( "(with-exception-handler (lambda (e) 42) (lambda () (+ 1 (raise-continuable 2))))"
          , "43"
          )
        , -- the handler sees the raised object
          ( "(with-exception-handler (lambda (e) (* e 10)) (lambda () (raise-continuable 4)))"
          , "40"
          )
        , -- nested handlers: the inner one wins, popped during its own run
          ( "(with-exception-handler (lambda (e) 'outer) (lambda () (with-exception-handler (lambda (e) (list 'inner e)) (lambda () (raise-continuable 1)))))"
          , "(inner 1)"
          )
        , -- errors thrown from Haskell primitives reach handlers too
          ( "(guard (g (#t 'after)) (with-exception-handler (lambda (e) (display \"\")) (lambda () (car 5))))"
          , "after"
          )
        , -- a guard inside a with-exception-handler catches first
          ( "(with-exception-handler (lambda (e) 'handler) (lambda () (guard (g (#t 'guard)) (raise 'x))))"
          , "guard"
          )
        , -- an unmatched guard re-raises outward to the enclosing handler
          ( "(with-exception-handler (lambda (e) (* e 2)) (lambda () (guard (g ((string? g) g)) (raise-continuable 21))))"
          , "42"
          )
        , ("(guard (e (#t (error-object-irritants e))) (error \"m\" 1 2))", "(1 2)")
        ]
    , evalCases
        "records"
        [ ("(define-record-type point (make-point x y) point? (x point-x) (y point-y set-point-y!)) (point-x (make-point 1 2))", "1")
        , ("(define-record-type point (make-point x y) point? (x point-x) (y point-y set-point-y!)) (define p (make-point 1 2)) (set-point-y! p 99) (point-y p)", "99")
        , ("(define-record-type point (make-point x y) point? (x point-x) (y point-y)) (point? (make-point 1 2))", "#t")
        , ("(define-record-type point (make-point x y) point? (x point-x) (y point-y)) (point? 5)", "#f")
        ]
    , evalCases
        "promises and parameters"
        [ ("(force (delay (+ 1 2)))", "3")
        , ("(define n 0) (define p (delay (begin (set! n (+ n 1)) n))) (force p) (force p)", "1")
        , ("(force 42)", "42") -- force on a non-promise
        , ("(define p (make-parameter 10)) (p)", "10")
        , ("(define p (make-parameter 10)) (parameterize ((p 42)) (p))", "42")
        , ("(define p (make-parameter 10)) (parameterize ((p 42)) #t) (p)", "10")
        , ("(define p (make-parameter 5 (lambda (x) (* x 2)))) (p)", "10")
        ]
    , evalCases
        "macros"
        [ ("(define-syntax my-if (syntax-rules () ((_ c t e) (cond (c t) (else e))))) (my-if #t 'yes 'no)", "yes")
        , ("(define-syntax swap! (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp))))) (define x 1) (define y 2) (swap! x y) (list x y)", "(2 1)")
        , ("(define-syntax my-or (syntax-rules () ((_) #f) ((_ e) e) ((_ e1 e2 ...) (let ((t e1)) (if t t (my-or e2 ...)))))) (my-or #f #f 3)", "3")
        , ("(define-syntax my-list (syntax-rules () ((_ x ...) (list x ...)))) (my-list 1 2 3)", "(1 2 3)")
        , ("(define-syntax pairs (syntax-rules () ((_ (a b) ...) (list (cons a b) ...)))) (pairs (1 2) (3 4))", "((1 . 2) (3 . 4))")
        , ("(let-syntax ((double (syntax-rules () ((_ x) (* 2 x))))) (double 21))", "42")
        , ("(define-syntax kw (syntax-rules (in) ((_ x in y) (cons x y)))) (kw 1 in 2)", "(1 . 2)")
        ]
    , evalCases
        "stdlib"
        [ ("(cadr '(1 2 3))", "2")
        , ("(caddr '(1 2 3))", "3")
        , ("(filter odd? '(1 2 3 4 5))", "(1 3 5)")
        , ("(foldl + 0 '(1 2 3 4))", "10")
        , ("(foldr cons '() '(1 2 3))", "(1 2 3)")
        , ("(any odd? '(2 4 5))", "#t")
        , ("(every odd? '(1 3 5))", "#t")
        , ("((compose car cdr) '(1 2 3))", "2")
        , ("((curry + 10) 5)", "15")
        , ("(fact 10)", "3628800")
        , ("(fib 20)", "6765")
        , ("(string-join '(\"a\" \"b\" \"c\") \"-\")", "\"a-b-c\"")
        ]
    , evalCases
        "string ports"
        [ ("(call-with-output-string (lambda (p) (display \"hi \" p) (display 42 p)))", "\"hi 42\"")
        , ("(define p (open-input-string \"a b\")) (read-char p)", "#\\a")
        , ("(define p (open-input-string \"(1 2) rest\")) (read p)", "(1 2)")
        , ("(read-line (open-input-string \"line one\\nline two\"))", "\"line one\"")
        , ("(eof-object? (read-char (open-input-string \"\")))", "#t")
        ]
    , evalCases
        "ports are parameter objects"
        [ ("(eqv? (current-output-port) (current-output-port))", "#t")
        , ("(eqv? (current-input-port) (current-input-port))", "#t")
        , -- a portless display goes wherever the parameter points
          ( "(call-with-output-string (lambda (p) (parameterize ((current-output-port p)) (display 42))))"
          , "\"42\""
          )
        , ("(input-port-open? (open-input-string \"x\"))", "#t")
        , ("(define p (open-input-string \"x\")) (close-port p) (input-port-open? p)", "#f")
        , ("(binary-port? (open-output-string))", "#f")
        ]
    , evalCases
        "read-string and port->string"
        [ ("(read-string 3 (open-input-string \"hello\"))", "\"hel\"")
        , ("(read-string 9 (open-input-string \"hi\"))", "\"hi\"")
        , ("(eof-object? (read-string 3 (open-input-string \"\")))", "#t")
        , ("(port->string (open-input-string \"a\\nb\"))", "\"a\\nb\"")
        ]
    , evalCases
        "eval"
        [ ("(eval '(+ 1 2))", "3")
        , ("(eval '(+ 1 2) (interaction-environment))", "3")
        , ("(eval (list '+ 1 2))", "3")
        ]
    , evalCases
        "case-lambda"
        [ ("(define f (case-lambda ((x) 'one) ((x y) 'two) ((x . rest) 'many))) (f 1)", "one")
        , ("(define f (case-lambda ((x) 'one) ((x y) 'two) ((x . rest) 'many))) (f 1 2)", "two")
        , ("(define f (case-lambda ((x) 'one) ((x y) 'two) ((x . rest) 'many))) (f 1 2 3)", "many")
        ]
    , evalCases
        "cond-expand and features"
        [ ("(cond-expand (iota 'yes) (else 'no))", "yes")
        , ("(cond-expand (not-a-feature 'yes) (else 'no))", "no")
        , ("(cond-expand ((and r7rs iota) 'both) (else 'no))", "both")
        , ("(cond-expand ((or nope r7rs) 'one) (else 'no))", "one")
        , ("(cond-expand ((not nope) 'not) (else 'no))", "not")
        , ("(cond-expand ((library (scheme sorting)) 'lib) (else 'no))", "no")
        , ("(memq 'r7rs (features))", "(r7rs exact-closed ratios ieee-float full-unicode iota)")
        ]
    , evalCases
        "import and define-library"
        [ ("(import (scheme base) (scheme write)) 'ok", "ok")
        , ( "(define-library (my lib) (import (scheme base)) (export triple) (begin (define (triple x) (* 3 x)))) (import (my lib)) (triple 14)"
          , "42"
          )
        , -- a library's definitions are not visible until imported
          ( "(define-library (my lib) (import (scheme base)) (export triple) (begin (define (triple x) (* 3 x)))) (triple 14)"
          , "unbound variable: triple"
          )
        , ( "(import (only (scheme base) car)) (import (prefix (scheme cxr) x:)) (x:caddr '(1 2 3))"
          , "3"
          )
        , ( "(import (rename (scheme base) (car head))) (head '(1 2))"
          , "1"
          )
        ]
    , evalCases
        "make-list, rationalize, syntax-error"
        [ ("(make-list 3 'x)", "(x x x)")
        , ("(length (make-list 2))", "2")
        , ("(rationalize (exact .3) 1/10)", "1/3")
        , ("(rationalize .3 1/10)", "0.3333333333333333")
        , ("(rationalize 1/3 0)", "1/3")
        , -- syntax-error fires at expansion time, before the guard runs
          ( "(define-syntax m (syntax-rules () ((_ x) (syntax-error \"bad use\" x)))) (guard (e (#t (error-object-message e))) (m 1))"
          , "syntax error: bad use 1"
          )
        ]
    , evalCases
        "sequential read and binary ports"
        [ ("(define p (open-input-string \"1 2 3\")) (list (read p) (read p) (read p))", "(1 2 3)")
        , ("(define p (open-input-string \"(a) b\")) (list (read p) (read p) (eof-object? (read p)))", "((a) b #t)")
        , ("(define p (open-input-bytevector #u8(1 2))) (list (read-u8 p) (read-u8 p) (eof-object? (read-u8 p)))", "(1 2 #t)")
        , ("(define p (open-input-bytevector #u8(9))) (list (peek-u8 p) (read-u8 p))", "(9 9)")
        , ("(define p (open-output-bytevector)) (write-u8 7 p) (write-bytevector #u8(8 9) p) (get-output-bytevector p)", "#u8(7 8 9)")
        , ("(read-bytevector 2 (open-input-bytevector #u8(1 2 3)))", "#u8(1 2)")
        , ("(eof-object? (read-bytevector 1 (open-input-bytevector #u8())))", "#t")
        , ("(binary-port? (open-input-bytevector #u8()))", "#t")
        , ("(u8-ready? (open-input-bytevector #u8()))", "#t")
        , ("(let ((p (open-output-string))) (write-string \"hello\" p 1 3) (get-output-string p))", "\"el\"")
        ]
    , evalCases
        "regression: precision, symbols, chars"
        [ ("(= (+ (expt 2 53) 1) (inexact (expt 2 53)))", "#f")
        , ("(< (inexact (expt 2 53)) (+ (expt 2 53) 1))", "#t")
        , ("(string->symbol \"1\")", "|1|")
        , ("(string->symbol \"-5\")", "|-5|")
        , ("(string->symbol \"abc\")", "abc")
        , ("(guard (e (#t 'range)) (integer->char 55296))", "range")
        , ("(define l (list 1 2)) (set-cdr! (cdr l) l) (equal? l l)", "#t")
        , ("(make-promise 1)", "#<promise>")
        , ("(force (make-promise 42))", "42")
        , ("(map force (map make-promise '(1 2)))", "(1 2)")
        ]
    , evalCases
        "hygiene"
        [ -- a temporary introduced by the template cannot capture a use-site variable
          ("(define-syntax my-or (syntax-rules () ((_ a b) (let ((t a)) (if t t b))))) (let ((t 5)) (my-or #f t))", "5")
        , -- a free identifier in the template means what it meant at the definition
          ("(define-syntax m (syntax-rules () ((_ x) (list x)))) (let ((list vector)) (m 1))", "(1)")
        , ("(define-syntax swap! (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp))))) (define tmp 1) (define y 2) (swap! tmp y) (list tmp y)", "(2 1)")
        , -- auxiliary keywords are compared by binding, so shadowing else defeats it
          ("(define-syntax my-if (syntax-rules () ((_ c t e) (cond (c t) (else e))))) (let ((else #f)) (my-if #f 1 2))", "2")
        , ("(let ((=> #f)) (cond (#t => 'ok)))", "ok")
        , -- (... ...) escapes for macro-defining macros
          ("(define-syntax be-like-begin (syntax-rules () ((_ name) (define-syntax name (syntax-rules () ((_ expr (... ...)) (begin expr (... ...)))))))) (be-like-begin sequence) (sequence 1 2 3 4)", "4")
        , ("(define-syntax my-let* (syntax-rules () ((_ () body ...) (let () body ...)) ((_ ((x v) rest ...) body ...) (let ((x v)) (my-let* (rest ...) body ...))))) (my-let* ((a 1) (b (+ a 1))) (* a b))", "2")
        , ("(define-syntax dupvar (syntax-rules () ((_ a a) a))) (dupvar 1 2)", "syntax error: syntax-rules: duplicate pattern variable: (syntax-rules () ((_ a a) a))")
        , -- a macro can expand into a definition of a use-site name
          ("(define-syntax def (syntax-rules () ((_ n v) (define n v)))) (def x 10) x", "10")
        ]
    , evalCases
        "re-entrant continuations and dynamic-wind"
        [ ("(define k #f) (define n 0) (define r (+ 1 (call/cc (lambda (c) (set! k c) 1)))) (set! n (+ n 1)) (if (< n 3) (k 10) (list r n))", "(11 3)")
        , ( "(define (make-gen lst) (define return #f) (define resume #f) (define (next) (call/cc (lambda (r) (set! return r) (if resume (resume 'go) (begin (for-each (lambda (x) (call/cc (lambda (k) (set! resume k) (return x)))) lst) (return 'done)))))) next) (define g (make-gen '(a b c))) (list (g) (g) (g) (g))"
          , "(a b c done)"
          )
        , -- re-entering a dynamic-wind runs before again. leaving runs after
          ( "(define log '()) (define k2 #f) (define n 0) (dynamic-wind (lambda () (set! log (cons 'in log))) (lambda () (call/cc (lambda (c) (set! k2 c)))) (lambda () (set! log (cons 'out log)))) (set! n (+ n 1)) (if (< n 3) (k2 #f) (reverse log))"
          , "(in out in out in out)"
          )
        , ("(let ((path '())) (dynamic-wind (lambda () (set! path (cons 'before path))) (lambda () (set! path (cons 'during path))) (lambda () (set! path (cons 'after path)))) (reverse path))", "(before during after)")
        , ("(let ((p '())) (call/cc (lambda (k) (dynamic-wind (lambda () (set! p (cons 'in p))) (lambda () (k 'escaped)) (lambda () (set! p (cons 'out p)))))) (reverse p))", "(in out)")
        , -- re-entering map through a continuation must not clobber the
          -- list it already returned
          ( "(let ((kk #f) (n 0) (first #f)) (let ((r (map (lambda (x) (call/cc (lambda (k) (if (= x 2) (set! kk k)) x))) '(1 2 3)))) (set! n (+ n 1)) (if (< n 2) (begin (set! first r) (kk 20)) (list first r))))"
          , "((1 2 3) (1 20 3))"
          )
        , ("(let ((k2 #f) (n 0)) (let ((v (vector-map (lambda (x) (call/cc (lambda (k) (if (= x 1) (set! k2 k)) x))) #(1 2)))) (set! n (+ n 1)) (if (< n 2) (k2 10) v)))", "#(10 2)")
        , -- parameterize is restored on re-entry too
          ("(define p (make-parameter 10 (lambda (x) (* x 2)))) (list (p) (parameterize ((p 5)) (p)) (p))", "(20 10 20)")
        , -- a guard that does not match re-raises in the raise's dynamic environment
          ("(with-exception-handler (lambda (e) 42) (lambda () (+ 1 (guard (g ((string? g) g)) (raise-continuable 'sym)))))", "43")
        , ("(guard (e ((symbol? e) e)) (dynamic-wind (lambda () #f) (lambda () (raise 'inside)) (lambda () #f)))", "inside")
        ]
    , evalCases
        "error objects"
        [ ("(guard (e ((error-object? e) (list (error-object-message e) (error-object-irritants e)))) (car 5))", "(\"expected a pair\" (5))")
        , ("(guard (e ((file-error? e) 'file-error)) (open-input-file \"/nonexistent/file\"))", "file-error")
        , ("(guard (e ((read-error? e) 'read-error)) (read (open-input-string \"(1 2\")))", "read-error")
        , ("(guard (e ((error-object? e) 'err) (else 'other)) (raise (make-parameter 1)))", "other")
        , ("(guard (e (#f 'no)) (raise 'unhandled))", "uncaught exception: unhandled")
        , ("(define f (case-lambda ((x) 1) ((x y) 2))) (f 1 2 3)", "case-lambda: no clause matches the arguments 1 2 3")
        , ("(if)", "bad syntax in if: (if)")
        , ("(letrec ((a b) (b 1)) a)", "unbound variable: b")
        , ("(set! undefined-var 1)", "unbound variable: undefined-var")
        ]
    , evalCases
        "libraries and environments"
        [ ("(eval '(+ 1 2) (environment '(scheme base)))", "3")
        , ("(eval 'car (environment '(only (scheme base) cdr)))", "unbound variable: car")
        , ("(eval '(if #t 1 2) (scheme-report-environment 5))", "1")
        , ("(import (except (scheme base) car)) (car '(1))", "1") -- still bound from the interaction environment
        , ("(define-library (a) (import (scheme base)) (export x) (begin (define x 1))) (define-library (b) (import (scheme base) (a)) (export y) (begin (define y (+ x 1)))) (import (b)) y", "2")
        , ("(define-library (c) (import (scheme base)) (export (rename inner outer)) (begin (define inner 7))) (import (c)) outer", "7")
        , ("(define-library (d) (import (scheme base)) (export m) (begin (define-syntax m (syntax-rules () ((_ x) (* x 2)))))) (import (d)) (m 21)", "42")
        , ("(define-library (e) (import (scheme base)) (export f) (begin (define helper 100) (define (f) helper))) (import (e)) (f)", "100")
        , ("(define-library (e) (import (scheme base)) (export f) (begin (define helper 100) (define (f) helper))) (import (e)) helper", "unbound variable: helper")
        , ("(cond-expand ((library (scheme base)) 'yes) (else 'no))", "yes")
        , ("(cond-expand ((library (scheme nonexistent)) 'yes) (else 'no))", "no")
        ]
    , evalCases
        "numbers: printing and comparison"
        [ ("0.01", "0.01")
        , ("1e7", "10000000.0")
        , ("123456789.0", "123456789.0")
        , ("1e22", "1.0e22")
        , ("1.5e-7", "1.5e-7")
        , ("-0.0", "-0.0")
        , ("(round -0.5)", "-0.0")
        , ("(= (expt 10 400) +inf.0)", "#f")
        , ("(< (expt 10 400) +inf.0)", "#t")
        , ("(number->string 10 1)", "expected a radix of 2, 8, 10 or 16, got: 1")
        , ("(string->number \"#b#b101\")", "#f")
        , ("(= #e1e400 (expt 10 400))", "#t")
        , ("(eqv? 0.0 -0.0)", "#f")
        , ("(vector-ref #(1 2) 1.0)", "expected an exact non-negative integer, got: 1.0")
        , ("(exact->inexact 12345678901234567890)", "12345678901234567000.0")
        ]
    , evalCases
        "equivalence and printing of shared structure"
        [ ("(define a (iota-range 200000)) (define b (iota-range 200000)) (equal? a b)", "#t")
        , ("(let ((l (list 1 2))) (set-cdr! (cdr l) l) l)", "#0=(1 2 . #0#)")
        , ("(let ((x (list 1))) (equal? (list x x) (list (list 1) (list 1))))", "#t")
        , ("(let ((a (list 1 2)) (b (list 1 2))) (set-cdr! (cdr a) a) (set-cdr! (cdr b) b) (equal? a b))", "#t")
        , ("(let ((x (list 1))) (call-with-output-string (lambda (p) (write-shared (list x x) p))))", "\"(#0=(1) #0#)\"")
        , ("(let ((x (list 1))) (call-with-output-string (lambda (p) (write (list x x) p))))", "\"((1) (1))\"")
        , ("'#1=(a . #1#)", "#0=(a . #0#)")
        , ("(let ((v (vector 1 2))) (vector-set! v 0 v) v)", "#0=#(#0# 2)")
        , ("(let ((p (open-output-string))) (write (string #\\x0 #\\x7f) p) (get-output-string p))", "\"\\\"\\\\x0;\\\\x7f;\\\"\"")
        ]
    , evalCases
        "characters and strings (unicode)"
        [ ("(char-numeric? #\\x663)", "#t")
        , ("(digit-value #\\x663)", "3")
        , ("(digit-value #\\a)", "#f")
        , ("(string-ci=? \"Straße\" \"STRASSE\")", "#t")
        , ("(char-foldcase #\\x3a3)", "#\\σ")
        , ("(string-foldcase \"ΣΑΣ\")", "\"σασ\"")
        , ("(read (open-input-string \"#!fold-case ABC\"))", "abc")
        ]
    , evalCases
        "definitions and derived forms"
        [ ("(define-values (a b . c) (values 1 2 3 4)) (list a b c)", "(1 2 (3 4))")
        , ("(let () (define-values (x y) (values 1 2)) (+ x y))", "3")
        , ("(let*-values (((a b) (values 1 2)) ((c) (values (+ a b)))) c)", "3")
        , ("(case 5 ((1 2) 'low) ((5) => (lambda (x) (* x 10))) (else 'other))", "50")
        , ("(do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i 3) acc))", "(2 1 0)")
        , ("(define (f) (define a 1) (define (g) (* a 2)) (g)) (f)", "2")
        , ("(define-record-type <point> (mk x) point? (x px)) <point>", "#<record-type <point>>")
        , ("(let () (define-record-type point (make-point x) point? (x point-x)) (point-x (make-point 1)))", "1")
        , ("(let ((x 1)) (define-syntax get (syntax-rules () ((_) x))) (let ((x 2)) (get)))", "1")
        , ("(define (loop n) (delay-force (if (= n 0) (make-promise 'done) (loop (- n 1))))) (force (loop 100000))", "done")
        , ("(let ((p (delay-force (delay 5)))) (force p))", "5")
        , ("(vector-copy! (vector 1 2 3 4 5) 1 (vector 1 2 3 4 5) 0 3)", "#<unspecified>")
        , ("(let ((v (vector 1 2 3 4 5))) (vector-copy! v 1 v 0 3) v)", "#(1 1 2 3 5)")
        , ("(let ((bv (bytevector 1 2 3))) (read-bytevector! bv (open-input-bytevector #u8(9 8)) 1))", "2")
        , ("(let ((bv (bytevector 1 2 3))) (read-bytevector! bv (open-input-bytevector #u8(9 8)) 1) bv)", "#u8(1 9 8)")
        , ("(list (exact? (current-jiffy)) (exact? (jiffies-per-second)) (inexact? (current-second)))", "(#t #t #t)")
        , ("(assoc \"IOTA_SURELY_UNSET_VARIABLE\" (get-environment-variables))", "#f")
        , ("(string? (cdr (assoc \"PATH\" (get-environment-variables))))", "#t")
        ]
    , evalCases
        "complex numbers"
        [ ("(make-rectangular 1 0)", "1")
        , ("(imag-part 2.5)", "0.0")
        , ("(list +i -i 1+2i 3-i 1/2+3/4i -2.5i)", "(+i -i 1+2i 3-i 1/2+3/4i 0.0-2.5i)")
        , ("(list (make-rectangular 1 2) (make-rectangular 1.0 0.0) (make-polar 2 0))", "(1+2i 1.0+0.0i 2)")
        , ("(list (real-part 1+2i) (imag-part 1+2i) (imag-part 3) (magnitude 3+4i) (angle -1))", "(1 2 0 5 3.141592653589793)")
        , ("(list (+ 1+2i 3-2i) (* +i +i) (- 1+2i) (/ 1+2i 1-2i) (/ 6+4i 2))", "(4 -1 -1-2i -3/5+4/5i 3+2i)")
        , ("(list (sqrt -4) (expt +i 2) (expt +i -1) (square +i) (exact 1.5+2.5i) (inexact 1+2i))", "(+2i -1 -i -1 3/2+5/2i 1.0+2.0i)")
        , ("(list (real? 1+2i) (real? -2.5+0i) (real? -2.5+0.0i) (complex? 1+2i) (exact? 1+2i) (exact? 1.0+2i) (integer? +i))", "(#f #t #f #t #t #f #f)")
        , ("(list (= 1+2i 1+2i) (= 1 1+0.0i) (= +i 1) (eqv? +i +i) (eqv? 1+2i 1.0+2.0i) (equal? (list +i) (list +i)))", "(#t #t #f #t #f #t)")
        , ("(list (zero? 0.0+0.0i) (nan? +nan.0i) (finite? 1+2i) (infinite? +inf.0i))", "(#t #t #t #t)")
        , ("(list (number->string 255+16i 16) (string->number \"1+i\") (string->number \"1+\"))", "(\"ff+10i\" 1+i #f)")
        , ("(exp 0.0+0.0i)", "1.0+0.0i")
        , ("(< +i 1)", "expected a real number, got: +i")
        , ("(abs +i)", "expected a real number, got: +i")
        , ("(number->string 1.5+2i 2)", "expected an exact number for a radix other than 10, got: 1.5+2.0i")
        ]
    , evalCases
        "errors are reported, not crashes"
        [ ("(car 5)", "expected a pair, got: 5")
        , ("(undefined-thing)", "unbound variable: undefined-thing")
        , ("(+ 1 'a)", "expected a number, got: a")
        , ("(1 2)", "not applicable: 1")
        , ("((lambda (x) x))", "lambda: expected exactly 1 argument, got 0")
        , ("(vector-ref #(1) 5)", "vector-ref: index out of range: 5")
        , ("(/ 1 0)", "/: division by zero")
        ]
    ]
