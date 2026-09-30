-- | Longer cases where several forms build on each other. Same helper as
-- Test.Eval.
module Test.Scheme (tests) where

import Test.Tasty
import Test.Util (evalCases)

tests :: TestTree
tests =
  testGroup
    "Scheme programs"
    [ evalCases
        "classic recursion"
        [ ( "(define (ack m n) (cond ((= m 0) (+ n 1)) ((= n 0) (ack (- m 1) 1)) (else (ack (- m 1) (ack m (- n 1)))))) (ack 2 3)"
          , "9"
          )
        , ( "(define (range a b) (if (>= a b) '() (cons a (range (+ a 1) b)))) (define (sum l) (foldl + 0 l)) (sum (range 1 101))"
          , "5050"
          )
        , ( "(define (qsort l) (if (null? l) '() (let ((p (car l)) (rest (cdr l))) (append (qsort (filter (lambda (x) (< x p)) rest)) (list p) (qsort (filter (lambda (x) (>= x p)) rest)))))) (qsort '(3 1 4 1 5 9 2 6))"
          , "(1 1 2 3 4 5 6 9)"
          )
        ]
    , evalCases
        "state and mutation"
        [ ( "(define (make-stack) (define items '()) (lambda (op . args) (case op ((push) (set! items (cons (car args) items))) ((pop) (let ((top (car items))) (set! items (cdr items)) top)) ((peek) (car items))))) (define s (make-stack)) (s 'push 1) (s 'push 2) (s 'pop) (s 'peek)"
          , "1"
          )
        , ( "(define v (make-vector 5 0)) (do ((i 0 (+ i 1))) ((= i 5)) (vector-set! v i (* i i))) v"
          , "#(0 1 4 9 16)"
          )
        ]
    , evalCases
        "association-list interpreter (mini eval)"
        [ ( "(define (lookup k env) (cond ((null? env) (error \"unbound\")) ((eq? k (caar env)) (cdar env)) (else (lookup k (cdr env))))) (lookup 'b '((a . 1) (b . 2)))"
          , "2"
          )
        ]
    , evalCases
        "church numerals via closures"
        [ ( "(define zero (lambda (f) (lambda (x) x))) (define (succ n) (lambda (f) (lambda (x) (f ((n f) x))))) (define (to-int n) ((n (lambda (k) (+ k 1))) 0)) (to-int (succ (succ (succ zero))))"
          , "3"
          )
        ]
    , evalCases
        "macros writing macros' bread and butter"
        [ ( "(define-syntax while (syntax-rules () ((_ test body ...) (let loop () (when test body ... (loop)))))) (define i 0) (define acc '()) (while (< i 5) (set! acc (cons i acc)) (set! i (+ i 1))) acc"
          , "(4 3 2 1 0)"
          )
        , ( "(define-syntax for (syntax-rules (in) ((_ x in lst body ...) (for-each (lambda (x) body ...) lst)))) (define sum 0) (for x in '(1 2 3) (set! sum (+ sum x))) sum"
          , "6"
          )
        ]
    , evalCases
        "generators with call/cc (escaping)"
        [ ( "(define (find-first pred lst) (call/cc (lambda (return) (for-each (lambda (x) (when (pred x) (return x))) lst) #f))) (find-first even? '(1 3 6 7))"
          , "6"
          )
        ]
    ]
