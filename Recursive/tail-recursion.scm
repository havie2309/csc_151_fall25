;; CSC 151-04 Fall 2025
;; Lab: Tail Recursion
;; Authors: Janett, Vy
;; Date: October 27th, 2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import lab)
(import test)

(title "Tail Recursion")

; --------------------------------------------------------------------

(part "Exercise 1: Preparation")

; In today's reading, you wrote a tail-recursive implementation of
; `append`. To begin the lab, review your code with your partner and
; agree upon a solution. In this lab, we'll rewrite several functions
; to be tail-recursive and test them in a standardized way. In
; particular, we shouldn't change the external behavior of a function,
; i.e., what output it produces for a given input. So we can use our
; non-recursive function to test our tail-recursion function!
;
; Below is the standard implementation of append, yet again:

(define list-append
  (lambda (l1 l2)
    (match l1
      [null l2]
      [(cons head tail) (cons head (append tail l2))])))

; Fill in the definition of (a) your tail-recursive helper function
; and (b) your wrapper function for append below. (Hint: you should
; have found that so-far is in the wrong order if you just follow the pattern
; from the reading. If you did not already do so, think about how
; you can use the reverse function to fix the issue.)

(define append-helper
  (lambda (lst so-far)
    (match lst
      [null so-far]
      [(cons head tail)(append-helper tail  (cons head so-far))])))

(define append-tr
  (lambda (l1 l2)
    (append-helper (reverse l1) l2)))

; Observe in the test cases below we use `append` to test `append-tr`.
; Ensure that your implementation passes these tests before continuing!

(test-case
  "append empty"
  equal?
  (append null (list 1 2 3))
  (lambda () (append-tr null (list 1 2 3))))

(test-case
  "append non-empty"
  equal?
  (append (list 1 2 3) (list 4 5 6))
  (lambda () (append-tr (list 1 2 3) (list 4 5 6))))

; Finally, trace through the execution of append-tr on the second
; test case:
;
;      (append-tr (list 1 2 3) (list 4 5 6))
; --> --> (append-helper (reverse (1 2 3)) (4 5 6))
; --> (append-helper (list 3 2 1) (4 5 6))
;--> (match (3 2 1) ([() (4 5 6)] [(cons head tail) (append-helper tail (cons head (4 5 6)))]))
;--> (append-helper (2 1) (cons 3 (4 5 6)))
;--> (append-helper (list 2 1) (list 3 4 5 6))
;--> (match (2 1) ([() (3 4 5 6)] [(cons head tail) (append-helper tail (cons head (3 4 5 6)))]))
;--> (append-helper (1) (cons 2 (3 4 5 6)))
;--> (append-helper (list 1) (list 2 3 4 5 6))
;--> (match (1) ([() (2 3 4 5 6)] [(cons head tail) (append-helper tail (cons head (2 3 4 5 6)))]))
;--> (append-helper () (cons 1 (2 3 4 5 6)))
;--> (append-helper null (list 1 2 3 4 5 6))
;--> (match () ([() (1 2 3 4 5 6)] [(cons head tail) (append-helper tail (cons head (1 2 3 4 5 6)))]))
;--> (1 2 3 4 5 6)
; --> (list 1 2 3 4 5 6)
;
; In a sentence or two below, describe how you know append-tr is tail
; recursive from your trace.

; We know append-str is tail recursive because before moving to the next calls, it appends the tail 
; to the old list and returns the new list.

(description "We know append-tr is tail recursive because... <TODO: fill me in!>")

; --------------------------------------------------------------------

(problem "Exercise 2: Tail Recursing Over Lists")

; It's a blast from the past! Let's rewrite some of our basic
; recursives over lists. For each function:
;
;   1. Write a tail-recursive version of the function.
;   2. Write a collection of test cases that uses the original
;      version of the function to test the tail-recursive version.
;

; (product l) -> number?
;   l : list? of numbers
; Returns the product of the numbers in l.
(define product
  (lambda (l)
    (match l
      [null 1]
      [(cons head tail) (* head (product tail))])))

; TODO: complete the tail-recursive version of the function below.
(define product-helper
  (lambda (l so-far)
    (match l
    [null so-far]
    [(cons head tail) (product-helper tail (* head so-far))])))

(define product-tr
  (lambda (l)
    (product-helper l 1)
))

; TODO: fill in suitable test cases for product/product-tr below.
(test-case "regular test-case"
  equal? (product (list 1 2 1 2))
  (lambda () (product-tr (list 1 2 1 2))))

(test-case "empty test-case"
  equal? (product (list ))
  (lambda () (product-tr (list))))

(test-case "negative number test-case"
  equal? (product (list -2 -3 -2))
  (lambda () (product-tr (list -2 -3 -2))))

(test-case "zero test-case"
  equal? (product (list 0))
  (lambda () (product-tr (list 0))))
; ...

; (any l) -> boolean?
;   l : list? of booleans
; Returns #t iff at least one of the booleans in l is #t
(define any
  (lambda (l)
    (match l
      [null #f]
      [(cons head tail) (or head (any tail))])))

; TODO: complete the tail-recursive version of the function below.
(define any-helper
  (lambda (l so-far)
    (match l
      [null so-far]
      [(cons head tail) (any-helper tail (or head so-far))])))

(define any-tr
  (lambda (l)
    (any-helper l #f)))

; TODO: fill in suitable test cases for any/any-tr below:
(test-case "basic test-case"
  equal? (any (list #t #t #f))
  (lambda () (any-tr (list #t #t #f))))

(test-case "empty test-case"
  equal? (any (list))
  (lambda () (any-tr (list))))

(test-case "all #t test-case"
  equal? (any (list #t #t #t))
  (lambda () (any-tr (list #t #t #t))))

(test-case "all #f test-case"
  equal? (any (list #f #f #f))
  (lambda () (any-tr (list #f #f #f))))
; ...

; --------------------------------------------------------------------

(problem "Exercise 3: Tail Recursing Over Numbers")

; It's yet another blast from the past! Now let's tackle
; tail-recursion over the natural numbers. Again, for each function:
;
;   1. Write a tail-recursive version of the function.
;   2. Write a collection of test cases that uses the original
;      version of the function to test the tail-recursive version.
;

; (harmonic-sequence-sum n) -> number?
;   n : intenger? >= 0
; Returns 0 + 1/1 + 1/2 + 1/3 + ... + 1/n
(define harmonic-sequence-sum
  (lambda (n)
    (if (= n 0)
        0
        (+ (harmonic-sequence-sum (- n 1)) (/ 1.0 n)))))

; TODO: complete the tail-recursive version of the function below.
(define harmonic-sequence-sum-helper
  (lambda (n so-far)
    (match n
      [0 so-far]
      [_ (harmonic-sequence-sum-helper (- n 1) (+ (/ 1.0 n) so-far))])))

(define harmonic-sequence-sum-tr
  (lambda (n)
    (harmonic-sequence-sum-helper n 0)))



; TODO: fill in suitable test cases for
; harmonic-sequence-sum/harmonic-sequence-sum-tr below.

; ...

(test-case "normal test-case"
  (=-eps 0.0001) (harmonic-sequence-sum 6)
  (lambda () (harmonic-sequence-sum-tr 6)))

(test-case "zero test-case"
  (=-eps 0.0001) (harmonic-sequence-sum 0)
  (lambda () (harmonic-sequence-sum-tr 0)))

(test-case "big number test-case"
  (=-eps 0.0001) (harmonic-sequence-sum 200)
  (lambda () (harmonic-sequence-sum-tr 200)))

; (take l n) -> list?
;   l : list?
;   n : integer?
; (take l n) returns the first n elements of l.
(define take
  (lambda (l n)
    (if (= n 0)
        null
        (match l
          [null null]
          [(cons head tail) (cons head (take tail (- n 1)))]))))

; TODO: complete the tail-recursive version of the function below.
; (Hint: like with append, you will need to use reverse to fix up
; the result of the function!)

(define take-helper
  (lambda (l n so-far)
    (if (= n 0)
      so-far
      (match l
      [null so-far]
      [(cons head tail) (take-helper tail (- n 1) (cons head so-far))]))))


(define take-tr
  (lambda (l n)
    (reverse (take-helper l n null))))

; TODO: fill in suitable test cases for take/take-tr below.

; ...

(test-case "normal test-case"
  equal? (take (list 2 3 5 6 7) 2)
  (lambda () (take-tr (list 2 3 5 6 7) 2)))

(test-case "range test-case"
  equal? (take (range 5) 2)
  (lambda () (take-tr (range 5) 2)))

(test-case "zero test-case"
  equal? (take (list 0) 1)
  (lambda () (take-tr (list 0) 1)))

(test-case "empty list test-case"
  equal? (take (list) 1)
  (lambda () (take-tr (list) 1)))

; --------------------------------------------------------------------

(problem "Exercise 4: Cheeky Reversal")

; With append and take, you observed that you needed to call reverse
; in order to fix up the result. This is problematic if the reverse
; function itself is not tail-recursive! Previously, we've
; implemented reverse in terms of a snoc or "cons-on-end" operator.
; In this exercise, we'll take advantage of the cheeky behavior of
; cons on so-far to implement reverse in a tail-recursive way.

; First, fill in this recursive decomposition for reverse in terms of
; the so-far argument. If the recursive decomposition is not
; apparent, try playing with the following example first.
;
; (reverse-helper (list 3 2 1) (list 4 5))
;
; Where the initial list given was (list 1 2 3 4 5), (list 3 2 1) is
; the result accumulated so far and (list 4 5) is the remaining list
; to process. (Hint: when I cons onto the front of so-far, what
; happens?)
;
; To reverse a list `l`, assuming that we have accumulated a reversed
; list `so-far`:
; + If `l` is empty, ...returns null
; + If `l` is non-empty, ... returns the reversed list

; Now, implement the recursive helper for reverse and its suitable
; wrapper function. Ensure that the function behaves identically to
; the library reverse function (which is implemented in a
; tail-recursive style, don't worry!)

(define reverse-helper
  (lambda (so-far l)
    (match l
      [null so-far]
      [(cons head tail) (reverse-helper (cons head so-far) tail)])))

(define reverse-tr
  (lambda (l)
    (reverse-helper null l)))

; TODO: fill in suitable test cases for take/take-tr below.

(test-case "regular list test-case"
  equal? (reverse (list 3 2 1))
  (lambda () (reverse-tr (list 3 2 1))))

(test-case "negative list test-case"
  equal? (reverse (list -3 -2 -1))
  (lambda () (reverse-tr (list -3 -2 -1))))

(test-case "random ordered list test-case"
  equal? (reverse (list 3 5 -1))
  (lambda () (reverse-tr (list 3 5 -1))))

(test-case "string test-case"
  equal? (reverse (list "l" "a" "k"))
  (lambda () (reverse-tr (list "l" "a" "k"))))


