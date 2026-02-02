;; CSC 151 (SEMESTER)
;; Lab: Recursion Practice (recursion-practice.scm)
;; Authors: Ella McEntee, Vy Nguyen
;; Date: October 10th,2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import lab)
(import test)

(title "Recursion practice")

; In today's lab, you will practice applying the recursive
; decomposition techniques discussed in the reading towards writing
; recursive functions over lists. We have stated the mantra that
; "programming is not a spectator sport" at the beginning of the
; course, and that holds especially true for recursive programming.
; Thinking recursively and applying recursive design to problems
; does not come naturally and require dedicated practice. Don't be
; discouraged; put in the effort, and you will succeed!
;
; What follows is a number of recursive functions for you to
; implement with you partner. For each function, you should follow
; the steps outlined in the reading:
;
; 1. Give a recursive decomposition of the problem in terms of the
;    recursive definition of a list. This means you should describe
;    a solution to the problem in terms of the case when the list
;    is empty and when it is non-empty.
; 2. Translate that recursive decomposition directly into code,
;    ideally using the match construct to express the
;    decomposition precisely.
;
; For each function, you should also write documentation and provide
; a brief, yet complete test suite using test-case demonstrating
; that your implementation works. Make sure to use the examples
; given in the problem description as a starting point!
;
; There are many extra problems on this lab that we don't expect you
; accomplish in class. We highly, highly recommend you
; tackle as many of them as time allows (either in class or after)
; so that you can get your practice in!

; ------------------------------------------------------------------------------

(problem "Problem 1: Product")

; Write a function (product lst) that computes the product of
; all the elements contained in lst, assumed to all be numbers.
; If lst is empty, then (product lst) returns 1. For example:
;
;(product (list 3 5 4))
; > 60
; (product (list))
; > 1
;
; <TODO: complete the recursive decomposition sketched below.>
; + When lst is empty, the product is 1
; + When lst is non-empty (with a head and tail), the product is product of head and the tail. 

;;; (product lst) -> number?
;;;   lst: list of number?
;;; Take list's members as an input, returns 1 if it is an empty list, return the product of 
;;; the list if it is an non-empty list.
(define product
  (lambda (lst)
    (match lst
    [null 1]
    [(cons head tail)(* head (product tail))])))

(test-case "The product of normal list"
  equal? 60
  (lambda () (product (list 3 5 4))))

(test-case "The product of empty list"
  equal? 1
  (lambda () (product (list))))

(test-case "The product of negative list"
  equal? -10
  (lambda () (product (list -2 -5 -1))))

; ------------------------------------------------------------------------------

(problem "Problem 2: All")

; Write a function (all lst) that returns #t if all of the elements
; of lst are #t and #f otherwise. It is assumed that lst only
; contains booleans. If the list is empty then the function returns
; #t. For example:
;
; (all (list #t #t #t))
; > #t
; (all (list #t #f #t))
; > #f
; (all (list))
; > #t
;
; <TODO: complete the recursive decomposition sketched below.>
; + When lst is empty, then all returns #t.
; + When lst is non-empty (with a head and tail), then all returns
;   #t when all of them return true.

;;; (all lst) -> boolean?
;;; lst : a list of booleans
;;; takes a list of booleans as input, returns #t for the empty list, for a non-empty 
;;; list, returns #f if any item in the list is #f, and #t if they are all #t. 
(define all
  (lambda (lst)
    (match lst
      [null #t]
      [(cons head tail) (and head (all tail))])))
      
(test-case "all true"
  equal? #t
  (lambda () (all (list #t #t #t))))

(test-case "null list"
  equal? #t
  (lambda () (all (list))))

(test-case "all false"
  equal? #f
  (lambda () (all (list #f #f #f))))

(test-case "some true some false"
  equal? #f
  (lambda () (all (list #f #t #f))))

(test-case "one item"
  equal? #f
  (lambda () (all (list #f))))

; ------------------------------------------------------------------------------

(problem "Problem 3: Contains")

; Write a function (contains x lst) that returns #t if x is contained
; somewhere inside of list lst. You should use equal? to determine
; whether x is one of the elements of lst. For example:
;
; (contains 1 (list 3 1 8))
; > #t
; (contains 4 (list 1 1))
; > #f
; (contains 12 (list))
; > #f
;
; <TODO: complete the recursive decomposition sketched below.>
; + When lst is empty, returns #f
; + When lst is non-empty, returns #t if x is contained
; somewhere inside of list lst.

;;; (contains x lst) -> boolean?
;;;  x: val?
;;; lst: list of val?
;;; Return #f if it is an empty list, for an non-empty list, returns #t if x is contained
;;; somewhere inside of list lst.
(define contains
  (lambda (x lst)
    (match lst
      [null #f]
      [(cons head tail) (or (equal? head x)(contains x tail))])))

(test-case "normal list"
  equal? #t
  (lambda () (contains 1 (list 3 1 8))))

(test-case "normal list but false"
  equal? #f
  (lambda () (contains 4 (list 1 1))))

(test-case "nempty list"
  equal? #f
  (lambda () (contains 12 (list))))

(test-case "string list"
  equal? #t
  (lambda () (contains "a" (list "a" "b" "c"))))

; ------------------------------------------------------------------------------

(problem "Problem 4: Dup")

; Write a function (dup lst) that returns lst but with every
; element of lst repeated or duplicated. For example:
;
; (dup (list 1 2 3))
; > (list 1 1 2 2 3 3)
; (dup (list))
; > (list)
;
; <TODO: complete the recursive decomposition sketched below.>
; + When lst is empty, return null
; + When lst is non-empty, return a new list with duplicated elements

;;; (dup lst) -> list?
;;; lst: list of val?
;;; If the list is empty, return an empty list; if the list is non-empty, return a new list with 
;;; duplicated of each element.
(define dup
  (lambda (lst)
    (match lst
    [null lst]
    [(cons head tail)(cons head (cons head (dup tail)))])))

(test-case "normal list"
  equal? (list 1 1 2 2 3 3)
  (lambda () (dup (list 1 2 3))))

(test-case "empty list"
  equal? (list )
  (lambda () (dup (list ))))

(test-case "list of string"
  equal? (list "a" "a" "b" "b" "c" "c")
  (lambda () (dup (list "a" "b" "c"))))


; +---------------------------+--------------------------------------
; | For those with extra time |
; +---------------------------+

; If you find that you finish all of these problems early, try one
; or more of the following problems.  
; The extra problems are highly recommended to get the hang of
; recursive design


; ------------------------------------------------------------------------------

(problem "Extra 1: Uppercase")

; This is a two-parter! First, write a function (list-uppercase lst)
; that takes a list of characters, lst, as input and returns lst but
; with all characters uppercased. Non-alphabetic characters are left
; unmodified. For example:
;
; (list-uppercase (list #\h #\i #\!))
; > (list #\H #\I #\!)

; Note that you could do this problem with map, but the
; point is to use recursion

; <TODO: complete the recursive decomposition sketched below.>
; + When lst is empty, return an empty list
; + When lst is non-empty, return a nre list wit all characters uppercased

(define list-uppercase
  (lambda (lst)
    (match lst
       [null lst]
       [(cons head tail)(cons (char-upcase head) (list-uppercase tail))])))

(list-uppercase (list #\h #\i #\!))

; Next, using list-uppercase and string-list conversion functions,
; write a function (uppercase str) that returns the uppercase
; version of string str.

(define uppercase
  (lambda (str)
    (list->string (list-uppercase (string->list str)))))

(uppercase "hello world!")

; ------------------------------------------------------------------------------

(problem "Extra 2: Descending")

; This is a two-parter, too! First, write a function (list-descending? lst)
; that takes a list of characters, lst, as input and returns #t if the
; characters are in descending order according to char-ci>=?.  The empty
; list is considered to be in descending order.
; For example:
;
; (list-descending (list #\z #\q #\a))
; > #t
; (list-descending (list #\h #\i #\!))
; > #f
; (list-descending (list))
; > #t
;
; <TODO: complete the recursive decomposition sketched below.>
; + When lst is empty, return #t
; + When lst is non-empty, return #t if the characters are in descending order, and false if not. 

(define list-descending?
  (lambda (lst)
    (match lst
      [null #t]
      [(cons head null) #t]
      [(cons x (cons y tail)) (and (char-ci>=? x y) (list-descending? tail))])))

(list-descending? (list #\z #\q #\a))

(list-descending? (list #\h #\i #\!))

(list-descending? (list))


; Next, using list-descending? and string-list conversion functions,
; write a function (descending? str) that returns #t if and only if
; the characters of string str are in descending order according to
; char-ci>=?.

(define descending?
  (lambda (str)
    (list-descending? (string->list str))))

(descending? "cba")

(descending? "abc")


; ------------------------------------------------------------------------------

(problem "Extra 3: Snoc")

; Write a recursive function `(snoc v l)` that returns list l
; but with value v added to the end of the list. For example:
;
; (snoc 22 (list 1 2 3))
; > (list 1 2 3 22)
; (snoc 0 (list))
; > (list 0)
;
; <TODO: complete the recursive decomposition sketched below.>
; + When lst is empty, return value v
; + When lst is non-empty, return a new list with value v added to the end

(define snoc
  (lambda (v l)
    (match l
      [(cons v null) v]
      [(cons head tail) (cons head (snoc v tail))])))

(snoc 22 (list 1 2 3))

; ------------------------------------------------------------------------------

(problem "Extra 4: Reverse")

; Using snoc from the previous problem, write a function
; `(rev l)` that returns list l but with the order of its
; elements reversed. For example:
;
; (rev (list 1 2 3 4 5))
; > (rev (5 4 3 2 1))
; 
; (NOTE: reverse is already in the standard library; don't use it for this
;        problem but it may be useful in writing test cases!)
;
; <TODO: complete the recursive decomposition sketched below.>
; + When lst is empty, ...
; + When lst is non-empty, ...

(define rev
  (lambda (l)
    ; TODO: implement me!
    null))
