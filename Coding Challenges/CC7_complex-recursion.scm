; CC6-template.scm

;; CSC-151-04 Fall 2025
;; Instructor: Nicole Eikmeier
;; Author: Vy Nguyen
;; Acknowledgements:
    
(import lab)
(import test)

(part "Problem 1: Panlindrome")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Problem 1: Palindromes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (palindrome-helper str n) -> boolean?
;;;  str: string?
;;;  n: val
;;; Returns true if the string str is a palindrome, false otherwise.
(define palindrome-helper
  (lambda (str n)
    (if (>= n (/ (string-length str) 2))
        #t
        (if (char=? (string-ref str n) 
            (string-ref str (- (string-length str) n 1)))
              (palindrome-helper str (+ n 1))
              #f))))
          
;;; (palindrome? str) -> boolean?
;;;  str: string?
;;; Returns true if the string str is a palindrome, false otherwise.
(define palindrome?
  (lambda (str)
    (palindrome-helper str 0)))

(test-case "'civic' is palindrome"
  equal? #t
  (lambda () (palindrome? "civic")))

(test-case "'hello' is not palindrome"
  equal? #f
  (lambda () (palindrome? "hello")))

(test-case "'radar' is palindrome"
  equal? #t
  (lambda () (palindrome? "radar")))

(test-case "'recursion'is palindrome"
  equal? #f
  (lambda () (palindrome? "recursion")))

(test-case "'aabaa' is palindrome"
  equal? #t
  (lambda () (palindrome? "aabaa")))


(part "Problem 2: Make-pair-accessor")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Problem 2: Generating caddadadr procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (procedure str) -> procedure?
;;;   str : string?
;;; Returns a function that consumes a list and repeatedly applies `car`
;;; when the corresponding character of str is 'a', or `cdr` otherwise.
(define procedure
  (lambda (str)
    (let ([head (string-ref str 0)]
          [tail (substring str 1 (string-length str))])
      (if (zero? (string-length str))
        (lambda (lst) lst)
          (o (if (char=? head #\a) car cdr) (procedure tail))))))

;;; (make-pair-accessor name) -> list? or value?
;;;   name : string?
;;; Removes the first and last characters of name and passes the result
;;; to `procedure`, returning a function that accesses part of a list.
(define make-pair-accessor
  (lambda (name)
    (procedure (substring name 1 (- (string-length name) 1)))))

(define my-cadr (make-pair-accessor "cadr"))
(test-case "cadr"
  equal? 2
  (lambda () (my-cadr (list 1 2 3))))

(define my-cdddddr (make-pair-accessor "cdddddr"))
(test-case "cdddddr"
  equal? (list 5 6 7 8 9)
  (lambda () (my-cdddddr (range 10))))

(define my-cdaddr (make-pair-accessor "cdaddr"))
(test-case "cdaddr"
  equal? (list 1)
  (lambda () (my-cdaddr (map range (range 5)))))

(define my-cdar (make-pair-accessor "cdar"))
(test-case "cdar"
  equal? (list 2 3)
  (lambda () (my-cdar (list (list 1 2 3) (list 4 5 6)))))

(define my-car (make-pair-accessor "car"))
(test-case "car"
  equal? 0
  (lambda () (my-car (range 4))))