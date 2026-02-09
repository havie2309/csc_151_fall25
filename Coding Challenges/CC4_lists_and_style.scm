; lists-and-style.scm
; CC4-template
;; CSC-151-04 Fall 2025
;; Instructor: Profesor Eikmeier
;; Date: 29th September 2025
;; Author: Thi Ha Vy Nguyen
;; Acknowledgements
(import test)
; PROBLEM 1: Filtering and Transforming Lists
; Part A: Filtering

;;; (inside? n lb ub) -> boolean?
;;;  n: number? 
;;; lb: number?
;;; ub: number?
;;; Returns #t if n is between lb and ub inclusive, otherwise #
(define inside?
    (lambda (n lb ub)
      (and (<= lb n)(<= n ub))))

;;; (restrict-to-range lst lb ub) -> list?
;;; lst: list? 
;;; lb: number?
;;; ub: number?
;;; Returns a sorted list of numbers from lst that are between lb and ub inclusive
(define restrict-to-range
  (lambda (lst lb ub)
    (sort (filter (section inside? _ lb ub) lst) <=)))

; Part B: Filtering
;;; (outside? n lb ub) -> boolean?
;;;  n: number? 
;;; lb: number?
;;; ub: number?
;;; Returns #t if n is outside the inclusive range the lb and ub , otherwise #f
(define outside?
    (lambda (n lb ub)
      (not(and (<= lb ub)(<= lb ub)))))

;;; (outside-to-range lst lb ub) -> list?
;;; lst: list? 
;;; lb: number?
;;; ub: number?
;;; Returns a sorted list of numbers from lst that outside the inclusive range
(define outside-to-range
  (lambda (lst lb ub)
    (sort (filter (section outside? _ lb ub) lst) <=)))

; Part C: Transforming
;;; (max-val lst) -> number?
;;; lst: list?
;;; Returns the largest number in lst
(define max-val
  (lambda (lst)
    (reduce max lst)))

;;; (scale-by-max lst) -> (listof number?)
;;; lst : list?
;;; Returns a new list where each number in lst is divided by the maximum value of lst
;;; If lst is empty or the maximum value is 0, returns an empty list
(define scale-by-max 
  (lambda (lst)
    (if (null? lst)
      (list )
      (if (= (max-val lst) 0)
        (list )
        (map (section / _ (max-val lst)) lst)))))

; Part D: Transforming
;;; (aver-val lst) -> number?
;;; lst : list?
;;; Returns the arithmetic mean (average) of the numbers in lst
(define aver-val
  (lambda (lst)
    (/ (reduce + lst) (length lst))))

;;; (scale-by-max lst) -> list?
;;; lst : list?
;;; Divides every element in lst by the maximum value in the list
;;; If lst is empty, returns an empty list
(define scale-by-average 
  (lambda (lst)
    (if (null? lst)
      (list )
      (if (= (aver-val lst) 0)  
        (list )
        (map (section / _ (aver-val lst)) lst)))))

; Part E: Summary
;;; (non-zero? num) -> boolean?
;;; num : number?
;;; Returns #t if num is not zero, otherwise #f
(define non-zero?
  (lambda (num)
    (not (zero? num))))

;;; (diff-min lst) -> list?
;;; lst: list?
;;; Returns a list of the differences between each element of 
;;; lst and the minimum value in lst, excluding any zeros 
(define diff-min
  (lambda (lst)
    (filter non-zero? (map (section - _ (reduce min lst)) lst))))

;;; (mean-min lst) -> number?
;;; lst : list?
;;; Returns the geometric mean of the differences between each element in lst
;;; and the minimum value, excluding the minimum itself
(define mean-min
  (lambda (lst)
    (expt (reduce * (diff-min lst))(/ 1 (length (diff-min lst))))))

;;; (diff-max lst) -> list?
;;; lst : list?
;;; Returns a list of the differences between the maximum value in lst
;;; and each element, excluding zeros 
(define diff-max
  (lambda (lst)
    (filter non-zero? (map (section - (reduce max lst) _) lst))))

;;; (mean-max lst) -> number?
;;; lst : list?
;;; Returns the geometric mean of the differences between the maximum value
;;; in lst and each element, excluding the maximum itself
(define mean-max
  (lambda (lst)
    (expt (reduce * (diff-max lst))(/ 1 (length (diff-max lst))))))

;;; (skewness lst) -> number?
;;; lst: list?
;;; Returns a measure of skewness defined as the sum of the geometric mean
;;; of differences from the maximum and the geometric mean of differences from the minimum
;;; Error if lst don't have at least two distinct numbers (to avoid all zero differences)
;;; Error if lst contain negative values when computing geometric mean
(define skewness
  (lambda (lst)
    (+ (mean-max lst) (mean-min lst))))

; High skewness: occurs when the maximum and minimum are far from most other elements
;; -> High skewness → list has extreme values relative to most elements
; Low skewness: occurs when the elements are close to each other
;; -> Low skewness → list is more uniform or clustered
; Skewness roughly measures the spread of the list, especially how far elements are from the extremes
; It gives a sense of how “spread out” or “asymmetric” the values are around their extremes


; PROBLEM 2: Tests
; Part A: Filtering
;Given test case
(test-case "Test A0"
  equal?
  (list 1 2 2 3 4 4)
  (lambda () (restrict-to-range (list 5 1 2 8 4 3 19 -5 2 4) 1 4)))

; Case with positive number
(test-case "Test A1"
  equal? (list 4.4 5 8 8.7 9)
  (lambda () (restrict-to-range (list 2 8 9 10 3.4 5 9.2 4.4 8.7) 4 9)))

; Case with negative number
(test-case "Test A2"
  equal? (list -7.2 -6.4 -5.6 -5 -4.5 -4)
  (lambda () (restrict-to-range (list -4 -5 -5.6 -7.2 -6.4 -8.2 -4.5 -1) -8 -2)))

; Case with both negative and positive number
(test-case "Test A3"
  equal? (list -0.6 -0.3 0 3.5 4 4.8 5.7)
  (lambda () (restrict-to-range (list 4 7.6 -4 9 -2.5 -0.6 -0.3 -5 0 5.7 4.8 3.5 ) -2 7)))

; If ub = lb
(test-case "Test A4"
  equal? (list 7)
  (lambda () (restrict-to-range (list 7 9 1 0 9 2) 7 7)))

; Case with outside the range
(test-case "Test A5"
  equal? (list )
  (lambda () (restrict-to-range (list -4 -5 -7) 0 1)))

; Case with duplicate number 
(test-case "Test A6"
  equal? (make-list 10 5 )
  (lambda () (restrict-to-range (make-list 10 5) 0 10)))

; Case with lb > ub
(test-case "Test A7"
  equal? ()
  (lambda () (restrict-to-range (list 3 4 5 1 3 5) 10 2)))

; Part C: Transforming
; Given test case
(test-case "Test C0"
  equal? (list 0.2631578947368421 0.05263157894736842 0.10526315789473684 
    0.42105263157894735 0.21052631578947367 0.15789473684210525 1 
    -0.2631578947368421 0.10526315789473684 0.21052631578947367)
    (lambda () (scale-by-max (list 5 1 2 8 4 3 19 -5 2 4))))

; Case with positive number
(test-case "Test C1"
  equal? (list 0.5660377358490566 0.8490566037735849 1 
    0.22641509433962265 0.5660377358490566 0.9433962264150944)
  (lambda () (scale-by-max (list 3 4.5 5.3 1.2 3 5))))

; Case with negative number 
(test-case "Test C2"
  equal? (list 2.5 3.75 4.416666666666667 1 2.5 4.166666666666667)
  (lambda () (scale-by-max (list -3 -4.5 -5.3 -1.2 -3 -5))))

; Case with negative and positive number 
(test-case "Test C3"
  equal? (list -0.5660377358490566 -0.8490566037735849 1 
    -0.22641509433962265 0.5660377358490566 -0.9433962264150944)
  (lambda () (scale-by-max (list -3 -4.5 5.3 -1.2 3 -5))))

; Case with one number in list
(test-case "Test C4"
  equal? (list 1)
  (lambda () (scale-by-max (list 10))))

; Case with duplicate number
(test-case "Test C5"
  equal? (make-list 5 1)
  (lambda () (scale-by-max (make-list 5 6))))

; Case with empty list
(test-case "Test C6"
  equal? (list )
  (lambda () (scale-by-max (list ))))

; Case with all-zero value
(test-case "Test C7"
  equal? (list )
  (lambda () (scale-by-max (list 0 0 0))))

; PROBLEM 3: What does this code do?
; Part A
(define hamster 
  (lambda (cat dog) 
    (- (/ (+ (min cat dog) 
          (max cat dog)) 
        2) 
      (min cat dog))))

; Part B
(define half-distance 
  (lambda (num1 num2) 
    (- (/ (+ (num1 num2) 2) (min num1 num2)))))

; Part C
;;; (half-distance num1 num2) -> number?
;;;   num1 : number?   ; the first number
;;;   num2 : number?   ; the second number
;;; Returns half the distance between num1 and num2
;;; Useful for computing the midpoint distance from the smaller number 






















