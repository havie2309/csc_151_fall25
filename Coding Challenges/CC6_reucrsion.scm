; reucrsion-cc.scm
; CC6-template.scm

;; CSC-151-04 Fall 2025
;; Instructor: Nicole Eikmeier
;; Author: Vy Nguyen
;; Acknowledgements:
    

(import data)
(import lab)
(import test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Problem 1: Remove val;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (remove-all val lst) -> list?
;;;  val: value?
;;;  lst: list of value?
;;; Return a new list that removes all copies of val from lst.
;; If head matches val, skip it and recurse on tail; otherwise, keep head and recurse.
(define remove-all
  (lambda (val lst)
    (match lst
       [null null]
       [(cons head tail) 
         (if (equal? val head)
           (remove-all val tail)
           (cons head (remove-all val tail)))])))

(test-case "List of characters"
  equal? (list "b" "d" "c")
  (lambda () (remove-all "a" (list "b" "a" "d" "a" "c"))))

(test-case "List of numbers"
  equal? (list 4 3 2)
  (lambda () (remove-all 7 (list 7 7 7 4 3 7 2))))

(test-case "List of null"
  equal? (list)
  (lambda () (remove-all "a" (list ))))

(test-case "All elements match"
  equal? (list )
  (lambda () (remove-all 1 (list 1 1 1 1 1 1))))

(test-case "Value not in list"
  equal? (list 1 2 3)
  (lambda () (remove-all 4 (list 1 2 3))))

(test-case "Large list with interleaved matches"
  equal? (list 1 3 4 5 6 7 8 9)
  (lambda () (remove-all 2 (list 1 2 3 2 4 5 2 6 7 2 8 9))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Problem 2: Remove duplicates;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (remove-duplicates lst) -> list?
;;;  lst: list?
;;; Returns a new list with the duplicates removed.
;; Keep head, remove all its duplicates from tail, then recurse.
(define remove-duplicates
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail)
        (cons head 
          (remove-duplicates (remove-all head tail)))])))

(test-case "List of non-duplicated numbers"
  equal? (list 4 2 1 5 6 23 27)
  (lambda () (remove-duplicates (list 4 2 1 5 2 4 6 1 23 27 6 1 1 1 2))))

(test-case "List of non-duplicated characters"
  equal? (list "b" "a" "d" "c" "e")
  (lambda () (remove-duplicates (list "b" "a" "d" "a" "c" "e" "c" "d" "a"))))

(test-case "Empty list"
  equal? (list )
  (lambda () (remove-duplicates (list ))))

(test-case "List of one element"
  equal? (list "a")
  (lambda () (remove-duplicates (list "a" "a" "a" "a" "a"))))

(test-case "All duplicates"
  equal? (list 2)
  (lambda () (remove-duplicates (list 2 2 2 2 2 2))))

(test-case "No duplicates"
  equal? (list 1 2 3 4 5)
  (lambda () (remove-duplicates (list 1 2 3 4 5))))

(test-case "Negative numbers"
  equal? (list -3 -2 -1 0 1 2)
  (lambda () (remove-duplicates (list -3 -2 -2 -1 0 0 1 2 -3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Problem 3: Joining Sorted Lists ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; (merge-sorted-lists lst1 lst2) -> list?
;;;  lst1 -> list?
;;;  lst2 -> list?
;;; Returns the combination of those two lists, still in sorted order.
;; Compare heads of both lists; take smaller (or equal) and recurse on the rest.
(define merge-sorted-lists
  (lambda (lst1 lst2)
    (match lst1
     [null lst2]
     [(cons head1 tail1)
        (match lst2
          [null lst1]
          [(cons head2 tail2)
            (if (<= head1 head2)
              (cons head1 (merge-sorted-lists tail1 lst2))
              (cons head2 (merge-sorted-lists lst1 tail2)))])])))

(test-case "Two lists of numbers"
  equal? (list 0 1 2 3 4 5 6 7 7 18 19 22)
  (lambda () (merge-sorted-lists (list 1 5 6 7 22) (list 0 2 3 4 7 18 19))))

(test-case "Another two lists of numbers"
  equal? (list 1 2 3 3 4 5 5 6 7)
  (lambda () (merge-sorted-lists (list 1 3 5 7) (list 2 3 4 5 6))))

(test-case "Two lists with one empty list"
  equal? (list 2 3 4 5 6)
  (lambda () (merge-sorted-lists (list ) (list 2 3 4 5 6))))

(test-case "Two empty lists"
  equal? (list)
  (lambda () (merge-sorted-lists (list) (list))))

(test-case "Lists with negative numbers"
  equal? (list -3 -2 -1 0 1 2)
  (lambda () (merge-sorted-lists (list -3 -1 1) (list -2 0 2))))

(test-case "Repeated numbers"
  equal? (list 1 1 2 2 3 3)
  (lambda () (merge-sorted-lists (list 1 2 3) (list 1 2 3))))

(test-case "Long and short"
  equal? (list 0 1 2 3 4 4 5 6 8 9 12 15)
  (lambda () (merge-sorted-lists (list 1 3 5) (list 0 2 4 4 6 8 9 12 15))))





 


  