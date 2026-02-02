;; CSC 151 Fall 2025
;; Lab: More List Practice
;; Authors: Patric Desir, Odysseus Wernke, Vy Nguyen
;; Date: October 13th,2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

;; Like the previous lab, we want you to practice writing recursive
;; functions as much as possible! 
;;
;; For each recursive function that you write, make sure to include
;; an appropriate recursive design, docstring, and test cases.

(import test)
(import lab)

; ------------------------------------------------------------------------------

(problem "Problem 1: Intersperse")

;; In this problem, let's consider the problem of a writing a function
;; called intersperse. (intersperse sep lst) return the list lst but
;; with element sep interspersed between the elements of lst.
;;
;; > (intersperse 0 (list 3 8 7 2 1))
;; (list 3 0 8 0 7 0 2 0 1)
;; > (intersperse #\, (list #\a #\b #\c #\d #\e))
;; (list #\a #\, #\b #\, #\c #\, #\d #\, #\e)
;; > (intersperse 22 null)
;; null
;;
;; While seemingly straightforward, it turns out that our basic
;; recursive decomposition will fail us! Let's investigate where we
;; go wrong in this approach and how we can enhance our recursive
;; skeleton to solve this problem.
;;
;; (a) First, consider the following attempt at implementing
;; intersperse using our current technique:

(define intersperse-bad
  (lambda (sep lst)
    (match lst
      [null null]
      [(cons head tail)
       (cons head (cons sep (intersperse-bad sep tail)))])))


;; Translate this implementation back to a recursive decomposition by
;; filling out this template:
;;
;; To intersperse sep between the elements of lst:
;;
;; + When lst is empty, it returns null
;; + When lst is non-empty, it returns a new list with sep in between elements and in the end.

;; (b) Next, let's explore what goes wrong with this approach. Write
;; test case    (lambda () (count-true (list #t #f #t #t #f #f))))
(test-case "count-true: normal case"
  equal? 0
    (lambda () (count-true (list #f #f #f))))s below until you can describe what is wrong with
;; intersperse-bad.

(test-case "Bad test"
  equal? (list 3 0 8 0 7 0 2 0 1 0)
    (lambda () (intersperse-bad 0 (list 3 8 7 2 1))))

;; It has an extra sep in the end of the list.

;; (c) Ultimately, the problem you described above occurs because the way
;; that we decomposed the problem is not accurate! To understand the
;; problem in more detail, trace execution of the following call to
;; intersperse-bad and use the exploration's window to check your work.
;; You may evaluate any given recursive function call directly to the
;; branch that the call would evaluate to in a single step.
;;
;; (intersperse-bad 0 (list 3 8 7 2 1))
;; --> ...
;; --> (cons 3 (cons 0 (intersperse-bad 0 (list 8 7 2 1))))
;; --> (cons 3 (cons 0 (cons 8 (cons 0 (intersperse-bad 0 (list 7 2 1))))))
;; --> ...
;; --> (3 0 8 0 7 0 2 0 1 0)

;; (d) From your trace, you should observe that the way that
;; intersperse-bad decomposes the interspersed list is as follows:
;;
;; 3 0  8 0  7 0  2 0  1 0
;;
;; Where I have uses spaces the pairs of numbers that are consed onto 
;; the result on each recursive call. Each recursive call follows
;; the pattern of:
;;
;;   (cons head (cons sep ...))
;;
;; Resulting in, e.g., 3 0 being added together in the first recursive
;; call to the function. In this light the problem is that the final
;; recursive call has an extraneous 0! To fix the problem, we only
;; need to ensure that we don't add an extra 0 in that last case. When
;; does that occur? That occurs when the list has exactly one element!
;;
;; This leads to one additional case to our recursive decomposition.
;; Complete the recursive decomposition for intersperse:
;;
;; To intersperse sep among the elements of lst:
;; + When the lst is empty, it returns null
;; + When the lst has one element, it returns (cons head null)
;; + When the lst has more than one element, it returns a new list with sep in between elements.
;;
;; Observe how that even though we add an extra case, our cases do
;; not overlap---each possible list only applies to one of the
;; cases---and our cases are exhaustive---they cover all possible
;; lists. We can add additional cases if our recursive decomposition
;; demands it as long as our cases have these two properties!

;; (e) Implement intersperse correctly using this new recursive
;; decomposition. Translate the recursive decomposition precisely,
;; adding an additional branch for the new case.

(define intersperse
  (lambda (sep lst)
    (match lst
      [null null]
      [(cons head null) (cons head null)]
      [(cons head tail)
       (cons head (cons sep (intersperse sep tail)))])))


; ------------------------------------------------------------------------------

(problem "Problem 2: Dropzeroes")

;; Implement a function (dropzeroes lst) that takes a list of
;; numbers lst as input and returns lst but with all the zeroes of
;; lst removed.
;;
;; > (dropzeroes (list 1 3 0 8 0 0 1 0))
;; (list 1 3 8 1)
;; > (dropzeroes null)
;; null

;; TODO: design, implement, document, and test your implementation
;; here!

;;; (dropzeroes lst) -> list?
;;; lst : list?, a list of real numbers
;;; This returns 'lst', but with elements containing 0s removed.
(define dropzeroes
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail)
        (cond [(= head 0)
                (dropzeroes tail)]
              [else (cons head (dropzeroes tail))])])))

(test-case "dropzeroes: normal case"
  equal? (list 1 3 8 1)
    (lambda () (dropzeroes (list 1 3 0 8 0 0 1 0))))

(test-case "dropzeroes: null case"
  equal? null
    (lambda () (dropzeroes (list))))

(test-case "dropzeroes: list of all zeroes -> null?"
  equal? null
    (lambda () (dropzeroes (list 0 0 0 0 0 0))))

; ------------------------------------------------------------------------------

(problem "Problem 3: Count True")


;; Implement a function (count-true lst) that takes a list of
;; booleans lst as input and returns the number of occurrences of
;; #t in lst.
;;
;; > (count-true (list #t #f #t #t #f #f)
;; 3
;; > (count-true null)
;; 0

;;; (count-true lst) -> integer?
;;; lst : list    (lambda () (count-true (list #t #f #t #t #f #f))))
(test-case "count-true: normal case"
  equal? 0
    (lambda () (count-true (list #f #f #f))))?, a list of boolean values
;;; This returns the number of #t values within list 'lst'.
(define count-true
  (lambda (lst)
    (match lst
      [null 0]
      [(cons head tail)
        (cond [(equal? head #t)
                (+ 1 (count-true tail))]
              [else (count-true tail)])])))

(test-case "count-true: normal case"
  equal? 3
    (lambda () (count-true (list #t #f #t #t #f #f))))
(test-case "count-true: normal case"
  equal? 0
    (lambda () (count-true (list #f #f #f))))
(test-case "count-true: null case"
  equal? 0
    (lambda () (count-true (list))))

; ------------------------------------------------------------------------------

(problem "Problem 4: Telescope")

;; Implement a function (telescope lst) that takes a list of
;; numbers lst as input and returns lst but before each element n of
;; lst is the sequence of numbers from 0 up to n-1. If the number
;; non-positive, no additional numbers precede it in the output.
;;
;; > (telescope (list 3 5 -1 1 2))
;; (list 0 1 2 3 0 1 2 3 4 5 -1 0 1 0 1 2)
;; > (telescope null)
;; null

;; TODO: design, implement, document, and test your implementation
;; here!

;;; (telescope lst) -> list?
;;; lst: list?, list of integers
;;; Returns lst with each positive integer being replaced by the range of numbers between
;;; 0 and the element, inclusive.
(define telescope
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail)
        (if (> head 0)
          (append (range (+ 1 head)) (telescope tail))
          (cons head (telescope tail)))])))

; (telescope (list 3 5 -1 1 2))

(test-case "normal case"
  equal? (list 0 1 2 3 0 1 2 3 4 5 -1 0 1 0 1 2)
    (lambda () (telescope (list 3 5 -1 1 2))))

(test-case "negatives only"
  equal? (list -8 -4 -5)
    (lambda () (telescope (list -8 -4 -5))))

(test-case "null list"
  equal? (list)
    (lambda () (telescope (list))))










; ------------------------------------------------------------------------------

