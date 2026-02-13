; last-CC.scm
;; CSC-151-04 Fall 2025
;; Instructor: Nicole Eikmeier
;; Author: Vy Nguyen
;; Acknowledgements:

(import lab)
(import test)
(import data)
(part "Problem 1: Vector Recursionome")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Problem 1: vector recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (count-matching vec pred? n) -> integer?
;;;   vec: vector?
;;;   pred?: procedure?: boolean?
;;;   n: integer?
;;; Counts how many elements in the vector 'vec' satisfy the predicate 'pred?'.
;;; This function only checks the elements from index 0 up to and including
;;; index 'n'
(define count-matching
  (lambda (vec pred? n)
    (if (< n 0)
        0
        (+ (if (pred? (vector-ref vec n)) 1 0)
           (count-matching vec pred? (- n 1))))))

;;; (my-vector-filter-helper vec pred? n so-far pos) -> vector?
;;;   vec: vector?
;;;   pred?: procedure? boolean?
;;;   n: integer?
;;;   so-far: vector?
;;;   pos: integer?
;;; A tail-recursive helper function that filters 'vec' based on 'pred?'. It iterates 
;;; *up* from index 'n' in 'vec'. It mutates the 'so-far' vector, placing matching 
;;; elements into 'so-far' starting at index 'pos'.
(define my-vector-filter-helper
  (lambda (vec pred? n so-far pos)
    (if (> n (vector-length vec))
        so-far
        (if (pred? (vector-ref vec n))
            (begin
              (vector-set! so-far pos (vector-ref vec n))
              (my-vector-filter-helper vec pred? (+ n 1) so-far (+ pos 1)))
            (my-vector-filter-helper vec pred? (+ n 1) so-far pos)))))

;;; (my-vector-filter vec pred?) -> vector?
;;;   vec: vector?
;;;   pred?: procedure? (any? -> boolean?)
;;; Creates a new vector containing only the elements from 'vec'
;;; that satisfy the predicate 'pred?'.
(define my-vector-filter
  (lambda (vec pred?)
    (let* ([count (count-matching vec pred? (- (vector-length vec) 1))]
           [result (make-vector count 0)])
      (my-vector-filter-helper vec pred? 0 result 0))))

(test-case "Vector1 just number"
  equal? (vector 1 2 3)
    (lambda () (my-vector-filter (vector 1 2 3) number?)))

(test-case "Vector2 just number"
  equal? (vector 1 2 3)
    (lambda () (my-vector-filter (vector 1 2 3 "a" #\b #t) number?)))

(test-case "Vector3 just number"
  equal? (vector 1 2 3)
    (lambda () (my-vector-filter (vector 1 'a 2 'v 3 'c) number?)))

(test-case "Vector just string"
  equal? (vector "you" "can" "do" "it")
    (lambda () (my-vector-filter (vector 1 "you" 2 "can" 3 "do" "it") string?)))

(test-case "Vector just char"
  equal? (vector #\a #\b #\c)
    (lambda () (my-vector-filter (vector #\a 2 #\b 5 "b" #\c) char?)))

(test-case "Vector just boolean"
  equal? (vector #t #t #f)
    (lambda () (my-vector-filter (vector #t "a" #t 5 10 "b" #f) boolean?)))

(part "Problem 2: Visualizing speeches")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Problem 2: Visualizing speeches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(problem "Provided speeches")
(define some-speeches
  (list (list "Joe" "My name is Joe")
        (list "Jane" "I heard that Abraham Lincoln once said that")
        (list "Joe" "My name is still Joe")
        (list "Jae" "I am not inclined to speak at length, nor on matters")
        (list "Joe" "It's still Joe")
        (list "Jo" "I am often mistaken for another person. Let me explain")
        (list "Joe" "I'm not Jo")))

;;; (numWords str) -> integer?
;;;   str: string?
;;; Counts the number of words in a string.
(define numWords
  (lambda (str)
    (let ([speeches (string-split str " ")])
    (length speeches))))

;;; (numLength str) -> integer?
;;;   str: string?
;;; Calculates the total number of characters in 'str', excluding all space characters (" ")
(define numLength
    (lambda (str)
      (let* ([speeches (string-split str "")]
            [new-speeches (filter (section not (equal? _ " ")) speeches)])
      (length new-speeches))))

;;; (averageLength str) -> number?
;;;   str: string?
;;; Calculates the average word length in a string.
;;; It divides the result of (numLength str) by the result of (numWords str)
(define averageLength
  (lambda (str)
    (/ (numLength str) (numWords str))))

;;; (new-pair lst) -> pair?
;;;   lst: list?
;;; Takes a single "speech entry" list and converts it into a
;;; coordinate pair suitable for plotting.
(define new-pair
  (lambda (lst)
    (pair (numWords (cadr lst)) (averageLength (cadr lst)))))

;;; (visualize-speeches speech) -> plot?
;;;   speech: list?
;;; Generates a scatter plot from a list of speech entries.
(define visualize-speeches
  (lambda (speech)
    (let* ([some-speeches-pair (map new-pair speech)])
  (with-plot-options
    (list (pair "x-label" "Number of words in a speech")
          (pair "y-label" "Average word length in the speech"))
    (plot-linear
      (dataset-scatter "Speeches"
        some-speeches-pair))))))

(visualize-speeches some-speeches)

(define some-speeches-v2
  (list (list "Joe" "My name is Joe")
        (list "Jane" "I heard that Abraham Lincoln once said that")
        (list "Joe" "My name is still Joe")
        (list "Jane" "I am not inclined to speak at length, nor on matters")
        (list "Joe" "It's still Joe")
        (list "Joe" "I am often mistaken for another person. Let me explain")
        (list "Joe" "I'm not Jo")))

;;; (speaker? speech speaker) -> boolean?
;;;   speech: list?
;;;   speaker: list?
;;; A predicate that determines if the given 'speaker' is the
;;; speaker of the 'speech'.
(define speaker?
  (lambda (speech speaker)
    (equal? (car speech) speaker)))

;;; (visualize-speeches-prime speech speakers) -> plot?
;;;   speech: list?
;;;   speakers: list?
;;; Generates a scatter plot comparing the speeches of two
;;; different speakers from the 'speech' list.
(define visualize-speeches-prime
  (lambda (speech speakers)
    (let* ([speaker1 (car speakers)]
           [speaker2 (cadr speakers)]
           [isspeaker1 (filter (section speaker? _ speaker1) speech)]
           [isspeaker1pair (map new-pair isspeaker1)]
           [isspeaker2 (filter (section speaker? _ speaker2) speech)]
           [isspeaker2pair (map new-pair isspeaker2)])
    (with-plot-options
      (list (pair "x-label" "Number of words in a speech")
            (pair "y-label" "Average word length in the speech"))
      (plot-linear
        (with-dataset-options
          (list (pair "background-color" "violet"))
          (dataset-scatter speaker1
            isspeaker1pair))
        (with-dataset-options
          (list (pair "background-color" "cyan"))
          (dataset-scatter speaker2
            isspeaker2pair)))))))

(visualize-speeches-prime some-speeches-v2 (list "Joe" "Jane"))

;;; (ratio str) -> number?
;;;   str: string?
;;; Calculates the ratio of the total number of non-space
;;; characters (from numLength) to the average word length
;;; (from averageLength)
(define ratio
  (lambda (str)
    (/ (numLength str) (averageLength str))))

;;; (speech-histogram speech) -> plot?
;;;   speech: list?
;;;
;;; Generates a categorical bar chart from a list of speech entries.
;;; It creates one bar for every speech in the list.
(define speech-histogram
  (lambda (speech)
    (let* ([speaker (map car speech)]
           [ratio-speech (map (section ratio (cadr _)) speech)])
    (plot-category
  speaker
  (with-dataset-options
      (list (pair "background-color" "pink"))
      (dataset-bar "Ratio speech"
        ratio-speech))))))

(speech-histogram some-speeches-v2)

(problem "New speeches")
(define some-speeches-2
  (list (list "Vy" "Tao la Vy nay")
        (list "Mem" "Chao may tao la Mem hihi")
        (list "Vy" "May khoe khong")
        (list "Mem" "Tao khoe may thi sao")
        (list "Vy" "Thoi tao di hoc day")
        (list "Mem" "U the thoi chao may nhe")))

(visualize-speeches some-speeches)
(visualize-speeches-prime some-speeches-2 (list "Vy" "Mem"))
(speech-histogram some-speeches-2)



  