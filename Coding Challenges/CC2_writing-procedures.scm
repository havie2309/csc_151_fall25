; writing-procedures.scm
;; CSC 151 (SEMESTER)
;; Coding Challenge 2 
;; Authors: NGUYEN THI HA VY
;; Date: 13th September 2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

; PROBLEM 1: Categorizing words
; This is my categories for a word as a string
; Sloth – words that have fewer than 3 characters → Lazy
; Snake – words longer than 10 letters → Long
; Lion – starts with uppercase → Roar
; Octopus – words with ≥ 2 "o" → Tentacles
; Peacock – words with unusual letters (z) → Fancyfeathers
; Ewe – words that have the same backward as forward → Same head and tail
; Wild animal – uncategorized word

(define count-check
  (lambda (cha)
    (if (char=? cha #\o) 
      1
      0)))

(define reverse-str
  (lambda (str)
    (list->string (reverse (string->list str)))))

(define categorize-word 
  (lambda (word) 
    (cond 
      [(< (string-length word ) 3) "Lazy like a sloth"] 
      [(> (string-length word ) 8) "Long like a snake"] 
      [(equal? word (string-upcase word)) "Roar like a lion"] 
      [(>= (apply + (map count-check (string->list word))) 2) "Many tentacles like an octopus" ]
      [(string=? (substring word 0 1) "z") "Fancy feathers like a peacock"]
      [(string=? (reverse-str word) word)  "Your head is the same as your tail like an ewe"]
      [else "You are a wild animal"])))

; PROBLEM 2: Median of three
; a) The median of three values using at least one if
(define median3-if
  (lambda (x y z)
    (if (and (<= y x) (<= x z))
      x
      (if (and (<= x y) (<= y z))
        y
        z))))

; b) The median of three values using cond
(define median3-cond
  (lambda (x y z)
    (cond
      [(and (<= y x)(<= x z)) x]
      [(and (<= x y)(<= y z)) y]
      [else z])))

; c) One of the three values is strictly between the other two
(define strictly-between-any?
  (lambda (x y z)
    (and (not(= x y)) (not(= y z)) (not(= x z)))))





