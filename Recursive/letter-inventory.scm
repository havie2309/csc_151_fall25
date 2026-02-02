;; CSC 151 Fall 2025
;; Lab: Letter Inventory (letter-inventory.scm)
;; Authors: Graham and Vy
;; Date: October 31 2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import lab)
(import test)

(title "Letter Inventory")

;; In today's lab, we'll review the association list data structure, a list of
;; pairs, that allow us to store values by associating them with keys. This
;; data structure is also called a map or a dictionary. We will then apply our
;; association list to do some experiementation over text: counting the letters
;; of a text.

;; -----------------------------------------------------------------------------

(part "Problem 1: Guilty by association")


;; Recall the set up for an association list:
;;
;; + An association list is a set of pairs.
;; + Each pair contains a key and a value.
;; + The key is used to "look up" its associated value in the list.
;;
;; For this first problem, we'll use an association list to maintain a roster
;; of students---mad scientists---and their attendance in class at mad
;; scientist univeristy. Complete the definition roster below as an
;; association list that makes the test cases below pass.

(define example-roster
  (list (pair "Prof. Froth" 5)
        (pair "Dr. Spark" 5)
        (pair "Prof. Afterthought" 2)
        (pair "Dr. Agon" 3)
        (pair "Dr. Putrid" 5)))

; Mad scientist names generated from Fantasy Name Generator:
; https://www.fantasynamegenerators.com/mad-scientist-names.php

(test-case "Prof. Froth is in the class"
  equal? #t
  (lambda ()
    (assoc-key? "Prof. Froth" example-roster)))

(test-case "Prof. Pride is not in the class"
  equal? #f
  (lambda ()
    (assoc-key? "Prof.Pride" example-roster)))

(test-case "Dr. Spark's attendance"
  = 5
  (lambda ()
    (assoc-ref "Dr. Spark" example-roster)))

(test-case "Prof. Froth's attendance"
  = 5
  (lambda ()
    (assoc-ref "Prof. Froth" example-roster)))

(test-case "Prof. Afterthought's attendance"
  = 2
  (lambda ()
    (assoc-ref "Prof. Afterthought" example-roster)))

(test-case "Dr. Agon's attendance"
  = 3
  (lambda ()
    (assoc-ref "Dr. Agon" example-roster)))

(test-case "Dr. Putrid's attendence"
  = 5
  (lambda ()
    (assoc-ref "Dr. Putrid" example-roster)))

;; The test cases demonstrate the use of two of the three association list
;; functions provided in the standard library: assoc-key? and assoc-ref.
;; Frequently, we'll use assoc-key? to first check if a key is in the
;; association list. If so, we know that we can then use assoc-ref to
;; safely pull out its corresponding value.
;;
;; Use these functions to implement the following function and give
;; appropriate test cases for the function using example-roster above.

;; (attended-all? student num-classes roster) -> boolean?
;;   student: string?
;;   total-classes: integer?, a non-negative value
;;   roster: list?, an association list of students and their attendance.
;; Returns #t if and only if student is in the roster and if they have
;; attended all of the classes this semester (captured by total-classes).
(define attended-all?
  (lambda (student total-num-classes roster)
    (if (assoc-key? student roster)
        (equal? (assoc-ref student roster) total-num-classes)
        #f)))

(test-case "Dr. Putrid's attend all"
  equal? #t
  (lambda ()
    (attended-all? "Dr. Putrid" 5 example-roster)))

(test-case "Doesn't exist"
  equal? #f
  (lambda ()
    (attended-all? "Joe" 5 example-roster)))

(test-case "Didn't attend all"
  equal? #f
  (lambda ()
    (attended-all? "Dr. Putrid" 7 example-roster)))

;; -----------------------------------------------------------------------------

(part "Problem 2: Tracking")

;; Finally, let's look at assoc-set. (assoc-set k v l) returns a new
;; association list that is identical to l, except that key k is now
;; associated with value v. Use assoc-set to implement the following function
;; over our class roster and give appropriate test cases using example-roster
;; from the previous problem.

;; (update-student student roster) -> list?
;;   student: string?
;;   roster: list?, an assoc. list of students and their attendance.
;; Returns a new association list that is roster except updated so that
;; the given student is attending one more class than before. If the student
;; is not yet in the roster, then they should now have an attendance of one.
(define update-student
  (lambda (student roster)
    (if (assoc-key? student roster)
        (assoc-set student (+ (assoc-ref student roster) 1) roster)
        (assoc-set student 1 roster))))

(test-case "Dr. Putrid attending again"
  equal? (list (pair "Prof. Froth" 5)
               (pair "Dr. Spark" 5)
               (pair "Prof. Afterthought" 2)
               (pair "Dr. Agon" 3)
               (pair "Dr. Putrid" 6))
  (lambda ()
    (update-student "Dr. Putrid" example-roster)))

(test-case "New person"
  equal? (list (pair "Prof. Froth" 5)
               (pair "Dr. Spark" 5)
               (pair "Prof. Afterthought" 2)
               (pair "Dr. Agon" 3)
               (pair "Dr. Putrid" 5)
               (pair "Joe" 1))
  (lambda ()
    (update-student "Joe" example-roster)))

;; -----------------------------------------------------------------------------

(part "Problem 3: Letter Inventory")

;; Now, let's turn our attention towards an application of dictionaries to
;; analyzing written texts: counting letters. While seemingly innoculous,
;; the frequency of letters is not arbitrary---some letters are more common
;; than others! Let's use an association list to compute frequency
;; counts of letters in a text to see how accurate this claim is. In our
;; association list, keys will consist of the 26 lowercase characters and
;; their associated values will be the number of times the character has
;; occurred in the text.


(problem "3a: empty-inventory")

;; First, complete the definition of empty-inventory that is an empty
;; letter inventory. An empty letter inventory has an entry for each
;; lowercase character set to zero.
;;
;; (Hint: while you could implement this with 26 calls to pair, try instead
;; to implement this as a pipeline involving range and map instead! To
;; assist you in this, you may want to implement a helper function that
;; takes a number in the range 0--25 and turns it into a lowercase
;; character. The functions integer->char and char->integer will be useful
;; for this helper function.)

(define helper 
  (lambda (n)
    (integer->char (+ n (char->integer #\a)))))

(define pair-0
  (lambda (k)
    (pair k 0)))

(define empty-inventory
  (|> 26 range (section map helper _) (section map pair-0 _))) 

empty-inventory

(test-case "No 'a's in the empty inventory"
  = 0
  (lambda ()
    (assoc-ref #\a empty-inventory)))

(test-case "No 'm's in the empty inventory, too"
  = 0
  (lambda ()
    (assoc-ref #\m empty-inventory)))

(problem "3b. update-inventory")


;; Now, let's implement the critical function for our letter inventory.
;; (update-inventory inv ch) takes an existing letter inventory inv and
;; a single character ch as input. If ch is a character (either lowercase or
;; uppercase), then update-inventory returns inv, but updated so that the
;; entry for ch has been incremented by one. Otherwise, inv is returned
;; unmodified.
;;
;; Implement update-inventory below, giving it a suitable docstring and
;; test suite.
;;
;; (_Hint_: this function is similar to update-student from the previous
;; problem. Make sure to add in appropriate tests to see if the input character
;; is a letter and make sure that you "process" the character to ensure that
;; it is lowercase!)

;;; (update-inventory inv ch) -> list?
;;;   inv: list?, of pairs
;;;   ch: char?
;;; Adds 1 to the pair with the key ch and returns unmodified inv if ch is not a key.
(define update-inventory
  (lambda (inv ch)
    (if (assoc-key? (char-downcase ch) inv)
        (assoc-set (char-downcase ch) (+ 1 (assoc-ref (char-downcase ch) inv)) inv)
        inv)))

(update-inventory empty-inventory #\b)

(test-case "Add to #\\b"
  equal? (list (pair #\a 0) (pair #\b 1) (pair #\c 0) (pair #\d 0)
               (pair #\e 0) (pair #\f 0) (pair #\g 0) (pair #\h 0)
               (pair #\i 0) (pair #\j 0) (pair #\k 0) (pair #\l 0)
               (pair #\m 0) (pair #\n 0) (pair #\o 0) (pair #\p 0)
               (pair #\q 0) (pair #\r 0) (pair #\s 0) (pair #\t 0)
               (pair #\u 0) (pair #\v 0) (pair #\w 0) (pair #\x 0)
               (pair #\y 0) (pair #\z 0))
  (lambda ()
    (update-inventory empty-inventory #\b)))

(test-case "Add to #\\D"
  equal? (list (pair #\a 0) (pair #\b 0) (pair #\c 0) (pair #\d 1)
               (pair #\e 0) (pair #\f 0) (pair #\g 0) (pair #\h 0)
               (pair #\i 0) (pair #\j 0) (pair #\k 0) (pair #\l 0)
               (pair #\m 0) (pair #\n 0) (pair #\o 0) (pair #\p 0)
               (pair #\q 0) (pair #\r 0) (pair #\s 0) (pair #\t 0)
               (pair #\u 0) (pair #\v 0) (pair #\w 0) (pair #\x 0)
               (pair #\y 0) (pair #\z 0))
  (lambda ()
    (update-inventory empty-inventory #\D)))

(test-case "Add to #\\3"
  equal? (list (pair #\a 0) (pair #\b 0) (pair #\c 0) (pair #\d 0)
               (pair #\e 0) (pair #\f 0) (pair #\g 0) (pair #\h 0)
               (pair #\i 0) (pair #\j 0) (pair #\k 0) (pair #\l 0)
               (pair #\m 0) (pair #\n 0) (pair #\o 0) (pair #\p 0)
               (pair #\q 0) (pair #\r 0) (pair #\s 0) (pair #\t 0)
               (pair #\u 0) (pair #\v 0) (pair #\w 0) (pair #\x 0)
               (pair #\y 0) (pair #\z 0))
  (lambda ()
    (update-inventory empty-inventory #\3)))

(problem "3c. make-inventory")


;; With empty-inventory and update-inventory, let's make our "top-level"
;; function. (make-inventory str) takes a string as input and produces a letter
;; inventory consisting of character counts from that string. To implement this
;; function, make use a pipeline:
;;
;; 1. Transform the string into a list of characters.
;; 2. Compute a final inventory for the string by sequentially updating an
;;    inventory with each character.
;;
;; What functions from the standard library can we use to accomplish these
;; tasks?
;;
;; Give a docstring and implement make-inventory below. If everything works,
;; the test cases given below should pass!
;;; (update inv lst)
;;;  inv: list?
;;;  lst: list of char?
;;; Update the inventory by adding each letter in the list.
(define update
  (lambda (inv lst)
    (match lst
      [null inv]
      [(cons head tail)
        (update (update-inventory inv head) tail)])))

;;; (make-inventory str)
;;;  str: string?
;;; Returns the number of character counts from the string.
(define make-inventory
  (lambda (str)
    (update empty-inventory (string->list str))))

(define inventory-for-greeting (make-inventory "hello world!"))

(test-case "Greeting counts for 'l'"
  = 3
  (lambda ()
    (assoc-ref #\l inventory-for-greeting)))

(test-case "Greeting counts for 'o'"
  = 2
  (lambda ()
    (assoc-ref #\o inventory-for-greeting)))

(test-case "Greeting counts for 'q'"
  = 0
  (lambda ()
    (assoc-ref #\q inventory-for-greeting)))

;; -----------------------------------------------------------------------------

(part "Problem 4: Investigating Frequencies")


;; Finally, with make-inventory implemented, let's try it out by investigating
;; letter frequencies in real-world texts. From this Wikipedia article:
;;
;; https://en.wikipedia.org/wiki/Letter_frequency
;;
;; You can see that there is a known set of average frequencies for English
;; letters with some skewed distribution of frequency values. For example 'e'
;; appears much more often than the rare consonants such as 'q' or 'z'.
;; However, do these frequencies persist across all texts?
;;
;; For this final part, go out and find different texts, calculate their
;; letter inventories, and write test cases demonstrating that the relations
;; between frequencies of different letters hold. For example, you might test
;; whether one letter appears more often than another or whether a letter
;; occurs most frequently in a text.
;;
;; You should generate letter inventories for three different texts that you
;; find on the internet and write test cases for two different relationships
;; between frequencies, six tests in total among the three texts. Feel free
;; to comb news websites, Wikipedia, or literature on websites such as
;; Project Gutenberg for material. Aim for a varied set of sources across
;; genres and, ideally, time periods. You should sample a reasonable amount of
;; text from each source, e.g., a few paragraphs, filling in the definitions
;; below with the data as string literals.
;;
;; Note that not all of your test cases may pass! For each test case that does
;; not pass, look at the text, and try to explain why the relationship does
;; not hold as predicted. For example, you might point out that the way that
;; the text is written is not "normal" and thus doesn't exhibit the same
;; distribution of frequencies as the "average" text.

(description "First paragraph of the linked wikipedia article")

(define sample-text-1 "Letter frequency is the number of times letters of the alphabet 
  appear on average in written language. Letter frequency analysis dates back to the Arab 
  mathematician Al-Kindi (c. AD 801–873), who formally developed the method to break ciphers. 
  Letter frequency analysis gained importance in Europe with the development of movable type 
  in AD 1450, wherein one must estimate the amount of type required for each letterform. 
  Linguists use letter frequency analysis as a rudimentary technique for language identification, 
  where it is particularly effective as an indication of whether an unknown writing system is 
  alphabetic, syllabic, or ideographic. ")

(define not-whitespace?
  (lambda (ch)
    (not (or (char-whitespace? ch) (equal? #\newline ch)))))

(list->string (filter not-whitespace? (string->list sample-text-1)))

(test-case "Frequency of e"
  = (* .127 (string-length (list->string (filter not-whitespace? (string->list sample-text-1)))))
  (lambda ()
    (assoc-ref #\e (make-inventory (list->string (filter not-whitespace? (string->list sample-text-1)))))))

(test-case "Frequency of b"
  = (* .015 (string-length (list->string (filter not-whitespace? (string->list sample-text-1)))))
  (lambda ()
    (assoc-ref #\b (make-inventory (list->string (filter not-whitespace? (string->list sample-text-1)))))))

(description "sample-text-2")

(define sample-text-2 "TODO: fill me in!")

;; TODO: give your two test cases for sample-text-2 here

(description "sample-text-3")

(define sample-text-3 "TODO: fill me in!")

;; TODO: give your two test cases for sample-text-3 here
