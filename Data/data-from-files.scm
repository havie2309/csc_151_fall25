;; CSC 151 (SEMESTER)
;; Lab: Data From Files
;; Authors: Vy Nguyen, Hoang Nguyen
;; Date: 10/03/2025
;; Acknowledgements:
;;   https://www.kaggle.com/datasets/brsdincer/chernobyl-chemical-radiation-csv-country-data

(import test)
(import data)
(import lab)

; +-------------+----------------------------------------------------
; | Preparation |
; +-------------+

; Make sure to save this file as data-from-files.scm.

; Throughout this lab, after completing a problem feel free to 
; "comment out" your code in order to keep the output
; pane not unbearably long.

; If you haven't already downloaded it, later in the lab you'll
; need the file on the Chernobyl Radiation. 
; https://eikmeier.sites.grinnell.edu/csc-151-fall-2025/files/Chernobyl_Chemical_Radiation.csv

; +-------------------------------+--------------------------------
; | Exercise 1: Text Data         |
; +-------------------------------+
(problem "Problem 1: Text Data")
; Project Gutenberg (https://www.gutenberg.org/) provides an extensive
; collection of public domain books in a variety of forms, including 
; "plain text".

; (a) 
; Navigate to the Project Gutenberg Web site and download a few books 
; in plain text format.  Strive for short- to medium-length books.  
; JaneEyre (https://www.gutenberg.org/ebooks/1260) is okay.  
; The Complete Works of William Shakespeare(https://www.gutenberg.org/ebooks/100)
; is not.

; As long as they are short to medium length, you can upload them
; to your Scamper browser. 

; <TO-DO>: Add a comment below to list the books you've chosen.
; THE JUNGLE BOOK By Rudyard Kipling
; JaneEyre



;(b)
; Pick one of the books you've downloaded and write instructions
; to read the characters, words, and lines from the book. 
; Save the first 20 characters, the first 10 words, and 
; the first 5 lines from the book.

(with-file "pg236.txt"
  (lambda (x)
    (let* ([lines (string->lines x)]
        [first-lines (list-take lines 5)]
        [words (string->words x)]
        [first-words (list-take words 10)]
        [letters (string->chars x)]
        [first-letters (list-take letters 20)])
    (list first-lines first-words first-letters))))



; Note: You may discover that Project Gutenberg adds a header.

; +-------------------------------+--------------------------------
; | Exercise 2: Tallying          |
; +-------------------------------+
(problem "Problem 2: Tallying")

;  In this question you'll need to use `tally-all`. Look the 
; procedure up in the docs if you don't remember how
; it works. 

; (a)
; Write code  to tally the first 10,000 words in your chosen text. 

;(with-file "pg236.txt"
;  (lambda (x)
 ;   (let* ([words (string->words x)]
  ;          [first-words (list-take words 10000)])
   ; (tally-all first-words))))



; (b)
; We'd like to order the tally data so that the most frequently
; used words appear at the beginning of the list. Write a 
; comparator which takes in two pairs of the form 
; (pair string? number?) and returns #t if the first number is 
; bigger than the second number.

(define comp 
  (lambda (pair1 pair2)
    (> (cdr pair1) (cdr pair2))))


; (c) 
; Use your comparator from part b to sort the tallied list. 
; You may find it useful to start with a copy of your solution
; from part a. 

;(with-file "pg236.txt"
 ; (lambda (x)
  ;  (let* ([words (string->words x)]
   ;         [first-words (list-take words 10000)])
    ;(sort (tally-all first-words) comp))))
; (d)
; Find the five most-frequently-used non-trivial words. 
; You don't need to write expressions to get these, you can 
; look at the first part of your sorted list from part c
; to identify them. The definition of non-trivial is up to you.

; <TO-DO>: "Bagheera" "Akela" "Khan" "Baloo" "Mowgli"
 


; +-----------------------------------------+--------------------------------
; | Exercise 3: Simple explorations of data |
; +-----------------------------------------+
(problem "Problem 3: Simple Explorations")

; The Chernobyl data comes from Kaggle, and you can 
; read all about the dataset on their website: 
; https://www.kaggle.com/datasets/brsdincer/chernobyl-chemical-radiation-csv-country-data

; In this table, each row has the following pieces, and all 
; values are stored as a string. 
; (list country-code city-name longitude latitude date
;        I_131 Cs_134 Cs_137 )
; The last three elements in that list are likely unfamiliar.
; These represent the concentration of Iodine-131 (I-131), 
; Caesium-134 (Cs-134), and Caesium-137 (Cs-137) as 
; aerosol particles. 

; (a)
; Determine how many entries (rows) there are in the 
; Chernobyl table.

(with-file "Chernobyl_Chemical_Radiation.csv"
  (lambda (x)
    (length (parse-csv x))))


; (b)
; Determine the last few entries in the Chernobyl table. 

; Hint: Use `list-drop`.

; Write your expression and answer below

(with-file "Chernobyl_Chemical_Radiation.csv"
  (lambda (x)
    (list-drop (parse-csv x) 2000)))


; (c) 
; Find the middle five elements of the Chernobyl table.  
; (You need not be exact, just close.)

; Hint: Use `list-drop` and `list-take` with appropriate values.

; Write your expression and answer below
(with-file "Chernobyl_Chemical_Radiation.csv"
  (lambda (x)
    (list-take (list-drop (parse-csv x) 1021) 5)))


; +-----------------------------+--------------------------------
; | Exercise 4: Sorting by date |
; +-----------------------------+
(problem "Problem 4: Sorting by date")

; In this table, dates are stored as a string such as "YY/MM/DD"
; Let's work towards sorting the table so that the rows appear
; in chronological order

; (a) 
; Write a procedure which returns a list of the year, month, date
; all as numbers. Note, you do not yet need to do anything
; with the file.

(define ex-row 
  (list "GR" "KOZANIS" "21.79" "40.31" "86/05/21" "0.01" "0" "0"))
  
  
;;; (return-date lst) -> list?
;;;   lst: list?, in the format of one row of the 
;;;               Chernobyl dataset
;;; Return type is a list of three numbers: the year,
;;; the month, and date of the event.
(define return-date
  (lambda (lst)
    (map string->number (string-split (list-ref lst 4) "/"))))

(test-case "straightforward example"
  equal? (list 86 05 21)
  (lambda () (return-date ex-row)))

; (b)
; Next, we need to create a comparator that will take in two
; of these dates and return #t if the first one comes before 
; the second.
; For example (list 86 05 21) comes before (list 89 05 21)
; because the year 1986 came before 1989

;;; (date<? date1 date2) -> boolean?
;;;    date1: list?, length 3, numbers, (year day month)
;;;    date2: list?, length 3, numbers, (year day month)
;;; Returns #t if date1 is chronologically before
;;; date2.
(define day
  (lambda (date1 date2)
    (cond [(<= (list-ref date1 1)(list-ref date2 1)) #t]
          [(> (list-ref date1 1)(list-ref date2 1)) #f])))

(define month
  (lambda (date1 date2)
    (cond [(< (list-ref date1 1)(list-ref date2 1)) #t]
          [(> (list-ref date1 1)(list-ref date2 1)) #f]
          [else (day date1 date2)])))

(define year
  (lambda (date1 date2)
    (cond [(< (list-ref date1 0)(list-ref date2 0)) #t]
          [(> (list-ref date1 0)(list-ref date2 0)) #f]
          [else (month date1 date2)])))


(define date<?
  (lambda (date1 date2)
    (year date1 date2)))
  
(test-case "given example"
  equal? #t
  (lambda () (date<? (list 86 05 21) (list 89 05 21))))

(test-case "same year"
  equal? #f
  (lambda () (date<? (list 86 05 21) (list 86 03 27))))

(test-case "same year and same month"
  equal? #t
  (lambda () (date<? (list 86 05 18) (list 86 05 21))))

; (c) 
; Next, we need to be able to apply this comparator to a full 
; row from the Chernobyl table. You'll need the solutions 
; from both a and b.


;;; (listdate<? row1 row2) -> boolean?
;;;   row1: list?, in the format of a row from the Chernobyl data
;;;   row2: list?, in the format of a row from the Chernobyl data
;;; Returns #t if the date in row1 is chronologically before
;;; the date in row2.
(define listdate<?
  (lambda (row1 row2)
    (date<? (return-date row1)(return-date row2))))
  
(test-case "tried and true example"
  equal? #t
  (lambda () (listdate<? 
    (list "GR" "KOZANIS" "21.79" "40.31" "86/05/21" "0.01" "0" "0") 
    (list "GR" "KOZANIS" "21.79" "40.31" "89/05/12" "0.01" "0" "0"))))
    

; (d)
; Finally, sort the Chernobyl table by date! Verify your sorting
; works by examining the first few lines, the last few lines, 
; and some lines in the middle. 

(with-file "Chernobyl_Chemical_Radiation.csv"
  (lambda (x)
    (let* ([lines (parse-csv x)]
          [drop-first (list-drop lines 1)]
          [first-lines (list-take drop-first 5)])
    (sort first-lines listdate<?))))

(with-file "Chernobyl_Chemical_Radiation.csv"
  (lambda (x)
    (let* ([lines (parse-csv x)]
          [drop-first (list-drop lines 1)]
          [first-lines (list-drop drop-first 2047)])
    (sort first-lines listdate<?))))

(with-file "Chernobyl_Chemical_Radiation.csv"
  (lambda (x)
    (let* ([lines (parse-csv x)]
          [drop-first (list-drop lines 1)]
          [first-lines (list-take (list-drop drop-first 1021) 5)])
    (sort first-lines listdate<?))))

; Note: You'll have to drop the first line of the table 
; before sorting it. 

; +-------------------------+--------------------------------
; | Exercise 5: Finding Data|
; +-------------------------+
(problem "Problem 5: Finding Data")
  
; With more than 2000 lines in the Chernobyl file, 
; it will be hard for us to manually find any particular 
; entry.  But that's why we have computers.

; As you know from the reading, the `assoc-ref` procedure 
; will search a list of pairs based on the
; first value in each pair.  The following procedure was given
; in the reading as helpful in getting the data in the correct
; form

;;; (key-pair lst) -> pair?
;;;    lst: list?, non-empty
;;; Change the contents of lst into a pair where the first
;;; element of the pair is the first element of lst, and the second
;;; element of the pair is the remainder of lst.
(define key-pair
  (lambda (lst)
    (pair (car lst) (cdr lst))))

; For example, we can find an entry with key "F" by 
; with (assoc-ref "F" data-in-key-pair-form)`

; (a)
; Use `assoc-ref` and the `key-pair` procedure
; to find an entry in the table with key "F"
; ("F" = France)
(with-file "Chernobyl_Chemical_Radiation.csv"
  (lambda (x)
    (let* ([data (parse-csv x)]
           [key-pair (map key-pair data)])
       (assoc-ref "F" key-pair)
       )))

; (b)
; What happens if you use `assoc-ref` on the original 
; ordering of the data versus the data which is sorted
; by date. Do you get the same entry from the table?
; <TO-DO: Enter your answer here>

; No, I don't get the same entry from the table. 

; (c)
; Give an explanation for your answer for part b. 
; What can we infer about `assoc-ref`?
; <TO-DO: Enter your answer here>

; I don’t get the same entry from the table because sorting the list by date changes 
; the order of the key–value pairs. The function assoc-ref searches the association 
; list from the beginning and returns the first matching entry for the given key. 
; Therefore, if the same key appears more than once in the list, changing the order 
; can cause assoc-ref to return a different entry.



; -------------------------------------------------------------------
; Congratulations on finishing this lab!  To turn in your work, submit
; this file (data-from-files.scm) to Gradescope, and also the .txt
; file you used in Problems 1 and 2. If you have time, please
; complete the exercises below before you turn in your work.
; -------------------------------------------------------------------


; -------------------------------------------------------------------
; | For those with extra time |
; +---------------------------+

; If you find that you have extra time, you may try any of the following.
; (That is, you need not do them in order from first to last.)

; +-------------------------------+--------------------------------
; | Extra 1: Extreme cities    |
; +-------------------------------+
; (a)
; Find the entries for the five largest values for the radiation
; of any of the three recorded values in the data.

; (b) 
; Find the entries for the five smallest values for the 
; radiation of any of the three recorded values in the data.

; (c)
; How "extreme" are your answers from parts a and b? 
; Are they very far away from the average recorded 
; radiation levels? Or relatively close?


; +-----------------------------------+--------------------------------
; | Extra 2: The beginning and the end|
; +-----------------------------------+
; In an earlier exercise, you found the five most-frequently-used
; non-trivial words in the first 10,000 words of the text.  Determine
; how frequently each of those words is used in the last 10,000 words of
; the text.

; +-----------------+--------------------------------
; | Extra 3: Digrams|
; +-----------------+
; In analyzing texts, it is often useful to create "digrams", pairs
; of neighboring elements in the text.  

; (a)
; Create a list of the first 1001 words in your text.

; (b)
; Using that list, create a list of the first 1000 word digrams in the
; text. 

; Hint: Use `(map (string string_apppend _ " " _) ... ...)`.

; (c)
; Can you figure out which word most frequently follows "the"?

; (d) 
; Is your answer different if your first convert the words to
; lowercase using `string-downcase`?


; +--------------------------------+--------------------------------
; | Extra 4: More tests
; +------------------------------=-+

; Write more tests for problem 4b. We can think of several
; distinct cases not covered already by the existing tests.


; +--------------------------------+--------------------------------
; | Extra 5: Get Creative
; +------------------------------=-+

; What else might be interesting to know about the Chernobyl
; dataset? Write out questions, and sketch how you might
; find the answers to your questions. You can attempt to 
; write the code too if you're feeling ambitious.

; +--------------------------------+--------------------------------
; | Extra 6: Who needs punctuation?|
; +------------------------------=-+

; While the list of characters we've received from `string->chars` is 
; comprehensive, it contains a lot of characters we might not want.  
; Let's consider a way to get a more "useful" set of characters.

; (a) 
; Write instructions to create a single string that consists of the
; first five hundred or so words of the text, separated by spaces.

; (b)
; Convert that string to a list of characters using `string->list`.

; (c)
; Using those characters, create a list of two-letter digrams.

; (d)
; Reflect on how those digrams might be useful


