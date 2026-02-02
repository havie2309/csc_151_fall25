; testing
;; CSC-151-NN A(SEMESTER)
;; Lab: More Fun with Lists
;; Authors: Hoang Nguyen, Vy Nguyen, Arham Areeb
;; Date: 09/17/2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import lab)
(import test)

(title "Lab: More Fun with Lists")

; +-------------+----------------------------------------------------
; | Preparation |
; +-------------+

; In this lab, you and your partner will practice manipulating lists
; using the big-three higher-order functions---map, filter, and
; reduce---as well as some other useful functions.

; In this lab, alternate drivers on every sub-problem. Both the driver and
; navigator should be talking about their ideas!

; The person with the problem description should drive and their
; partner should navigate.  Make sure to be good partners and
; focus completely on solving the current problem together rather than
; working ahead on your own.

; a. If you haven't done so yet, make sure to open tabs in your browser
; for the lab and the recent readings.

; b. Don't forget to save this file as `lists-more.scm`!


(part "Exercise 1: Manipulating a list")

; (alternate sides for each part!) 

; Complete each of the definitions that manipulate ex-1-list in various
; sorts of ways using one or more of the primary list functions.  You
; may also use other standard library functions for lists (e.g.,
; length) when appropriate.

; Please write expressions that compute the result, rather than computing
; the result by hand.

(define ex-1-list (list 25 25 23 5 21 20 20 18 10 1 22 21))

(problem "a. ex-1-list-adjusted")

; Increments the value of each element of the list `ex-1-list` by 5
(define ex-1-list-adjusted
  (map (section + _ 5) ex-1-list)
  )

(test-case "Adjusting the list"
  equal? (list 30 30 28 10 26 25 25 23 15 6 27 26)
  (lambda () ex-1-list-adjusted))

(problem "b. ex-1-list-filtered")

; Keeps only the elements of the list that are greater than 10.
(define ex-1-list-filtered
  (filter (section > _ 10) ex-1-list)
  )

(test-case "ex-1-list-filtered"
  equal? (list 25 25 23 21 20 20 18 22 21)
  (lambda () ex-1-list-filtered))

(problem "c. ex-1-list-average")

; Computes the average of the list (Hint: this computation is more
; than just a single call to reduce!)
(define ex-1-list-average
  (/ (reduce + ex-1-list) (length ex-1-list)
  ))

(test-case "ex-1-list-average"
  equal? (/ 211 12)
  (lambda () ex-1-list-average))

(problem "d. Counts the odd values")

(define ex-1-odd-count
  (length (filter (section odd? _) ex-1-list))
  )

(test-case "ex-1-odd-count"
  equal? 7
  (lambda () ex-1-odd-count))

; For e. and f., you'll need a function that sorts. There is one in
; the standard library! Check out the documentation to see how to
; use it!

(problem "e. Puts them in inceasing order, from smallest to largest")

(define ex-1-increasing
  (sort ex-1-list <=)
  )

(test-case "ex-1-increasing"
  equal? (list 1 5 10 18 20 20 21 21 22 23 25 25)
  (lambda () ex-1-increasing))

(problem "f. Puts them in decreasing order, from largest to smallest")

(define ex-1-decreasing
   (sort ex-1-list >=)
  )

(test-case "ex-1-decreasing"
  equal? (list 25 25 23 22 21 21 20 20 18 10 5 1)
  (lambda () ex-1-decreasing))

; -------------------------------------------------------------------

(part "Exercise 2: Higher-order corners")

; (alternate sides for each part!) 

; Suppose we want to add 5 to every element of a list.  Some of you
; may have tried the following.
;
; > (define numbers (list 3 1 4 1 5 9 2 6))
; > (define numbers-plus-5a (map + numbers 5))

; Why doesn't that work? Fill your answer into the description
; below.

(problem "Why doesn't `(map + numbers 5)` work?")

(description "It doesn't work because map only takes two types of parameters, 
  a procedure and a list")

(problem "numbers-plus-five with section")

; As we hope you've learned, you can use `section` to help with
; this problem.  Write an expression using `section` that adds
; five to each element of `numbers`.

(define numbers (list 3 1 4 1 5 9 2 6))
(define numbers-plus-five-b
  (map (section + _ 5) numbers)
  )

(test-case "numbers-plus-5-b"
  equal? (list 8 6 9 6 10 14 7 11)
  (lambda () numbers-plus-five-b))

(problem "numbers-plus-five without section")

; Suppose you had not learned `section`.  There are still at least
; two other ways to to add five to each element of a list.  Come up 
; with two, at least one of which uses the three-parameter `map`.

(define helper
  (lambda (x)
    (+ x 5)))

(define helper3
  (lambda (x)
    (+ x 3)))

(define helper2
  (lambda (x)
    (+ x 2)))


(define numbers-plus-five-c1
    (map helper numbers))

(define numbers-plus-five-c2
 (map (o helper2 helper3) numbers)
  )

(test-case "numbers-plus-5-c1"
  equal? (list 8 6 9 6 10 14 7 11)
  (lambda () numbers-plus-five-c1))

(test-case "numbers-plus-5-c2"
  equal? (list 8 6 9 6 10 14 7 11)
  (lambda () numbers-plus-five-c2))

(problem "Preferences...")

; Which of those definitions do you most prefer?  Why?

(description "I like definition 1 because it's more compact")

; -------------------------------------------------------------------

(part "Exercise 3: Manipulating another list")

; (alternate sides for each part!) 

; Complete each of the definitions that manipulate ex-1-list in various sorts
; of ways using one or more of list functions.  You may also use other standard
; library functions for lists (e.g., length) when appropriate. At times,
; you may find it useful to write helper procedures.

; Please write expressions that compute the result, rather than computing
; the result by hand.

(define ex-3-list
  (list "someone" "suggests" "that" "something" "may" "be" "smart" "&" "snarky"))

(problem "a. ex-3-title-case")

; Convert each word to title case, the first character should be capital.
(define convert-to-title-case (lambda (word)(
  string-append (string-upcase (substring word 0 1)) (substring word 1 (string-length word))
)))

(define ex-3-title-case
    (map convert-to-title-case ex-3-list)
  )

(test-case "Titlecasing elements"
  equal? (list "Someone" "Suggests" "That" "Something" "May" "Be" "Smart" "&" "Snarky")
  (lambda () ex-3-title-case))

(problem "b. ex-3-s-words")

; Keeps only the elements of the list that start with s
(define check-s? (lambda (word)(
  string=? "s" (substring word 0 1)
)))

(define ex-3-s-words
  (filter check-s? ex-3-list)
  )

(test-case "Selecting s words"
  equal? (list "someone" "suggests" "something" "smart" "snarky")
  (lambda () ex-3-s-words))

(problem "c. ex-3-smushed")

; Joins the words together without worrying about spaces

(define ex-3-smushed
   (reduce (section string-append _ "" _ ) ex-3-list)
  )

(test-case "smushed together"
  equal? "someonesuggeststhatsomethingmaybesmart&snarky"
  (lambda () ex-3-smushed))

(problem "d. ex-3-spaced")

; Joins the words together with spaces in between them

(define ex-3-spaced
 (reduce (section string-append _ " " _ ) ex-3-list)
  )

(test-case "spaced out"
  equal? "someone suggests that something may be smart & snarky"
  (lambda () ex-3-spaced))

(problem "e. ex-3-short-count")

; Counts how many words have four or fewer letters

(define ex-3-short-count
  (length (filter (section >= 4 (string-length _)) ex-3-list)
  ))

(test-case "short count"
  equal? 4
  (lambda () ex-3-short-count))

(problem "f. ex-3-alphabetical")

; Puts them in alphabetical order

(define ex-3-alphabetical 
  (sort ex-3-list string<=?)
  )

(test-case "alphabetical"
  equal? (list "&" "be" "may" "smart" "snarky" "someone" "something" "suggests" "that")
  (lambda () ex-3-alphabetical))
  
(problem "g. ex-3-by-size")

; Puts them in order from shortest to longest. Note that you may want to
; write a separate procedure that compares two strings by size.
(define comp (lambda (str1 str2)(
  if (<= (string-length str1) (string-length str2)) #t #f
)))

(define ex-3-by-size
  ; TODO: fill me in!
   (sort ex-3-list comp)
  )

(test-case "by size"
  equal? (list "&" "be" "may" "that" "smart" "snarky" "someone" "suggests" "something")
  (lambda () ex-3-by-size))

; -------------------------------------------------------------------

(part "Exercise 4: Exercises in reduction")

; (alternate sides for each part!)

(problem "4a: Mystery reduction")

; What does the following procedure do?

(define combine
  (lambda (s1 s2)
    (string-append s1 " and " s2 " and " s1)))

(description "(combine s1 s2)... <TODO: fill me in!>")

(problem "4b: Exploring the mystery")

; What value do you expect to get for the following expressions?
; Check each as you go by displaying the identifier below each
; description.

(define combine-2 (reduce combine (list "A" "B")))
 combine-2

(description "combine-2 = A and B and A")

(define combine-1 (reduce combine (list "A")))
combine-1

(description "combine-1 = A")

(define combine-3-left (reduce combine (list "A" "B" "C")))
combine-3-left

(description "combine-3-left = A and B and A and C and A and B and A")

(define combine-3-right (reduce-right combine (list "A" "B" "C")))
combine-3-right

(description "combine-3-right = A and B and C and B and A")

(define left-and-right (equal? combine-3-left combine-3-right))

(description "left-and-right = #f")

(problem "4c: Even more mysteries!")

; What values do you expect for all-the-same below?  Why?
; Don't check yet!  Wait until you get to part d.

(define combine-abcde-1 (reduce combine (list "A" "B" "C" "D" "E")))
combine-abcde-1

(description "combine-abcde-1 = A and B and A and C and A and B and 
  A and D and A and B and A and C and A and B and A and E and A and B and A and C and A and 
  B and A and D and A and B and A and C and A and B and A")

(define combine-abcde-2 (reduce combine (list "A" "B" "C" "D" "E")))
combine-abcde-2

(description "combine-abcde-2 = A and B and A and C and A and B and A and D and A and B and A and C and A 
  and B and A and E and A and B and A and C and A and B and A and D and 
  A and B and A and C and A and B and A")

(define combine-abcde-3 (reduce combine (list "A" "B" "C" "D" "E")))
combine-abcde-3

(description "combine-abcde-3 = A and B and A and C and A and B and A and D and A and B and A and C 
  and A and B and A and E and A and B and A and C and A and
  B and A and D and A and B and A and C and A and B and A")

(define all-the-same 
  (and (equal? combine-abcde-1 combine-abcde-2)
       (equal? combine-abcde-1 combine-abcde-3)
       (equal? combine-abcde-2 combine-abcde-3)))
all-the-same 

(description "all-the-same = #t")

(problem "4d: Mystery unveiled!")

; Check your answer experimentally.  That is, print out the value
; of `all-the-same` and see if it's what you expected.
;
; What do you take as the main lessons of this exercise?

(description "From this exercise, I learned how to write compartors for the sort functions, how to use sections and compose. 
  I also learned that sometimes functions can have unexpected results.")

; -------------------------------------------------------------------

; Congratulations on finishing this lab!  To turn in your work, submit
; this file (lists-more.scm) to Gradescope. If you have time, please
; complete the exercises below before you turn in your work.

; -------------------------------------------------------------------
; | For those with extra time |
; +---------------------------+

; EXTRA PROBLEM 1 : Reverse individual words

; Suppose that we have a string of words separated by spaces. For
; example "Today is a great day". Write a procedure to give a new string
; with each of the words reversed. The spaces should be preserved. So
; for our example the output should be "yadoT si a taerg yad"



; EXTRA PROBLEM 2 : Timestamps, revisited


; Suppose that we have a bunch of timestamps, which represent number of seconds 
; elapsed since midnight on January 1, 1970. We will use these timestamps 
; to compute the day of the week.

; Represent Sunday as 0, Monday as 1, ...., and Saturday as 6. 

; Suppose we have this list of timestamps:
(define timestamp-list (list 233366400 1757965983))
; Write a procedure or expression which will return a list of integers corresponding
; to the days of the week that each timestamp corresponds to. 
; The answer should be (list 3 1 5)


; EXTRA PROBLEM 3: Compose vs. Pipelining

; Consider the problem of taking a string, appending two exclamation
; marks on the end, reversing it (using your procedure from EXTRA PROBLEM 1),
; and then turning the string into a list. 

; "hello" becomes (list #\! #\! #\o #\l #\l #\e #\h)

; Extra 2a : Solve this problem using compose



; Extra 2b : Solve this problem using pipelining




; -------------------------------------------------------------------



