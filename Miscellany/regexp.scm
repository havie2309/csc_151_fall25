;; CSC 151 Fall 2025
;; Lab: Regular Expressions (regexp.scm)
;; Authors: Ella, Vy 
;; Date: November 17th 2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import lab)
(import test)
(import data)
(import rex)

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 1 : Basics")

; While regular expressions are a library feature of Scheme (as well as
; virtually every other programming language on the planet), they are
; complicated enough that we should think of them as forming their own
; little programming language. As such, let's first gain some basic
; fluency with the different constructs of regular expressions as
; introduced in the reading.

; For each of the following descriptions of string patterns, define a
; regex that recognizes that pattern. 

; The relevant procedures you'll need for this problem are:
; rex-any-char
; rex-char-set
; rex-concat
; rex-string
; rex-repeat-0
; rex-any-of


; The string starts with an "a", ends in a "b", and has any three
; characters in the middle.
(define ex-1-1-regex 
  (rex-concat (rex-string "a")
              (rex-any-char)
              (rex-any-char)
              (rex-any-char)
              (rex-string "b")))

(test-case "a   b"
  equal? #t
  (lambda () (rex-matches? ex-1-1-regex "a   b")))
(test-case "aqqqb"
  equal? #t
  (lambda () (rex-matches? ex-1-1-regex "aqqqb")))
(test-case "a1+ob"
  equal? #t
  (lambda () (rex-matches? ex-1-1-regex "a1+ob")))
(test-case "ab"
  equal? #f
  (lambda () (rex-matches? ex-1-1-regex "ab")))
(test-case "aqqqqqqqqqqqqqqb"
  equal? #f
  (lambda () (rex-matches? ex-1-1-regex "aqqqqqqqqqqqqqqb")))

; Strings that contain the substring "aaa".
(define ex-1-2-regex 
  (rex-concat (rex-repeat-0 (rex-any-char))
              (rex-string "aaa")
              (rex-repeat-0 (rex-any-char))))

(test-case "aaa"
  equal? #t
  (lambda () (rex-matches? ex-1-2-regex "aaa")))
(test-case "qaaaq"
  equal? #t
  (lambda () (rex-matches? ex-1-2-regex "qaaaq")))
(test-case "aaa---------"
  equal? #t
  (lambda () (rex-matches? ex-1-2-regex "aaa---------")))
(test-case "happy happy happy"
  equal? #f
  (lambda () (rex-matches? ex-1-2-regex "happy happy happy")))
(test-case "happy haapy"
  equal? #f
  (lambda () (rex-matches? ex-1-2-regex "happy haapy")))

; Strings that contain *either* the substring "aaa" or the substring
; "bbb".
(define ex-1-3-regex 
    (rex-concat (rex-repeat-0 (rex-any-char))
              (rex-any-of (rex-string "aaa") (rex-string "bbb"))
              (rex-repeat-0 (rex-any-char))))

(test-case "aaa"
  equal? #t
  (lambda () (rex-matches? ex-1-3-regex "aaa")))
(test-case "qaaaq"
  equal? #t
  (lambda () (rex-matches? ex-1-3-regex "qaaaq")))
(test-case "bbb---------"
  equal? #t
  (lambda () (rex-matches? ex-1-3-regex "bbb---------")))
(test-case "habby habby habby"
  equal? #f
  (lambda () (rex-matches? ex-1-3-regex "habby habby habby")))
(test-case "habby haaby"
  equal? #f
  (lambda () (rex-matches? ex-1-3-regex "habby haaby")))
(test-case "have you ever said bbbb out loud?"
  equal? #t
  (lambda () (rex-matches? ex-1-3-regex "have you ever said bbbb out loud?")))

; Strings that contain *both* the substring "aaa" and the substring
; "bbb". 
(define ex-1-4-regex 
  (rex-concat (rex-any-of 
                  (rex-concat (rex-repeat-0 (rex-any-char))
                  (rex-string "aaa")
                  (rex-repeat-0 (rex-any-char))
                  (rex-string "bbb")
                  (rex-repeat-0 (rex-any-char)))

                  (rex-concat (rex-repeat-0 (rex-any-char))
                  (rex-string "bbb")
                  (rex-repeat-0 (rex-any-char))
                  (rex-string "aaa")
                  (rex-repeat-0 (rex-any-char))))))

(test-case "zzzbbbzzzaaa"
  equal? #t
  (lambda () (rex-matches? ex-1-4-regex "zzzbbbzzzaaa")))
(test-case "zzzaaazzzbbb"
  equal? #t
  (lambda () (rex-matches? ex-1-4-regex "zzzaaazzzbbb")))
(test-case "zazazazbzbzb"
  equal? #f
  (lambda () (rex-matches? ex-1-4-regex "zazazazbzbzb")))


;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 2 : More Basics")

; Similarly to exercise 1, write down regular expressions for each of
; these patterns of strings. Add a few tests for examples that don't have
; any. 

; The relevant procedures you'll need for this problem are:
; rex-any-char
; rex-char-set
; rex-concat
; rex-string
; rex-repeat-0
; rex-any-of
; rex-empty


; Any string that starts with "a" and has odd length.
(define ex-2-1-regex 
  (rex-concat (rex-string "a")
              (rex-repeat-0 (rex-concat (rex-any-char) (rex-any-char)))))

(test-case "a"
  equal? #t
  (lambda () (rex-matches? ex-2-1-regex "a")))
(test-case "ab"
  equal? #f
  (lambda () (rex-matches? ex-2-1-regex "ab")))
(test-case "abb"
  equal? #t
  (lambda () (rex-matches? ex-2-1-regex "abb")))
(test-case "azzzzzzzzzzzzzz"
  equal? #t
  (lambda () (rex-matches? ex-2-1-regex "azzzzzzzzzzzzzz")))

; Any string that either starts with "a" and has odd length or starts
; with a "b" and has even length.
(define ex-2-2-regex
  (rex-any-of 
      (rex-concat (rex-string "a")
              (rex-repeat-0 (rex-concat (rex-any-char) (rex-any-char))))
    (rex-concat (rex-string "b")
      (rex-any-char) (rex-repeat-0 (rex-concat (rex-any-char) (rex-any-char))))))

(test-case "a"
  equal? #t
  (lambda () (rex-matches? ex-2-2-regex "a")))
(test-case "ab"
  equal? #f
  (lambda () (rex-matches? ex-2-2-regex "ab")))
(test-case "abb"
  equal? #t
  (lambda () (rex-matches? ex-2-2-regex "abb")))
(test-case "azzzzzzzzzzzzzz"
  equal? #t
  (lambda () (rex-matches? ex-2-2-regex "azzzzzzzzzzzzzz")))
(test-case "b"
  equal? #f
  (lambda () (rex-matches? ex-2-2-regex "b")))
(test-case "ba"
  equal? #t
  (lambda () (rex-matches? ex-2-2-regex "ba")))
(test-case "bba"
  equal? #f
  (lambda () (rex-matches? ex-2-2-regex "bba")))
(test-case "bzzzzzzzzzzzzzzz"
  equal? #t
  (lambda () (rex-matches? ex-2-2-regex "bzzzzzzzzzzzzzzz")))


; The empty string (*i.e.*, a string containing no characters).
(define ex-2-3-regex 
  (rex-string ""))

(test-case "empty string"
  equal? #t
  (lambda () (rex-matches? ex-2-3-regex "")))
(test-case "non-empty string"
  equal? #f
  (lambda () (rex-matches? ex-2-3-regex "nonempty")))


; Any non-empty string.
(define ex-2-4-regex
  (rex-repeat (rex-any-char)))

(test-case "empty string"
  equal? #f
  (lambda () (rex-matches? ex-2-4-regex "")))
(test-case "non-empty string"
  equal? #t
  (lambda () (rex-matches? ex-2-4-regex "nonempty")))


;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 3 : The joys of special characters")



; (a) Now let's tackle a potentially frustrating aspect of regular
; expressions: special characters and escape sequences. First, recall
; that strings are data that represent sequences of characters. We write
;*string literals*, values of the string type, using quotes, *e.g.*,
; `"hello world!"`

; Suppose that we wanted to write a string literal that contains quotes.
; For example, we want to manipulate this line of text in a Scheme
; program:

;    They said "we should really consider getting out of here!"

; In the space below, briefly describe the problem with the following
; string literal.  Complete the definition of sample-text-1 below,
; fixing the problem that you identified.

; (define sample-text-1
;    "They said \"we should really consider getting out of here!\"")

(description "The problem is that the quotes indicate starting and ending a string, and
  this could cause issues with representing the whole string. ")

(define sample-text-1  "They said \"we should really consider getting out of here!\"")

; (b) Now suppose that we wanted to define a string that contains
; backslashes, *e.g.*, the following text:

;   In the LaTeX typesetting system, we italicize text with the
;      \emph{...} command!

; In the space below, report what happens when you use the following string
; literal. Complete the definition of sample-text-2 below,
; fixing the problem that you identified.
; When we use the string literal, it removes the char after backslash. We fixed this by adding an
; extra backslash. 

  ; (define sample-text-2 
  ;   "In the LaTeX typesetting system, we italicize text with the \emph{...} command!")

(define sample-text-2
  "In the LaTeX typesetting system, we italicize text with the \\emph{...} command!")

; (c) Escape sequences are particularly difficult to deal with in
; regular expression values because slashes are *also* used to escape
; special characters that appear in them!

; With this in mind, try writing regular expressions that recognize the
; following patterns of strings. Add additional tests to
; verify the correctness of your constructions.


; Any string that starts with a left-square bracket `[` and ends with a
; right-square bracket `]` and any amount of numeric digits in-between.
(define ex-3-1-regex 
  (rex-concat (rex-string "\[") 
              (rex-repeat-0 (rex-char-set "0123456789"))
              (rex-string "\]")))

(test-case "ex-3-1 simple example"
  equal? #t
  (lambda () (rex-matches? ex-3-1-regex "\[454\]")))

(test-case "ex-3-1 more numbers"
  equal? #t
  (lambda () (rex-matches? ex-3-1-regex "\[123456\]")))

(test-case "ex-3-1 simple example"
  equal? #f
  (lambda () (rex-matches? ex-3-1-regex "\[abcd\]")))

; Any string that starts and ends with a backslash and contains 
; any amount of numeric digits in between.
(define ex-3-2-regex
    (rex-concat (rex-string "\\") 
              (rex-repeat-0 (rex-char-set "0123456789"))
              (rex-string "\\")))

(test-case "ex-3-2 simple example"
  equal? #t
  (lambda () (rex-matches? ex-3-2-regex "\\454\\")))

(test-case "ex-3-2 more number"
  equal? #t
  (lambda () (rex-matches? ex-3-2-regex "\\75930\\")))

(test-case "ex-3-2 with space"
  equal? #f
  (lambda () (rex-matches? ex-3-2-regex "\\ \\")))


;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 4 : My Dear")

; (a) Project Gutenberg (https://www.gutenberg.org/) provides an
; extensive collection of public domain books in a variety of forms,
; including "plain text".

; For this exercise, we'll look at Arthur Conan Doyle's The Adventure of
; Sherlock Holmes.  You may download it directly from Gutenberg via this
; URL: https://www.gutenberg.org/files/1661

; Open the book in Scamper and inspect its contents. Make sure that
; you have a plain text file that contains the book in question. With
; your partner, note the structure of the text file.
; Manually remove the preamble of the text so that 
; the file begins with the actual book contents, *i.e.*, the title page.

; Now let's extract text from the book and do some work with it! As a
; warm-up, use the file reading functions of the `data` library to
; determine the total number of words in this book. You may like to 
; review the data tab in the Scamper docs to remember options
; available for processing data. 

(with-file "1661-0.txt"
  (lambda (text)
  (let* ([words (string->words text)])
    (length words))))

; (b) Now for some fun!  If you are not aware, one of Sherlock Holmes 
; famous lines is "My dear ..." where the '...' is frequently "Watson", 
; his best friend and assistant.  However, who else does Sherlock reference 
; when he says this phrase?

; In this problem, you will use regular expressions to produce a list of 
; all occurences of 
    ; My dear ...
; in the Adventures of Sherlock Holmes where the '...' is the word 
; immediately following the phrase. You can assume that this whole 
; phrase is whitespace-separated and that the follow-up word is made up
; solely of letters.

; (Hint: In this problem, we don't want to break up the text before
;  matching it. We will also need rex-find-matches.)

(define dear
  (rex-concat (rex-any-of (rex-string "My dear") 
                          (rex-string "MY DEAR")
                          (rex-string "my dear"))
              (rex-string " ")
               (rex-repeat (rex-char-antiset " !"))
               (rex-char-set ",! ”")))
                
(with-file "1661-0.txt"
  (lambda (text)
    (rex-find-matches dear text)))

; (c) After writing (b), you hopefully ran your code to
; see if your function worked, and hopefully, you found some hits!
; However, did you capture everything?  Open the book in the browser
; and use the manual find feature (ctrl-f or cmd-f on mac) to look for
; occurrences of "My dear" in the text. (You might have to go to the .txt
; webpage rather than use Scamper for the find to work.)

; Try to locate the occurrences that your function found.  Likely, you
; will find additional occurrences that your function did not catch.  Go
; back and modify `my-dear-occurrences` to capture these instances, if
; possible.  Continue until you are satisfied that you have found
; everything.

; (d) In the space below, report the occurrences that the final version
; of your function reports.  For reference, my version of the code found
; 32 occurrences, but you may be able to find more!

(define matches-list (list "My dear Holmes,” " "MY DEAR MR. " 
  "my dear sir,” " "my dear Watson,” " "my dear Watson, " 
  "my dear sir. " "My dear doctor, " "My dear fellow,” "
  "My dear fellow, " "My dear fellow, " "my dear girl. " 
  "my dear little " "My dear madam,” " "My dear wife "
  "My dear fellow, " "My dear Holmes!" "My dear fellow, " 
  "my dear madam,” " "My dear Holmes!" "my dear Watson, " 
  "My dear fellow, " "MY DEAR MR. " "My dear Holmes!" 
  "my dear wife " "my dear sir,” " "my dear boy
to " "my dear fellow, " "my dear young " "my dear young " 
  "My dear young " "my dear young " "My dear Watson, "))

(length matches-list)

(description "I was able to find 32 occurences of 'My dear...' in the
  given text. See above for the final list and list length. ")



;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Extra problem ")

; If you have extra time and/or want extra practice, think of an
; additional question that you might ask about the text and build a
; function that loads the file into an appropriate data structure and
; uses regular expressions to answer that question!


