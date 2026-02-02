; structs

; structs.scm
;; CSC-151-NN (fall)
;; Lab: Structs (structs.scm)
;; Authors: Sarah Kim, Vy Nguyen
;; Date: 11/14/2025
;; Acknowledgements:
;;   professor Eikmeier, mentor

(import lab)
(import test)

;; In the reading, you learned about data abstraction and precondition
;; verification. In particular, you saw how we design a custom data type, e.g.,
;; a name, and separate the implementation of that data type from how we use
;; it.
;;
;; In today's lab, we'll introduce one last Scheme construct, the struct, which
;; allows us to define our own data types in a concise, elegant manner!

;; -----------------------------------------------------------------------------
(problem "Problem 1: Introduction to Structs")

;; Recall from the reading that we developed a data type to represent a name.
;; We defined a name as a collection of strings: 
;;
;; A prefix (optional)
;; A given name (required)
;; A middle name (optional)
;; A family name (optional)
;; A suffix (optional)
;;
;; Only the given name is required to create a name. We implemented this
;; data type as a list of strings, a vector of strings, and finally an
;; association list mapping strings to strings. To do so, we had to implement
;; a number of functions over our name data type:
;;
;; (name prefix given middle family suffix) -> name?
;; (name? value) -> boolean?
;; (name-prefix name) -> string?
;; (name-given name) -> string?
;; (name-middle name) -> string?
;; (name-family name) -> string?
;; (name-suffix name) -> string?
;;
;; It was laborious to implement these functions by hand regardless of the
;; implementation we chose. Ideally, we could specify what was contained inside
;; of our data type and Scheme would take care of the rest. Luckily, Scheme
;; has a construct that does this! A struct declaration defines a custom data
;; type that is composed of a number of named values, called fields. Here is
;; the struct declaration for our name:

(struct name (prefix given middle family suffix))

;; A struct declaration has the following syntax:
;;
;; (struct <identifier> (<identifier> ... <identifier>))
;;
;; The first identifier is the name of the data type we are constructing. The
;; remaining identifiers are the names of the fields of the data type. The
;; struct declaration automatically generates all of the functions that we
;; previous implemented by hand, yay!
;;
;; Let's first observe and appreciate this first point! Complete the
;; definition of an example name from the reading:
;;
;; Admiral Grace Murray Hopper
;; ↑       ↑     ↑      ↑
;; prefix  │     │      │
;;         given │      │
;;               middle │
;;                      family
;;
;; And the name has no suffix (so it is #f).

(description "Admiral Grace Hopper test cases")

(define admiral-hopper
  (name "Admiral" "Grace" "Murray" "Hopper" #f))

;; And now, write test cases validating that all of the remaining
;; name functions work for this example name:
;;
;; (name? value) -> boolean?
;; (name-prefix name) -> string?
;; (name-given name) -> string?
;; (name-middle name) -> string?
;; (name-family name) -> string?
;; (name-suffix name) -> string?

;; TODO: write your test-case definitions here, one per function!
(test-case "Admiral Grace Murray Hopper"
  equal? #t
  (lambda () (name? admiral-hopper)))

(test-case "admiral-hopper/prefix"
  equal? "Admiral"
  (lambda () (name-prefix admiral-hopper)))
(test-case "admiral-hopper/given"
  equal? "Grace"
  (lambda () (name-given admiral-hopper)))
(test-case "admiral-hopper/middle"
  equal? "Murray"
  (lambda () (name-middle admiral-hopper)))
(test-case "admiral-hopper/family"
  equal? "Hopper"
  (lambda () (name-family admiral-hopper)))
(test-case "admiral-hopper/suffix"
  equal? #f
  (lambda () (name-suffix admiral-hopper)))

;; -----------------------------------------------------------------------------
(problem "Problem 2: Printing Names")

;; In the reading exercise for today, you were tasked to write a function
;; (name->string name) that returns the string representation of a name.
;; Because your implementation of this function only depended on the
;; interface of the name data type and not its implementation, it should
;; "just work" for our new struct-based name!
;;
;; Additionally, give a few additional examples of names and test-cases for
;; name->string. Include different names that include some fields of the
;; struct but not others. Recall that our data type says that any such
;; fields that are not present should be marked #f.

; (define name->string
;   (lambda (name)
;     ; TODO: fill me in!
;     ""))

(define name->string
  (lambda (name)
    (let* ([parts (list (name-prefix name)
                       (name-given name)
                       (name-middle name)
                       (name-family name)
                       (name-suffix name))])
          (reduce (section string-append _ " " _) (filter string? parts)))))

(test-case "admiral-hopper"
  equal? "Admiral Grace Murray Hopper"
  (lambda ()
    (name->string admiral-hopper)))

(description "name->string test cases")
(define sarah-kim
  (name #f "Sarah" #f "Kim" #f))

(test-case "sarah-kim"
  equal? "Sarah Kim"
  (lambda ()
    (name->string sarah-kim)))

(define havy
  (name #f "Vy" "Ha" "Nguyen" #f))

(test-case "havy"
  equal? "Vy Ha Nguyen"
  (lambda ()
    (name->string havy)))

; TODO: include at least 2 other test cases here.

;; -----------------------------------------------------------------------------
(problem "Problem 3: Husking")

;; In the reading, you learned about verifying preconditions of functions
;; explicitly so that we can produce good error messages when those
;; preconditions are not satisified. This leads to a particular programming
;; pattern where we decompose a function into a "husk" that verifies the
;; preconditions and a "kernel" that implements the actual function under
;; the assumption that the preconditions are satisfied.
;;
;; From the struct declaration above, we can see that it is likely that
;; none of our intended preconditions are being checked. Indeed, the following
;; call to the name function is valid:

(define bad-name
  (name "King" 0 "Bad" #t (list "Not good")))

;; And generates a name that breaks our invariants about what a name should
;; look like: each field should be a string or #f.
;;
;; To solve this, let's take the name function as our "kernel" and build a
;; "husk" function that validates the preconditions of the arguments before
;; calling the name function itself.
;;
;; Implement, document, and test this function, make-name, below:

; TODO: add documentation for this function here!
;;; (make->name val) -> error? or name?
;;; prefix -> string?
;;; given -> string?
;;; middle -> string?
;;; family-> string?
;;; suffix -> string?
;;; Returns error if val doesnt validate the preconditions of the argument, and returns name if
;;; it validates the preconditions.
(define make-name
  (let ([string-or-false?
         (lambda (val)
           (or (string? val) (equal? val #f)))])
    (lambda (prefix given middle family suffix)
      (cond
        [(not (string-or-false? prefix))
         (error "name: Invalid prefix ")]
        [(not (string? given))
         (error "name: Invalid given name ")]
        [(not (string-or-false? middle))
         (error "name: Invalid middle " )]
        [(not (string-or-false? family))
         (error "name: Invalid family: " )]
        [(not (string-or-false? suffix))
         (error "name: Invalid suffix: ")]
        [else
         (name prefix given middle family suffix)]))))

; TODO: add test-cases for this function here!
(test-case "havy"
  equal? (name #f "Vy" "Ha" "Nguyen" #f)
  (lambda ()
    (make-name #f "Vy" "Ha" "Nguyen" #f)))

(test-case "sarahkim"
  equal? (name #f "Sarah" #f "Kim" #f)
  (lambda ()
    (make-name #f "Sarah" #f "Kim" #f)))

(make-name #f "Sarah" #f "Kim" (list "hi"))

;; -----------------------------------------------------------------------------
(problem "Problem 4: Time")

;; Another example of an object amendable to representation as a struct is
;; a time. Create a structured type, time, that represents a time of the day
;; such as "7:35:45 PM" or "12:00:00 AM" where the three numbers represent the
;; hours, minutes, and seconds, respectively. With your partner, you should
;; choose what fields best capture a time!

(struct time (hours minutes seconds ampm))

;; -----------------------------------------------------------------------------
(problem "Problem 5: Temporal Husks")

;; Implement a husk for your time struct as a function, make-time. Ensure that
;; make-time validates its arguments appropriately.

;; TODO: fill in your make-time implementation here! Make sure to include
;; documentation and test cases as well!

;;; (make-time val) -> error? of time?
;;; hours: non-negative integers? 
;;; minutes: non-negative integer?
;;; seconds: non-negative integers?
;;; ampm: string? ("AM" or "PM")
;;; Returns error if it doesnt validate the precondition, returns time otherwise.
(define make-time
  (let ([right-time?
         (lambda (val)
           (and (string? val) (or (equal? val "AM") (equal? val "PM"))))]
        [valid-time? (lambda (val) (and (integer? val) (positive? val)))])
    (lambda (hours minutes seconds ampm)
      (cond
        [(not (valid-time? hours))
         (error "make-time: Invalid hours ")]
        [(not (valid-time? minutes))
         (error "make-time: Invalid minutes ")]
        [(not (valid-time? seconds))
         (error "make-time: Invalid seconds " )]
        [(not (right-time? ampm))
         (error "make-time: Invalid ampm " )]
        [else
         (time hours minutes seconds ampm)]))))

(test-case "7:45:43 AM"
  equal? (time 7 45 43 "AM")
  (lambda ()
    (make-time 7 45 43 "AM")))

(test-case "6:45:15 PM"
  equal? (time 6 45 15 "PM")
  (lambda ()
    (make-time 6 45 15 "PM")))

;; -----------------------------------------------------------------------------
(problem "Problem 6: Printing Time")

;; Write a procedure, `(time->string atime)`, that takes a time as
;; a parameter and returns the time as a string of the form
;; HH:MM:SS with time represented in 24-hour format.

;;; Helper: Add leading zero if number is single digit
;;; (helper n) -> string?
;;;   n: integer?, typically 0-59
;;; Returns a two-character string representation of n with leading zero if needed.
(define helper
  (lambda (n)
    (if (< n 10)
        (string-append "0" (number->string n))
        (number->string n))))

;;; (time->string atime) -> string?
;;;   atime: integer?, seconds since midnight (0-86399)
;;; Returns the time as a string in 24-hour format "HH:MM:SS".
(define time->string
  (lambda (atime)
    (let* ([hours (quotient atime 3600)]   
           [remaining (remainder atime 3600)]
           [minutes (quotient remaining 60)]  
           [seconds (remainder remaining 60)])
      (string-append (helper hours)
                     (helper minutes)
                     (helper seconds)))))


;; Also, write a procedure `(string->time str)`, that takes a string 
;; of the form `HH:MM:SS` as a parameter and returns an appropriate time
;; value.

;;; (string->time str) -> integer?
;;;   str: string?, format "HH:MM:SS"
;;; Returns the time as seconds since midnight.
(define string->time
  (lambda (str)
    (let* ([parts (string-split str ":")]
           [hours (string->number (car parts))]
           [minutes (string->number (cadr parts))]
           [seconds (string->number (caddr parts))])
      (+ (* hours 3600)      
         (* minutes 60)      
         seconds))))

;; Finally, time->string and string->time should be inverses of each other.
;; In other words, calling one function with a value and passing that result
;; to the other function returns the original value!
;;
;; Use this fact to write a collection of test-cases for both of these
;; functions.

(test-case "1 second"
  equal? 1
  (lambda () (string->time "00:00:01")))

(test-case "1 minute"
  equal? 60
  (lambda () (string->time "00:01:00")))
;; -----------------------------------------------------------------------------
(problem "Extra Problem: Comparing Time")

;; Once we define a data type, we often want to define additional functions
;; that operates over that data type. For example, it is useful to be able
;; to compare two times to see which comes first.
;;
;; Write a function (time<? t1 t2) that returns #t if t1 represents a time
;; that comes before t2 and #f otherwise.

;; TODO: insert your implementation of time<? below. Make sure to include
;; documentation and test cases!

;; -----------------------------------------------------------------------------
(problem "Extra Problem: Chirps")

;; *Chirp* is a new Internet startup that lets you send notes to your
;; friends, which they call "chirps".  (Creativity is not their strong
;; suit.)  Create a structured type, `chirp`, with the following
;; fields.

;; * `id`, a string we'll use to identify the chirp.
;; * `author`, a string that identifies the author of the chirp.
;; * `contents`, a string that contains the body of the chirp.
;; * `tags`, a list of strings
;; * `date`, a date that represents when the chirp was chirped.
;; * `time`, a string that represents the time the chirp was chirped.
;;
;; Note, we have a time struct! You may also want to create an analogous
;; date struct to capture the date of a chirp.

;; TODO: write your definition of chirp here!

;; Create an appropriate husk, `make-chirp`, for this struct that validates
;; its fields. Feel free to represent the various fields of a chirp using
;; whatever types make the most sense to you!

;; TODO: write your definition of make-chirp here!

;; Finally, try writing a function, 'chirp<?' that takes two chirps and
;; determines which of the two chirps was chirped first. (Hey, this almost
;; sounds like a viable business!)

;; TODO: write your definition of chirp<? here!