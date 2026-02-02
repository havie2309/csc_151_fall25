;; CSC-151-04 Fall 2025
;; Authors: Vy, Janett
;; Date: November 12th, 2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import lab)

(title "Random Language")

; -------------------------------------------------------------------
(part "Supplied Code")

;;; (roll-a-die) -> integer?
;;; Simulate the rolling of a die by returning an unpredictable
;;; integer between 1 and 6, inclusive.
(define roll-a-die
  (lambda ()
    (+ 1 (random 6))))

;;; (roll-dice n) -> listof integer?
;;;   n : non-negative-integer?
;;; Create a list consisting of the values from `n` dice rolls
(define roll-dice
  (lambda (n)
    (if (zero? n)
        null
        (cons (roll-a-die) (roll-dice (- n 1))))))

;;; (random-list-element lst) -> any?
;;;   lst : listof any?
;;; Randomly select an element of `lst`
(define random-list-element
  (lambda (lst)
    (list-ref lst (random (length lst)))))

;;; people -> listof string?
;;; A list of some of the folks who teach computer science
(define people (list "Charlie" "Eric" "Fernanda" "Jerod" "Nicole" "Peter-Michael" "Sam" "Sarah"
  "Shawn" "Will" "Vy" "Sarah" "Mem" "Janett"))


;;; (random-person) -> string?
;;; Randomly select an element of the people list.
(define random-person
  (lambda ()
    (random-list-element people)))

;;; transitive-verbs -> listof string?
;;; A short list of transitive verbs (those that take a direct object)
(define transitive-verbs (list "saw" "watched" "threw" "ate" "borrowed" "run" "buy"
  "decorate" "borrow"))

;;; (random-transitive-verb) -> string?
;;; Randomly select among the transitive verbs
(define random-transitive-verb
  (lambda ()
    (random-list-element transitive-verbs)))

;;; articles -> listof string?
;;; A short list of articles.
(define articles (list "the" "a"))

;;; adjectives -> listof string?
;;; A short list of adjectives.
(define adjectives (list "heavy" "blue" "green" "hot" "cold" "disgusting" "beautiful" 
  "pretty" "old" "new" "small" "big"))

;;; nouns -> Listof string?
;;; A short list of nouns (or noun phrases).
(define nouns (list "cup of coffee" "computer" "classroom" "Chicken Wrap" "ice-cream"
                    "PBJ algorithm" "homework assignment" "laptop" "mirror"))

;;; (random-object) -> string?
;;; Generate a random noun-phrase that can serve as the object of a
;;; transitive verb.
(define random-object
  (lambda ()
    (string-append
     (random-list-element articles) " "
     (random-list-element adjectives) " "
     (random-list-element nouns))))

;;; (sentence) -> string?
;;; Generate a random sentence.
(define sentence
  (lambda ()
    (string-append
     (random-person) " "
     (random-transitive-verb) " "
     (random-object) ".")))



;;; (average nums) -> real?
;;;   nums : listof? real
;;; Find the average of a nonempty list of real numbers.
(define average
  (lambda (nums)
    (/ (reduce + nums)
       (length nums))))

; -------------------------------------------------------------------
(part "Problem 0: Preparation")

; a. Introduce yourself to your partner, discuss work habits, decide what
; you will be doing at the end of class.
;
; b. Quickly skim through the *documentation* for the procedures above to
; make sure you understand their goals.  Ask the members of the lab staff
; if any seem confusing to you.
;
; Do not submit anything for this exercise; it's just a reminder to take
; these first steps.

; -------------------------------------------------------------------
(part "Problem 1: Averaging dice rolls")

; One way to explore the quality of our `roll-a-die` procedure is to
; average a bunch of dice rolls. Arguably, if the dice are "fair",
; we should find that the average is about 3.5.

(problem "Problem 1a")

; Try a few experiments to see if we usually get an average of
; about 3.5 (or 3 1/2, since we're working with exact numbers) if we
; roll five dice. Use the `average` function given above to perform
; this computation. Describe your findings below:

; TODO: experiment here...
(roll-a-die)
(average (roll-dice 5))
; 3.4

(description "The results of testing 5 roll-a-die calls are 1 2 5 4 5. The average of testing 
  roll-a-die is 3.4")

(problem "Problem 1b")

; Can our experiment ever give a result of exactly 3 1/2? Why or
; why not?
(average (roll-dice 10))

(description "Our experiment can never give a result of exactly 3.5
  because we cannot get the exact;y 3.5 when take average of 5 numbers ")

(problem "Problem 1c")

; Try a few experiments in which you roll ten dice. We assume you can figure
; out how to do so by modifying the experiment presented above.

; TODO: experiment here...

(description "The results of testing 10 roll-a-die calls are it gives an average range of 2-4.6")

(problem "Problem 1d")
  
; Can this revised experiment ever give a result of exactly 3 1/2? Why or why not?


(description "Our revised experiment can give a result of exactly 3.5 because it'll eventually produce 3.5
  after a series of test due to the roll-dice being 10. It has larger amount of numbers")

; -------------------------------------------------------------------
(part "Problem 2: Rolling multiple dice")

; Some students, when confronted with the prior problem, write the
; following to find the average of ten dice rolls.  Don't uncomment
; and evaluate this expression yet!

(average (make-list 10 (roll-a-die)))

(problem "Problem 2a")
  
; What do you see as the advantages and disadvantages of that approach?

(description "The advantages and disadvantages of this approach are that the make-a-list produces a 
  list of 10 of the value of whatever roll-a-die is so the average would always an integer number.")

(problem "Problem 2b")
  
; Try that experiment a few times. Do you get results that you
; expected? If you did not expect the results you got, try to explain
; them. If you did get the results you expected, explain those.

(description "Our results are integer numbers and our explanation is make-a-list produces a 
  list of 10 of the value of whatever roll-a-die is so the average would always an integer number")

(problem "Problem 2c")

; Try the following expression a following expression a few times
; and see if it heps explain what was happening. Explain what is
; happening here, too, and why it is (likely) undesirable.

(make-list 10 (roll-a-die))

(description "This expression is a function that returns a list of the same number because it rolls the 
  die once and produces 10 copies of the result.")

; -------------------------------------------------------------------
(part "Problem 3: Rolling multiple dice, revisited")

; As you've likely determined, the `roll-dice` procedure from the reading
; is a better way to reliably roll an arbitrary number of dice.

(problem "Problem 3a")
  
; Conduct a few experiments with a large number of dice (say 1000 or
; more) to see whether we seem to get an appropriate average.  

; TODO: Experiment here

(average (roll-dice 1000))

(description "Our results have more decimal points and closer to 3.5")

(problem "Problem 3b")
  
; `roll-dice` creates a list.  As you've seen before, rather than
; creating a list, we can just add as we go.  Write a recursive
; procedure, `(sum-dice n)`, that gives the sum of the rolls of `n`
; dice.  You may assume that `n` is a positive integer.  

; Your procedure should not create a list.  Among other things, that
; means you cannot call `roll-dice` in writing `sum-dice`.  Use 
; recursion instead!

(define sum-dice
  (lambda (n)
    (match n
    [0 0]
    [_ (+ (roll-a-die) (sum-dice (- n 1)))])))


(problem "Problem 3c")

; Using `sum-dice` as a helper, write a procedure, `(average-roll n)`, 
; that computes the average value of `n` dice rolls.  You may assume
; that `n` is a positive integer.

(define average-roll
  (lambda (n)
    (/ (sum-dice n) n)))

(average-roll 5)

; -------------------------------------------------------------------
(part "Problem 4: Tallying sevens and elevens")

; There are some games of chance in which the player rolls two dice
; and if those two dice total seven or eleven, the player wins. It
; might be interesting to determine how often we are likely to roll
; a seven or eleven in n rolls of two dice.

; Consider the following pair of procedures intended to help us achieve
; that goal.

;;; (pair-a-dice) -> integer?
;;; Roll two dice and return their result, which should be an
;;; integer between 2 and 12 (but not with an even distribution).
(define pair-a-dice
  (lambda ()
    (+ (roll-a-die) (roll-a-die))))

;;; (play-seven-eleven) -> integer?
;;; Play one round of the seven-eleven game.  Return 1 for a
;;; victory and 0 for a loss.
; (define play-seven-eleven
;   (lambda ()
;     (cond
;       [(= 7 (pair-a-dice))
;        1]
;       [(= 11 (pair-a-dice))
;        1]
;       [else
;        0])))


(define play-seven-eleven
  (lambda ()
    (let ([roll (pair-a-dice)])
      (cond
        [(or (= roll 7) (= roll 11)) 1]
        [else 0]))))

(problem "Problem 4a")

; Verify that both `pair-a-dice` and `play-seven-eleven` appear to
; work as advertised. (Note that, in our own experiments, it can take
; as many as ten rolls before you win; theoretically, it could take
; many more.)

; TODO: experiment here!

(play-seven-eleven)

(description "We observed that it harder to get 1 because if i want to get 1, you have to get 
  exactly 7 or 11, which is more unlikely.")

(problem "Problem 4b")

; Make a list of all the rolls of a red die and a black die that
; will give you seven or eleven.  For example, red 1 and black 6 give
; you 7, as do red 6 and black 1; red 6 and and black 5 give you
; eleven.  Using that list, compute the probability that you roll
; seven or eleven.  (There are thirty-six different possible rolls
; on two dice.) How many games would you expect to win if you played
; 36,000 games?

; TODO: write your notes here in a comment:
; (red 1 black 6) (red 6 black 1) (red 3 black 4) (black 4 red 3) (red 2 black 5) (red 5 black 2)
; (red 6 black 5) (red 5 black 6)

(description "We expect to win around 8000 games if we played 36,000 games")

(problem "Problem 4c")

; We might also want to compute the experimental probability of
; winning.  Write a recursive procedure, `(count-wins n)`, that 
; plays the seven-eleven game `n` times and reports how many wins
; there are.

;;; (count-wins n) -> integer?
;;;   n : non-negative-integer?
;;; Play `n` games of seven-eleven and determine how many times
;;; you win.
(define count-wins
  (lambda (n)
    (match n
    [0 0]
    [_ (if (= (play-seven-eleven) 1)
        (+ 1 (count-wins (- n 1)))
        (count-wins (- n 1)))])))

(problem "Problem 4d")

; Using `count-wins`, determine how many wins you get in 36,000 games.
; You may want to conduct a few experiments.   What, if anything, do you 
; notice?

; TODO: experiment here...

(count-wins 36000)

(description "Our observations are the number of times we win is around 7,500-7,700")

; -------------------------------------------------------------------
(part "Problem 5: Exploring subtle errors")

; You may have noted that we got fewer wins than expected.  (The house
; cheats, it appears.)  That suggests that there is a subtle error in
; `play-seven-eleven`.  You may have even discovered it.  It's okay
; if you haven't.

; Whether or not you've discovered the error, we're going to consider 
; how you might find it.

(problem "Problem 5a")

; How many dice rolls should happen if we call `(count-wins 10)`?
; (The question is not how many do happen; the question is how many
; times should we roll the dice if we play ten games of seven-eleven?

(description "10 dice rolls should happen with (count-wins 10)")

(problem "Problem 5b")

; Use the stepper to determine how many times we roll the dice in `(count-wins 10)`.
; Is it the same as the number you indicated in part a?  Is it always
; the same?

(description "We observed that the dice is rolled 20 times because the 
  of the function play-seven-eleven that takes in 2 sets of dice and the count
  win uses that function and calls 2 sets of dice 10 times so 20 times the 
  dice is rolled")


(problem "Problem 5c")

; As your experiment likely showed, we seem to be rolling the dice
; more times than we expected.  Determine why.

(description "The reason for this is because the count-wins uses the play-seven
  -eleven  that takes in 2 pairs of dice so the count-wins calls the 2 pairs of 
  dice 10 times")

(problem "Problem 5d")

; Fix `play-seven-eleven` by modifying the original implementation,
; leaving the buggy part of the code in but commented out. Verify
; that your fix resolves the problem.
(count-wins 10)

; -------------------------------------------------------------------
(part "Problem 6: Choosing names")

; Suppose we have a list of names, `students`, that represents all
; of the students in a class. 

;;; students : listof string?
;;; A list of students in a fictitious class
(define students
  (list "Andi" "Brook" "Casey" "Devin" "Drew" "Dylan" "Emerson" "Frances"
        "Gray" "Harper" "Jamie" "Kennedy" "Morgan" "Phoenix" "Quinn" "Ryan"))

(problem "Problem 6a")

; Write a zero-parameter procedure, `(random-student)`, that randomly
; selects the name of a student from the class.  You may certainly rely
; on other procedures we've written before.

(define random-elt
  (lambda (lst)
    (list-ref lst (random (length lst)))))
;;; (random-student) -> str?
;;; randomly selects a student name from the list of students
(define random-student
  (lambda ()
    (random-elt students)))
(random-student)

(problem "Problem 6b")
  
; Write a zero-parameter procedure, (random-pair), that calls
; (random-student) twice and puts the two students together into a
; list.

;;; (random-pair) -> ???
;;; ???
(define random-pair
  (lambda ()
    (list (random-student)(random-student))))
(random-pair)

(problem "Problem 6c")
  
; What are some potential problems with using (random-pair) to select
; partners for the class?

(description "Some potential problems are that there is a chance where a
  student is paired with themselves")

; -------------------------------------------------------------------
(part "Problem 7: Generating sentences")

(problem "Problem 7a")

; Using the `sentence` procedure from the reading, generate about
; five different sentences. Give them below:

; TODO: write code here to generate the sentences
(sentence)
(description "Sentence 1: Sam saw a green homework assignment.")
(description "Sentence 2: Charlie borrowed a hot homework assignment.")
(description "Sentence 3: Will threw a disgusting cup of coffee.")
(description "Sentence 4: Jerod ate the heavy PBJ algorithm.")
(description "Sentence 5: Shawn saw a blue classroom.")

(problem "Problem 7b")

; Add a few names, verbs, adjectives, and nouns to expand the range
; of sentences, then generate five new sentences.

; TODO: write code here to generate the sentences
(sentence)
(description "Sentence 1: Jerod buy the old cup of coffee.")
(description "Sentence 2: Sam borrowed the green computer.")
(description "Sentence 3: Fernanda borrowed a cold mirror.")
(description "Sentence 4: Janett run the hot Chicken Wrap.")
(description "Sentence 5: Mem ate the heavy ice-cream.")

; -------------------------------------------------------------------
(part "Problem 8: Madder libs")

; There's a famous party game called "Mad Libs" in which one player
; starts with a template for a story and asks other players to supply
; different types of words without knowing the context.

; Here's a sample Scheme procedure that does a simple form of Mad Libs.

(define mad-labs
  (lambda (adjective1 adjective2 verb noun1 noun2 noun3 name)
    (string-append "CSC 151 labs made me "
                   adjective2
                   ".  Each day we "
                   verb
                   " "
                   noun3
                   "s.  I had the opportunity to work with many "
                   adjective1
                   " "
                   noun1
                   "s.  I would definitely recommend this class to "
                   name
                   "'s "
                   noun2
                   ".")))

(problem "Problem 8a")

; Write a call to `mad-labs` with the parameters "sleepy", "tearful", 
; "defenestrate", "dragon", "dog", "cat", and "President Harris" below.

; TODO: add a call to mad-labs below
(mad-labs "sleepy" "tearful" "defenstrate" "dragon" "dog" "cat" "President Harris")

(problem "Problem 8b")

; Write a zero-parameter procedure, `(random-mad-labs)`, that uses
; mad-labs to generate a story with parameters randomly selected from
; the appropriate grammatical category. You should not change the
; mad-labs procedure, nor should you copy its body. Rather,
; random-mad-labs should generate an appropriate call to mad-labs.

(define random-mad-labs
  (lambda ()
        (string-append "CSC 151 labs made me "
                   (random-list-element adjectives)
                   ".  Each day we "
                   (random-transitive-verb)
                   " "
                   (random-list-element nouns)
                   "s.  I had the opportunity to work with many "
                   (random-list-element adjectives)
                   " "
                   (random-list-element nouns)
                   "s.  I would definitely recommend this class to "
                   (random-person)
                   "'s "
                   (random-list-element nouns)
                   ".")))
(random-mad-labs)