;; CSC 151 04 (Fall)
;; Lab: Vectors (vectors.scm)
;; Authors: Ellis, Vy
;; Date: 11/7/25
;; Acknowledgements:
;;   N/A

(import lab)
(import test)

;; In today's lab, we'll explore how we can create and manipulate vectors in
;; Scheme. For each problem, alternative driver/navigator with your partner.

;; -----------------------------------------------------------------------------
(part "Problem 1: Binding and Sequencing")

;; Whenever possible, we will want to avoid using top-level definitions in
;; favor of local bindings and parameter passing. However, it is relatively
;; easy syntactically to define a top-level vector and mutate it at the top
;; level. But doing so locally requires a particular combination of binding
;; and sequencing with the begin expression that we should get used to.

;; First, let's review how the begin expression works. Consider the following
;; code snippet that attempts to mutate the first, third, and fifth slots of
;; the vector using function chaining:

 (define example-vector (vector 0 1 2 3 4))

(vector-set!
  (vector-set!
    (vector-set! example-vector 0 "zero")
    2 "two")
  4 "four")

;; Uncomment the code (highlight the block, then press ctrl-/) and run it.
;; You should receive an error! In a sentence or two, describe the error and
;; why it occurs:

(description "The error is ... It occurs because ... the return of any vector-set is a void, and 
  subsequent calls to vector-set must take in a vector")

;; (begin ...) allows us to execute a sequence of side effects. Now, let's
;; combine this concept with a local binding to:
;;
;; (a) Create a new vector (as a let-bound variable)
;; (b) Mutate the elements of the vector (using begin and vector-set!)
;; (c) Returns that vector after mutating it.
;;
;; Complete the definition of the function below that:
;;
;; a.  Creates a new vector of five elements, initially with values 0–4
;; b.  Mutates the 1st, 3rd, and 5th elements to be "zero", "two", and
;;     "four."
;; c.  Finally, the function returns that vector as output. Note that a
;;     begin expression produces the value of its final expression as
;;     output.

;;; (make-and-mutate-vector)
;;; Returns a vector of ("zero" 1 "two" 3 "four")

(define make-and-mutate-vector
  (lambda ()
    (let*
     ([vec (vector 0 1 2 3 4)])
     (begin 
      (vector-set! vec 0 "zero")
      (vector-set! vec 2 "two")
      (vector-set! vec 4 "four")
      vec)
  )))

(test-case "make-and-mutate-vector"
  equal? (vector "zero" 1 "two" 3 "four")
  make-and-mutate-vector)

;; -----------------------------------------------------------------------------
(part "Problem 2: Swapping")

;; Complete the following function definition using vector-set! that
;; swaps the elements found at two locations in a vector. For this function,
;; make sure to check that i and j are valid indices into the vector. If
;; not, your function should raise an error with (error msg). Additionally,
;; write two at least additional test cases for the function below. Observe
;; with the example test how we use the let/begin pattern from the previous
;; problem to make a vector, mutate it with swap!, and return it.

;; (swap! v i j) -> void?
;;   v: vector?
;;   i: integer?, a valid index into v
;;   j: integer?, a valid index into v
;; Swaps the elements at indices i and j of v.
(define swap!
  (lambda (vec i j)
    (if (and (> (vector-length vec) i) (> (vector-length vec) j))
      (let
       ([x (vector-ref vec i)])
      (begin
       (vector-set! vec i (vector-ref vec j))
       (vector-set! vec j x)
       void))
    (error "msg")
     )))

(test-case "swap-test-1"
  equal? (vector 0 3 2 1 4)
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (swap! v 1 3)
        v))))

(test-case "swap-test-2"
  equal? (vector 4 1 2 3 0)
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (swap! v 0 4)
        v))))

(test-case "swap-test-3"
  equal? (vector 2 1 0 3 4)
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (swap! v 0 2)
        v))))


; TODO: add at least two more tests here!

;; -----------------------------------------------------------------------------
(part "Problem 3: Incrementing in two ways")

;; A common "stateful" operation we might consider is incrementing the
;; value at a particular location in a vector. We may also want the value at
;; at that location either _before_ the increment occurs or _after_ the
;; increment occurs.
;;
;; Implement the following pair of functions:
;;
;; + (pre-inc vec i) takes a vector vec of numbers and a valid index i into
;;   vec as input. The function increments the ith element of vec as a side-
;;   effect and returns the value of the ith element _before_ the increment.
;; + (post-inc vec i) takes a vector vec of numbers and a valid index i into
;;   vec as input. The function increments the ith element of vec as a side-
;;   effect and returns the value of the ith element _after_ the increment.

(define pre-inc
  (lambda (vec i)
    (let ([x (vector-ref vec i)])
          (begin
           (vector-set! vec i (+ (vector-ref vec i) 1))
    x
    ))
   ))

(define post-inc
  (lambda (vec i)
    (begin 
      (vector-set! vec i (+ (vector-ref vec i) 1))
    (vector-ref vec i))))

(test-case "pre-ince-test-1"
  equal? (vector 1 1 2 3 4)
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (pre-inc v 0)
        v))))

(test-case "post-ince-test-1"
  equal? (vector 1 1 2 3 4)
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (post-inc v 0)
        v))))

(test-case "post-ince-test-2"
  equal? (vector 0 2 2 3 4)
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (post-inc v 1)
        v))))

;; Write at least three test cases for your functions below. Keep in mind
;; that pre-inc and and post-inc mutate their input vectors, so the effects
;; of previous changes to a test vector will be reflected in your tests if
;; you aren't careful! We recommend using the let/begin pattern from the
;; first problem to write test-cases that are self-contained.

;; TODO: write tests here!

;; -----------------------------------------------------------------------------
(part "Problem 4: Recursion with Vectors")

;; A notable difference between lists and vectors is that vectors cannot be
;; easily broken up into a head and tail. Our only recourse for creating the
;; tail of a vector is to create a new vector without the head! If the
;; vector is large, then creating these vectors can become very expensive.
;; In contrast, lists are implemented behind the scenes so that we do not
;; need to create a copy of the list to access the tail---it is just available
;; to us.
;;
;; Nevertheless, we can still use recursion to traverse vectors. However,
;; instead of using structural recursion on the vector (because it is not
;; recursively defined), we'll instead use numeric recursion on the indices
;; of the vector. These indices range from 0 to (- 1 (vector-length v)) for
;; a vector v.
;;
;; This "current index" will then become a parameter to our function in
;; question. However, we frequently will want to always start this index at
;; 0 or (- 1 (vector-length vec)) to scan the entire vector. Thus, our
;; recursive functions over vectors will be broken up into two functions:
;;
;; - A "helper" function that takes the current index and vector as input and
;;   actually does the recursion.
;; - A "top-level" function that takes just the vector as input and simply
;;   calls the helper with an appropriate initial index.
;;
;; Let's apply this concept to write a recursive function
;; (vector-contains vec v) that returns #t if and only if v is contained
;; somewhere inside of vec. First, implement the helper function which
;; performs numeric recursion on the index of the vector:

(define vector-contains-helper
  (lambda (vec v i)
    (if (= i (vector-length vec))
            #f
           (if (equal? (vector-ref vec i) v)
               #t
        
            (vector-contains-helper vec v (+ i 1)))
      )))

;; Now, implement the top-level function that calls the helper you wrote
;; with an appropriate initial value for the index:

(define vector-contains
  (lambda (vec v)
    (vector-contains-helper vec v 0)))

;; Finally, write test cases for vector-contains below, using the local
;; vector test-case pattern with let/begin described previously.

;; TODO: write your test cases here

(test-case "vector-contains-test-1"
  equal? #t
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (vector-contains v 1)
        ))))

(test-case "vector-contains-test-2"
  equal? #t
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (vector-contains v 4)
        ))))

(test-case "vector-contains-test-3"
  equal? #f
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (vector-contains v 5)
        ))))


;; -----------------------------------------------------------------------------
(part "Problem 5: More Vector Recursion")

;; Use numeric recursion over vectors to implement the following functions.
;; Make sure to write helper functions that perform the recursion; the
;; signatures of the top-level functions are provided below. Make sure to
;; write test cases to check your work below each implementation.

; (vector-all pred vec) -> bool?
;   pred: a predicate (one-argument function) over the elements of vec
;   vec: vector?
; Returns #t if every element of vec satisifies pred.

(define vector-all-helper
  (lambda (pred vec i)
    (if (= i (vector-length vec))
        #t
        (if (pred (vector-ref vec i))
        (vector-all-helper pred vec (+ i 1))
          #f)
        
      ))
    )

(define vector-all
  (lambda (pred vec)
    (vector-all-helper pred vec 0)))

(test-case "vector-all-test-1"
  equal? #f
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (vector-all even?  v )
        ))))

(test-case "vector-all-test-2"
  equal? #f
  (lambda ()
    (let ([v (vector 0 1 2 3 4)])
      (begin
        (vector-all odd?  v )
        ))))

(test-case "vector-all-test-3"
  equal? #t
  (lambda ()
    (let ([v (vector 1 2 3 4)])
      (begin
        (vector-all positive?  v )
        ))))

; (vector-sum-every-n vec n) -> number?
;   vec: vector? of numbers
;   n: integer?, non-negative
; Returns the sum of every nth element of vec, starting with the first.
; For example, if n = 3, then we sum up the elements at index 0, 3, 6, 9, etc.
(define vector-sum-every-n-helper
  (lambda (vec n i)
    (let ([sum 0])
    (if (>= i (vector-length vec))
      sum
      (+ (vector-ref vec i) (vector-sum-every-n-helper vec n (+ i n)) sum)))))

(define vector-sum-every-n
  (lambda (vec n)
    (vector-sum-every-n-helper vec n 0)))

(test-case "vector-sum-every-n-test-1"
  equal? 4
  (lambda ()
    (let ([v (vector 1 2 3 4)])
      (begin
        (vector-sum-every-n v 2)
        ))))

(test-case "vector-sum-every-n-test-2"
  equal? 12
  (lambda ()
    (let ([v (vector 1 2 3 4 5 6 7)])
      (begin
        (vector-sum-every-n v 3)
        ))))

(test-case "vector-sum-every-n-test-3"
  equal? 6
  (lambda ()
    (let ([v (vector 1 2 3 4 5 6 7)])
      (begin
        (vector-sum-every-n v 4)
        ))))

;; -----------------------------------------------------------------------------
(part "Problem 6: Mutable Map")

;; The standard library provides two mapping functions over vector. vector-map
;; behaves identically to map but over a vector. This function operates by
;; creating a new vector that contains the mapped elements. In contrast,
;; vector-map! _mutates_ the input vector to contain the mapped elements.
;;
;; Let's replicate the behavior of vector-map! by implementing
;; (my-vector-map! f vec) with numeric  recursion that performs a mapping
;; operation over vec with f but instead of creating a new vector, it instead
;; mutates each element of vec to be the  result of applying f to that element.
;; The function does not return anything as a result!
(define my-vector-map!-helper
  (lambda (f vec i)
      (if (= i (vector-length vec))
        vec
        (begin 
         (vector-set! vec i (f (vector-ref vec i)))
         (my-vector-map!-helper f vec (+ i 1))))
      ))

(define my-vector-map!
  (lambda (f vec)
    (my-vector-map!-helper f vec 0)))


(display (my-vector-map! (section + _ 1) (vector 1 2 3 4)))








