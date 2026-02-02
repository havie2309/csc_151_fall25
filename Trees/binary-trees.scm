;; CSC-151-04 Fall 2025
;; Lab: Binary Trees (binary-trees.scm)
;; Authors: Ella, Vy
;; Date: Nov 21, 2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import lab)
(import test)

(title "Binary Trees")

; -------------------------------------------------------------------
(part "Provided code: The definition of a tree")

;; A tree is either:
;;   + Empty (a leaf) or
;;   + Non-empty (a node) with a value, left subtree, and right subtree.
(struct leaf ())
(struct node (value left right))

;;; (tree? v) -> boolean?
;;;   v: any
;;; Returns #t iff v is a tree.
(define tree?
  (lambda (v) (or (leaf? v) (node? v))))

;;; (tree-level->string level tree) -> string?
;;;   level: nat?
;;;   tree: tree?
;;; Returns the string representation of tree starting at the given level.
(define tree-level->string
  (let ([bullets (vector "* " "+ " "- " ". ")]
        [make-spaces (lambda (n)
                       (list->string (make-list n #\space)))])
    (lambda (level tree)
      (let* ([spaces (make-spaces (* 2 level))]
             [bullet
               (string-append
                 spaces
                 (vector-ref bullets (remainder level (vector-length bullets))))])
        (match tree
          [(leaf) ""]
          [(node value (leaf) (leaf)) (string-append bullet value)]
          [(node value left (leaf))
           (string-append
             (string-append bullet value)
             "\n"
             (tree-level->string (+ level 1) left))]
          [(node value (leaf) right)
           (string-append
             (string-append bullet value)
             "\n"
             (tree-level->string (+ level 1) right))]
          [(node value left right)
          (string-append
            (string-append bullet value)
            "\n"
            (tree-level->string (+ 1 level) left)
            "\n"
            (tree-level->string (+ 1 level) right))])))))

;;; (tree->string tree) -> string?
;;;   tree: tree?
;;; Returns the string representation of tree.
(define tree->string
  (lambda (tree)
    (tree-level->string 0 tree)))

;;; The legendary management tree
(define management-tree
  (node
    "Board"
    (leaf)
    (node
      "CEO"
      (node
        "Head of Engineering"
        (node "Software Developer" (leaf) (leaf))
        (node "Tester" (leaf) (leaf)))
      (node
        "Head of Legal"
        (leaf)
        (node "Lawyer" (leaf) (leaf))))))

(tree->string management-tree)

; -------------------------------------------------------------------
(part "Part 1: Making trees")

(problem "1a")

; Consider the following trees of numbers drawn with ASCII art:

; i. tree-i
;           "b"
;           / \
;          /   \
;        "a"   "c"

; ii. tree-ii
;            "e"
;            / \
;           /   \
;          "b"  "f"
;          / \    \
;         /   \    \
;       "a"   "c"  "g"
;             /
;            "d"

; iii. tree-iii
;           "f"
;           /
;         "e"
;         /
;       "d"
;       /
;     "c"
;     /
;   "b"
;   /
; "a"

; For each of these trees, use the tree-making functions from the
; reading to complete the definitions of `tree-i`, `tree-ii`, and
; `tree-iii` below. 

; Make sure to check that each thing you enter is a binary tree with
; `tree?` and that it has the right form with `tree->string`

;     > (tree? tree-i)
;     #t
;     > (tree->string tree-i)
;     * b
;       + a
;       + c
;     > (tree? tree-ii)
;     #t
;     > (tree->string tree-ii)
;     * e
;       + b
;         - a
;         - c
;            . d
;       + f
;         - g
;     > (tree? tree-iii)
;     #t
;     > (tree->string tree-iii)
;     * f
;       + e
;         - d
;           . c
;             . b
;               . a

(description "tree-i")

(define tree-i
  (node "b" (node "a" (leaf) (leaf)) (node "c" (leaf) (leaf))))

(display (tree->string tree-i))

(description "tree-ii")

(define tree-ii
  (node "e" (node "b" (node "a" (leaf) (leaf)) (node "c" (node "d" (leaf) (leaf)) (leaf))) 
    (node "f" (node "g" (leaf) (leaf)) (leaf))))

(display (tree->string tree-ii))

(description "tree-iii")

(define tree-iii
  (node "f" (node "e" (node "d" (node "c" (node "b" 
    (node "a" (leaf) (leaf)) (leaf)) (leaf)) (leaf)) (leaf)) (leaf)))

(display (tree->string tree-iii))

(problem "1b")

; Note that tree-iii is a left-leaning tree. That is, all its children are
; left children. Complete the definition of tree-iv below which is
; the same as tree-iii except that its leaves grow to the right rather
; than the left.

; "a"
;   \
;   "b"
;     \
;     "c"
;       \ 
;       "d"
;         \ 
;         "e"
;           \
;           "f"

; > (tree? tree-iv)
; #t
; > (tree->string tree-iv)
; * a
;   + b
;     - c
;       . d
;         * e
;           + f

(description "tree-iv")

(define tree-iv 
  (node "a" (leaf) (node "b" (leaf) (node "c" (leaf) (node "d" (leaf) 
    (node "e" (leaf) (node "f" (leaf) (leaf))))))))

(display (tree->string tree-iv))

(problem "1c")

; Finally, in the space below describe in a few sentences the
; differences and similarities between tree-iii and tree-iv. Do you
; consider these trees to be the same tree or different trees?
; Why?

(description "<The similarities between tree-iii and tree-iv is they both have one-side empty and the
  other side is the smaller trees. The differences between them is in tree-iii, the empty side is always 
  on the right, and in tree-iv it is on the left. >")

; -------------------------------------------------------------------
(part "Part 2: Leaves")

(problem "2a")

; As you may have noted, in the sample code, we use the very verbose
;
; (node val (leaf) (leaf))
;
; To create a node with no children. This is a bit tedious! Write a
; helper function 'singleton' that takes a value as input and produces
; a node with no children as output, i.e., a tree of one element.
; Document and test the function appropriately.

;;; (singleton val) -> tree?
;;; val : any?
;;; takes a value 'val' and returns a node with no children or 
;;; a tree with one element. 
(define singleton
  (lambda (val)
    (node val (leaf) (leaf))))

(description "singleton tests")

(test-case "normal case - str"
  equal? (node "a" (leaf) (leaf))
   (lambda () (singleton "a")))

(test-case "normal case - number"
  equal? (node 6 (leaf) (leaf))
   (lambda () (singleton 6)))

; Now, write a function 'singleton?' that takes a tree as input and
; returns #t if and only if the tree has no children. Use the query
; functions produced by our struct declarations leaf? and node? in
; conjunction with operations over booleans for this task.  Document
; and test this function appropriately, too.

; TODO: write your documentation here
(define singleton?
 (lambda (tree)
   (and (not (node? (node-left tree)))(not (node? (node-right tree))))))

(description "singleton? tests")

(test-case "normal case - num"
  equal? #t
  (lambda () (singleton? (singleton 6))))

(test-case "normal case - str"
  equal? #t
  (lambda () (singleton? (singleton "a"))))

(test-case "normal case - false"
  equal? #f
  (lambda () (singleton? (node 5 (node 6 (leaf) (leaf)) (leaf)))))

; -------------------------------------------------------------------
(part "Part 3: Traversing trees")

; Recall from our discussion of structs that we can get out the
; fields of a struct using struct projection functions.
;
; For our tree structs, we have:
;
; + The (node-value n), (node-left n), and (node-right n) functions
;   retrieve the value, left child, and right child of a node.
;   We also have the pattern (node value left right) to pattern

;
; In the space below, write expressions to retrieve the given
; value from the trees you created above:


(problem "3a: \"b\" from tree-i")

(define tree-i-projection
  (node-value tree-i))

(test-case "tree-i-projection"
  equal? "b"
  (lambda () tree-i-projection))

(problem "3b: \"c\" from tree-ii")

(define tree-ii-projection
  (node-value (node-right (node-left tree-ii))))

(test-case "tree-ii-projection"
  equal? "c"
  (lambda () tree-ii-projection))

(problem "3c: \"b\" from tree-iii")

(define tree-iii-projection
  (node-value (node-left (node-left (node-left (node-left tree-iii))))))

(test-case "tree-iii-projection"
  equal? "b"
  (lambda () tree-iii-projection))

(problem "3d: \"Head of Legal\" from management-tree")

(define management-tree-i-projection
  (node-value (node-right (node-right management-tree))))

(test-case "management-tree-i-projection"
  equal? "Head of Legal"
  (lambda () management-tree-i-projection))

(problem "3e: \"Software Developer\" from management-tree")

(define management-tree-ii-projection
  (node-value (node-left (node-left (node-right management-tree)))))

(test-case "management-tree-ii-projection"
  equal? "Software Developer"
  (lambda () management-tree-ii-projection))


; -------------------------------------------------------------------
(part "Part 4: Exploring tree recursion")

; From the reading, we noted that a binary tree is recursively defined like a
; list. A binary tree is either:

; + *Empty*, or
; + *Non-empty* where the tree contains a value and up to two *children*
;   (*subtrees*) that are, themselves, trees.

; Like lists, our tree operations mirror this recursive decomposition of
; the list. As a first example, consider the following function which
; computes the *size* of the input tree, *i.e.*, the number of values it
; contains.

;;; (tree-size tree) -> integer?
;;;   tree : tree?
;;; Determine how many values are in binary tree.
(define tree-size
  (lambda (t)
    (match t
      [(leaf) 0]
      [(node _ l r) (+ 1 (tree-size l) (tree-size r))])))

; a. For reference, copy and paste your definitions from tree-i and
; tree-ii from a previous problem within this comment below:

; (define tree-i
;    (node "b" (node "a" (leaf) (leaf)) (node "c" (leaf) (leaf))))
; )

; (define tree-ii
;    (node "e" (node "b" (node "a" (leaf) (leaf)) (node "c" (node "d" (leaf) (leaf)) (leaf))) 
;    (node "f" (node "g" (leaf) (leaf)) (leaf))))
; )

; Now, use your mental model of computation to give an evaluation trace
; of the following expression in the space below. In your derivation,
; you may take the following short-cuts:
;
; + You may evaluate a recursive call to tree-size directly to the
;   branch of the pattern match that is selected.
; + You may elide the contents of the tree's children during evaluation.
;
; If you feel unsure, use the tracing tool to check your results. 

; a. (tree-size tree-i)
; --> (match (node "b" (node "a" (leaf) (leaf)) (node "c" (leaf) (leaf)))
;       [(leaf) 0]
;       [(node _ l r) (+ 1 (tree-size l) (tree-size r))])

; --> (match (node "b" (node "a" (leaf) (leaf)) (node "c" (leaf) (leaf)))
;       [(node _ l r) (+ 1 (tree-size l) (tree-size r))])

; --> (match (node "b" (node "a" (leaf) (leaf)) (node "c" (leaf) (leaf)))
;       [(node _ l r) (+ 1 (tree-size (node "a" (leaf) (leaf))) (tree-size (node "c" (leaf) (leaf)))])

; --> (match (node "b" (node "a" (leaf) (leaf)) (node "c" (leaf) (leaf)))
;       [(node _ l r) (+ 1 (tree-size (node "a" 0 0) (tree-size (node "c" 0 0)])

; --> (match (node "b" (node "a" (leaf) (leaf)) (node "c" (leaf) (leaf)))
;       [(node _ l r) (+ 1 1 1)])

; ---> 3

(description "Size of tree-i: 3")

(tree-size tree-i)

; b. (tree-size tree-ii)
; -->     (match (node "e" (node "b" (node "a" (leaf) (leaf)) (node "c" (node "d" (leaf) (leaf)) (leaf))) 
;   (node "f" (node "g" (leaf) (leaf)) (leaf))))
;     [(leaf) 0]
;     [(node _ l r) (+ 1 (tree-size l) (tree-size r))])))

; -->     (match (node "e" (node "b" (node "a" (leaf) (leaf)) (node "c" (node "d" (leaf) (leaf)) (leaf))) 
;   (node "f" (node "g" (leaf) (leaf)) (leaf))))
;     [(node _ l r) (+ 1 (tree-size (node "b" (node "a" (leaf) (leaf)) (node "c" (node "d" (leaf) (leaf)) (leaf)))
;                        (tree-size (node "f" (node "g" (leaf) (leaf)) (leaf)))))])))

; (+ 1 1 (tree-size (node "a" (leaf) (leaf)) (node "c" (node "d" (leaf) (leaf)) (leaf)))
;                        (+ 1 (tree-size (node "g" (leaf) (leaf)) (leaf)))))])))

; (+ 1 1 1 (tree-size (node "c" (node "d" (leaf) (leaf)) (leaf)))
;                        (+ 1 1))))])))

; (+ 1 1 1 1 (tree-size (node "d" (leaf) (leaf)) (leaf)))
;                        (+ 1 1))))])))

; (+ 1 1 1 1 1 1 1)

; 7

(description "Size of tree-ii: 7")

(tree-size tree-ii)

; Fill out the following high-level description of `tree-size` in
; terms of the base and recursive cases of the function above.

(description "The size of a tree is:")
(description "+ 0 in the base case.")
(description "+ 1 in the recursive case.")

; -------------------------------------------------------------------
(part "Part 5: Additional Tree Recursion Problems")

;; For the following two functions, follow the prompts to implement the
;; functions using tree recursion. Make sure to write a test suite for
;; the functions using the trees you created from exercise 1.

;; Consider the following recursive skeleton for a function that computes the
;; combined product of all the elements in a tree of numbers.
;;
;; To compute the product of a binary tree:
;; + If the tree is empty, the product is 1.
;; + If the tree is non-empty, the product is the value at the root of the tree
;;   times the products of left and right subtrees.
;;
;; Use this skeleton to complete the implementation of tree-product below:

;;; (tree-product t) -> number
;;;   t: tree? of numbers
;;; Returns the product of all the elements in the tree t.
(define tree-product
  (lambda (t)
    (match t
    [(leaf) 1]
      [(node _ l r) (* (node-value t) (tree-product l) (tree-product r))])))
    
(description "tree-product tests")


(test-case "normal case"
  equal? 6
    (lambda () (tree-product (node 1 (node 2 (leaf) (leaf)) (node 3 (leaf) (leaf))))))

(test-case "normal case - more complex"
  equal? 1440
    (lambda () (tree-product  (node 1 (node 2 (node 3 (leaf) (leaf)) (node 4 (node 5 (leaf) (leaf)) (leaf))) 
    (node 6 (node 2 (leaf) (leaf)) (leaf))))))

;; Consider the following recursive skeleton for a function that computes the
;; number of leaves in a tree.
;;
;; To compute the number of leaves in the tree:
;; + If the tree is empty, the tree is a leaf, so the number of leaves is 1.
;; + If the tree is non-empty, the number of leaves is the sum of the number of
;;   leaves in the left and right subtrees
;;
;; Use this skeleton to complete the implementation of tree-leaf-count below:

;; (tree-leaf-count t) -> number
;;   t: tree?
;; Returns a count of the number of leaves in tree t.
(define tree-leaf-count
  (lambda (t)
    (match t
     [(leaf) 1]
      [(node _ l r) (+ (tree-leaf-count l) (tree-leaf-count r))])))

(description "tree-leaf-count tests")

(test-case "normal case"
  equal? 4
  (lambda () (tree-leaf-count (node 1 (node 2 (leaf) (leaf)) (node 3 (leaf) (leaf))))))

(test-case "normal case - str"
  equal? 7
  (lambda () (tree-leaf-count tree-iii)))
