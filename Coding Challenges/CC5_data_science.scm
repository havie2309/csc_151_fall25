; data-science.scm
; CC5-template.scm

;; CSC-151-04 Fall 2025
;; Instructor: Nicole Eikmeier 
;; Author: Vy Nguyen
;; Acknowledgements:
    ;; https://simplemaps.com/data/us-zips for us-zips data

(import data)
(import lab)
(import test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Problem 1 ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; part a
(problem "Problem 1a: deg->rad")

; coordinate data that may be helpful in testing
(define DesMoines (pair 41.672687 -93.572173))
(define Grinnell (pair 41.685324 -92.630258))

;;; (deg->rad deg) -> number?
;;;   deg : number?
;;; Converts an angle `deg` from degrees to radians.
;;; Returns the equivalent angle in radians, computed as (π * deg / 180).
(define deg->rad
  (lambda (deg)
    (/(* pi deg) 180)))

(test-case "Convert from degree to radian"
  equal? 0.7258754545874336
  (lambda () (deg->rad 41.5896)))

(test-case "Convert 180 from degree to radian"
  equal? 3.141592653589793
  (lambda () (deg->rad 180)))


(test-case "Convert 90 from degree to radian"
  equal? 1.5707963267948966 
  (lambda () (deg->rad 90)))

;; part b
(problem "Problem 1b: haversine")


;;; (a cor1 cor2) -> number?
;;;   cor1 : pair?, where (car cor1) and (cdr cor1) are numbers (degree)
;;;   cor2 : pair?, where (car cor2) and (cdr cor2) are numbers (degree)
;;; Computes an intermediate value used in the haversine formula for calculating
;;; the great-circle distance between two points on a sphere.
(define a
  (lambda (cor1 cor2)
    (let ([sin1 (square (sin (/ (- (deg->rad (car cor2)) (deg->rad (car cor1))) 2)))]
          [sin2 (square (sin (/ (- (deg->rad (cdr cor2)) (deg->rad (cdr cor1))) 2)))]) 
      (+ sin1 (* (cos (deg->rad (car cor1))) (cos (deg->rad (car cor2))) sin2)))))

;;; (haversine cor1 cor2) -> number?
;;;   cor1 : pair?, where (car cor1) and (cdr cor1) are numbers (degree)
;;;   cor2 : pair?, where (car cor2) and (cdr cor2) are numbers (degree)
;;; Computes the great-circle distance between two points on Earth
;;; using the haversine formula. The result is returned in kilometers.
(define haversine
  (lambda (cor1 cor2)
    (let ([a (a cor1 cor2)])
    (* 6371 2 (atan(* (sqrt a)(sqrt (- 1 a))))))))

(define IAC (pair 41.6611 -91.5302))
(define CHI (pair 41.8119 -87.6873))

(test-case "Haversine distance between Des Moines and Grinnell"
  equal? 78.23482791072104
  (lambda () (haversine DesMoines Grinnell)))

(test-case "Haversine distance between Des Moines and Iowa City"
  equal? 169.58757618666806
  (lambda () (haversine DesMoines IAC)))

(test-case "Haversine distance between Des Moines and Chicago"
  equal? 487.6879594020877
  (lambda () (haversine DesMoines CHI)))


;; part c
(problem "Problem 1c: closest cities to Des Moines")

;;; (non-empty? lst) -> boolean?
;;;   lst : list?
;;; Determines whether `lst` is a list with at least three elements
;;; and ensures that the second and third elements are non-empty strings.
(define non-empty?
  (lambda (lst)
    (and (list? lst)
         (>= (length lst) 3)
         (not (string=? (cadr lst) ""))
         (not (string=? (caddr lst) "")))))

;;; (with-file-chooser handler) -> list?
;;;   handler : procedure?
;;; Reads a CSV file selected by the user, filters valid city data, 
;;; converts coordinate values to radians, and calculates distances 
;;; from a reference location (DSM). 
;;; Returns a list of the five nearest cities to the reference point.
(with-file-chooser
  (lambda (x)
      (let* ([data (parse-csv x)]
             [new-cities (filter non-empty? data)]
             [list-pair (lambda (cor) 
               (pair (string->number (cadr cor))
                     (string->number (caddr cor))))]
             [distance<=? (lambda (cor1 cor2) 
                (<= (haversine (list-pair cor1) DesMoines)
                    (haversine (list-pair cor2) DesMoines)))])
    (list-take (sort new-cities distance<=?) 5))))
;;; RESULT: 
;;; "50981" "41.672687" "-93.572173" "Des Moines" "IA" "Polk"
;;; "50980" "41.672687" "-93.572173" "Des Moines" "IA" "Polk"
;;; "50950" "41.672687" "-93.572173" "Des Moines" "IA" "Polk"
;;; "50947" "41.672687" "-93.572173" "Des Moines" "IA" "Polk"
;;; "50940" "41.672687" "-93.572173" "Des Moines" "IA" "Polk"

;;;; Part d
(problem "Problem 1d: nicer output")
;;; (place-name lst) -> string?
;;;   lst : list?
;;; Extracts the place name and country from `lst` 
;;; and combines them into a single string separated by a comma.
(define place-name
  (lambda (lst)
    (string-append (cadddr lst) ", " (cadr (cdddr lst)))))

(test-case "Nicer name for New York"
  equal? "New York, NY"
  (lambda () (place-name (list "10123" 40.751489 -73.990537 "New York" "NY" "New York"))))

(test-case "Nicer name for Humeston"
  equal? "Humeston, IA"
  (lambda () (place-name (list "50123" 40.80286 -93.518445 "Humeston" "IA" "Wayne"))))

(test-case "Nicer name Saint Paul"
  equal? "Saint Paul, MN"
  (lambda () (place-name (list "55123" 44.805989 -93.140878 "Saint Paul" "MN" "Dakota"))))

(test-case "Nicer name Saint Paul"
  equal? "Pasadena, CA"
  (lambda () (place-name (list "91123" 33.786594 -118.298662 "Pasadena" "CA" "Los Angeles"))))



