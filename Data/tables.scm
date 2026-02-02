;; CSC 151 (SEMESTER)
;; Lab: Tables
;; Authors: Vy Nguyen, Janett Montes
;; Date: October 1, 2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE


(import test)
(import lab)

; --------------------------+-----------------
; Helper functions and Data |
; --------------------------+

(define one-two-three
  (list 
    (list "02123" 42.338947 -70.919635 "Boston" "MA" "Suffolk")
    (list "04123" 44.408078 -70.470703 "Portland" "ME" "Cumberland")
    (list "06123" 41.791776 -72.718832 "Hartford" "CT" "Hartford")
    (list "09123" "" "" "Apo" "AE" "")
    (list "10123" 40.751489 -73.990537 "New York" "NY" "New York")
    (list "12123" 42.565736 -73.629023 "Nassau" "NY" "Rensselaer")
    (list "13123" 43.236407 -75.776918 "North Bay" "NY" "Oneida")
    (list "15123" 40.434436 -80.024817 "West Mifflin" "PA" "Allegheny")
    (list "16123" 40.813208 -80.199126 "Fombell" "PA" "Beaver")
    (list "17123" 40.267518 -76.883734 "Harrisburg" "PA" "Dauphin")
    (list "19123" 39.98238 -75.115369 "Philadelphia" "PA" "Philadelphia")
    (list "21123" 38.974203 -76.594942 "Pasadena" "MD" "Anne Arundel")
    (list "23123" 37.563225 -78.53643 "New Canton" "VA" "Buckingham")
    (list "25123" 38.704805 -81.914474 "Leon" "WV" "Mason")
    (list "28123" 35.529792 -80.947028 "Mount Mourne" "NC" "Iredell")
    (list "29123" 33.766538 -81.247547 "Pelion" "SC" "Lexington")
    (list "30123" 34.244179 -84.845684 "Cassville" "GA" "Bartow")
    (list "32123" 29.022729 -81.172169 "Daytona Beach" "FL" "Volusia")
    (list "35123" 33.544622 -86.929208 "Palmerdale" "AL" "Jefferson")
    (list "36123" 32.233377 -86.208528 "Montgomery" "AL" "Montgomery")
    (list "42123" 36.815869 -85.992039 "Austin" "KY" "Barren")
    (list "43123" 39.969036 -83.011389 "Grove City" "OH" "Franklin")
    (list "44123" 41.602548 -81.525842 "Euclid" "OH" "Cuyahoga")
    (list "45123" 39.288414 -83.583223 "Greenfield" "OH" "Highland")
    (list "46123" 39.721434 -86.488881 "Avon" "IN" "Hendricks")
    (list "47123" 38.281966 -86.484327 "Grantsburg" "IN" "Crawford")
    (list "48123" 42.239933 -83.150823 "Dearborn" "MI" "Wayne")
    (list "50123" 40.80286 -93.518445 "Humeston" "IA" "Wayne")
    (list "54123" 44.211065 -88.151532 "Forest Junction" "WI" "Calumet")
    (list "55123" 44.805989 -93.140878 "Saint Paul" "MN" "Dakota")
    (list "56123" 44.048028 -95.704397 "Currie" "MN" "Murray")
    (list "58123" 46.934596 -97.229718 "Fargo" "ND" "Cass")
    (list "60123" 42.036325 -88.371044 "Elgin" "IL" "Kane")
    (list "63123" 38.547552 -90.324115 "Saint Louis" "MO" "Saint Louis")
    (list "64123" 39.068154 -94.505231 "Kansas City" "MO" "Jackson")
    (list "67123" 37.937595 -97.038364 "Potwin" "KS" "Butler")
    (list "68123" 41.115628 -95.939269 "Bellevue" "NE" "Sarpy")
    (list "69123" 41.049046 -100.391826 "Brady" "NE" "Lincoln")
    (list "70123" 29.951082 -90.206031 "New Orleans" "LA" "Jefferson")
    (list "72123" 35.25881 -91.236983 "Patterson" "AR" "Woodruff")
    (list "73123" 35.535989 -97.202331 "Oklahoma City" "OK" "Oklahoma")
    (list "75123" 32.767268 -96.777626 "Desoto" "TX" "Dallas")
    (list "76123" 32.771419 -97.291484 "Fort Worth" "TX" "Tarrant")
    (list "78123" 29.600748 -98.041576 "Mc Queeney" "TX" "Guadalupe")
    (list "79123" 35.401475 -101.895089 "Amarillo" "TX" "Potter")
    (list "80123" 39.620596 -105.090085 "Littleton" "CO" "Jefferson")
    (list "81123" 37.435812 -105.57348 "Blanca" "CO" "Costilla")
    (list "83123" 42.206598 -110.286804 "La Barge" "WY" "Lincoln")
    (list "84123" 40.65963 -111.919269 "Salt Lake City" "UT" "Salt Lake")
    (list "87123" 35.069999 -106.457841 "Albuquerque" "NM" "Bernalillo")
    (list "88123" 33.717607 -103.145119 "Lingo" "NM" "Roosevelt")
    (list "89123" 35.988844 -115.157513 "Las Vegas" "NV" "Clark")
    (list "91123" 33.786594 -118.298662 "Pasadena" "CA" "Los Angeles")
    (list "92123" 32.810983 -117.136812 "San Diego" "CA" "San Diego")
    (list "94123" 37.79967 -122.435732 "San Francisco" "CA" "San Francisco")
    (list "95123" 37.189396 -121.705327 "San Jose" "CA" "Santa Clara")
    (list "96123" 40.776154 -120.326259 "Ravendale" "CA" "Lassen")
    (list "97123" 45.458397 -122.977963 "Hillsboro" "OR" "Washington")
    (list "99123" 47.913065 -119.042562 "Electric City" "WA" "Grant")))

; As you might be able to tell, the lines create a table of cities in
; which each entry has a zip code (a string), a latitude, a longitude,
; a city, a state, and a county.  These data are taken from a much
; longer list at 
; `<https://simplemaps.com/data/us-zips>`.  
; We have extracted only the zip codes that end in `123`.  

; You will find that it's often useful to work with a subset of a data
; set before trying to work with the whole data set.  In this case, 
; there are 42,741 lines in the original data set.

; +-------------+----------------------------------------------------
; | Preparation |
; +-------------+

; 1. Make sure to save this file as tables.scm.


; ---------------------------+-------------------
; Exercise 1: Sorting by city
; ---------------------------+
(problem "Problem 1: Sorting by city")

; Write a procedure, `sort-by-city`, that will
; sort the data in one-two-three by city name.  

; Hint: You may find it helpful to write a separate procedure that
; compares two entries by city, called `compare-by-city<?`
; Hint 2: You may also find it helpful to write
; a separate procedure that extracts the city from an entry.  
; You might call it `extract-city`. Use algorithmic decomposition!

; Don't forget to include documentation!


;(compare-by-city<?
;   (list "76123" 32.771419 -97.291484 "Fort Worth" "TX" "Tarrant")
;   (list "78123" 29.600748 -98.041576 "Mc Queeney" "TX" "Guadalupe"))
;#t
;(compare-by-city<?
;   (list "78123" 29.600748 -98.041576 "Mc Queeney" "TX" "Guadalupe")
;   (list "79123" 35.401475 -101.895089 "Amarillo" "TX" "Potter"))
;#f

;;; (compare-by-city<? city1 city2) -> boolean?
;;;   city1 : list?
;;;   city2 : list?
;;; Returns true if city 1 is infront of city 2 if not returns false
(define compare-by-city<?
  (lambda (city1 city2)
    (string-ci<? (cadddr city1) (cadddr city2))))

;;; (compare-by-city<? lst) -> lst?
;;;   lst : list?
;;; Returns the list of cities
(define extract-city
  (lambda (lst)
    (map (section cadddr _) lst)))

;;; (sort-by-city lst) -> lst?
;;;   lst : list?
;;; Returns the list of cities sorted alphabetically
(define sort-by-city
  (lambda (list) 
    (sort (extract-city list) string-ci<?)))

; (sort-by-city one-two-three)

(test-case "compare-by-city<? false example"
  equal? #f
    (lambda () (compare-by-city<?
     (list "78123" 29.600748 -98.041576 "Mc Queeney" "TX" "Guadalupe")
     (list "79123" 35.401475 -101.895089 "Amarillo" "TX" "Potter"))))
     
(test-case "compare-by-city<? true example"
  equal? #t
    (lambda () (compare-by-city<?
   (list "76123" 32.771419 -97.291484 "Fort Worth" "TX" "Tarrant")
   (list "78123" 29.600748 -98.041576 "Mc Queeney" "TX" "Guadalupe"))))


; ------------------------------+-------------------
; Exercise 2: Organizing entries
; ------------------------------+
(problem "Problem 2: Organizing entries")

; (a)  
; Although the original data present the zip code first, we might find
; it easier to see the city and state first.  Document and write a procedure,
; `zip-first->city-first`, that takes one parameter, a list of the form
; `'(zip latitude longitude city state county)`, and returns a list
; of the form `'(city state zip latitude longitude county)`.

; > (zip-first->city-first (list "70123" 29.951082 -90.206031 "New Orleans" "LA" "Jefferson"))
;  ("New Orleans" "LA" "70123" 29.951082 -90.206031 "Jefferson")

; Hint: You will probably find it useful to break the list into three
; parts and then append them those parts together.

;;; (zip-first->city-first lst) -> lst?
;;;   lst : list?
;;; returns a list of the form
; `'(zip latitude longitude city state county)`, and returns a list
; of the form `'(city state zip latitude longitude county)`
(define zip-first->city-first
  (lambda (lst)
    (list (list-ref lst 3)
          (list-ref lst 4)
          (list-ref lst 0)
          (list-ref lst 1)
          (list-ref lst 2)
          (list-ref lst 5))))

    

(test-case "organizing entries"
  equal? (list "New Orleans" "LA" "70123" 29.951082 -90.206031 "Jefferson")
  (lambda () (zip-first->city-first (list "70123" 29.951082 -90.206031 "New Orleans" "LA" "Jefferson"))))

; (b)
; Create a new table, `cities123` by mapping `zip-first->city-first` over the
; list `one-two-three`.

;;; (cities123) -> lst?
;;;   lst : list?
;;; returns an updated list with the new format
(define cities123
  (map zip-first->city-first one-two-three))

; -----------------------------------+-------------------
; Exercise 3: The Northernmost cities
; -----------------------------------+
(problem "Problem 3: Northernmost cities")

; In todays reading, we noted that
; we could find the Northernmost city in a list of cities by organizing 
; the cities by latitude from highest to lowest.  

; (a)
; Sketch, but do not run, the code that you would use to order
; `cities123` by latitude from highest to lowest.

; Your idea goes below as a comment
; (define city-latitude>?
;   (lambda (city1 city2)
;     (> (cadddr city1) (cadddr city2))))
; (sort (cities123 one-two-three) city-latitude>?)
; (b)
; What problems, if any, do you anticipate from running those
; procedures?

; Your idea(s) go below as a comment
; the cities latitude is a string when it needs a number
; ----------------------------------+-------------------
; Exercise 4: The Northernmost cities, revisited
; ----------------------------------+
(problem "Problem 4: Northernmost cities revisited")

; As you may have noted, there's a small problem with the data that we have.
; In particular, we have an entry with no latitude and longitude.  If we
; try to compare the string `""` to a number, we will get an error.  You
; may recall that we said that you often start your analysis by *cleaning*
; the data.  Here's one instance in which we need to clean data.

; (a) 
; Write a procedure, `nonempty-lat?` which takes in a list with city
; data in the cities123 format, and returns #t if the latitude is a number

;;; (checks-for-num) -> lst?
;;;   lst : list?
;;; filters out the non number
(define checks-for-num
  (lambda (lst)
    (filter number? lst)
    ))
;;; (non-empty-lat?) -> lst?
;;;   lst : list?
;;; returns a boolean if the list has a latitue or not
(define nonempty-lat?
  (lambda (lst)
    (not (zero? (length (checks-for-num lst))))))
  
(test-case "empty latitude"
  equal? #f
  (lambda () (nonempty-lat? (list "09123" "" "" "Apo" "AE" ""))))

(test-case "not empty latitude"
  equal? #t
  (lambda () (nonempty-lat? (list "New Orleans" "LA" "70123" 29.951082 -90.206031 "Jefferson"))))

; (b)
; Finish your instructions for ordering `cities123` from highest
; latitude to least. You'll need to use your answer from part a to filter the 
; data first

;;; (cities1234) -> lst?
;;;   lst : list?
;;; returns the list that has non empty values of latitude
(define cities1234
  (lambda (lst)
    (filter nonempty-lat? lst)))
;;; (city-latitude? city1 city2) -> boolean?
;;;   city1 : number?
;;;   city2 : number?
;;; compares between 2 cities and checks to see which has the greater latitude
(define city-latitude>?
  (lambda (city1 city2)
    (> (cadddr city1) (cadddr city2))))
(sort (cities1234 cities123) city-latitude>?)
; +-------------+----------------------------------------------------
; | Wrapping up |
; +-------------+

; Make sure that you save tables.scm as a text file and submit
; that file on Gradescope.

; ----------------+-------------------
; Extra problems
; ----------------+

; If you find that you have extra time, you may try any of the following.

; ------------------------------+-------------------
; Extra 1: Tallying states
; ------------------------------+

; (a)
; Write an instruction to extract just the states from `cities123`.

; (b)
; Create a tally of the states using `tally-all`.

(define state-tally
  {??})

; (c)
; Order that tally from largest to smallest.

(define ordered-state-tally
  {??})

; (d)
; Determine which three states appear most often in this list.

; Your answer goes below as a comment

; --------------------------------+-------------------
; Extra 2: Tallying by meridian
; --------------------------------+

; It is often useful to think about the whole-number meridian of a city,
; which we can find by rounding the longitude to the nearest whole number.
; For example, although the longitude of St. Louis is -90.324115 and the
; longitude of New Orleans is -90.206031, we would say that they are both
; on the -90 degree meridian.

; Write expression(s) to tally cities by their nearest whole number meridian.





