;; CSC 151 Fall 2025
;; Lab: Displaying Data (displaying-data.scm)
;; Authors: Odysseus Wernke, Vy Nguyen
;; Date: 11/3/2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import lab)
(import test)
(import data)

 ;; In this lab, you will have the opportunity to explore some of
 ;; the visualizations available through Scamper
 
 
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; Lab Preparation

; In this lab we will return to the us zip code data and the 
; Jane Eyre text from earlier in the semester. 

(define JaneEyre "pg1260.txt")

; Download the zip code data from the lab page if you haven't already. 
; The data is stored in the following format
; "02123", 42.338947, -70.919635, "Boston", "MA", "Suffolk
; zip-code, latitude, longitude, city, state, county

; these procedures will be helpful
(define zip-ends-with
  (lambda (city three-char-suffix)
    (string=? (substring (car city) 2 5) three-char-suffix)))

(define zip-starts-with
  (lambda (city three-char-prefix)
    (string=? (substring (car city) 0 3) three-char-prefix)))
 
 (define useful-entry?
  (lambda (city)
    (not (or (string=? "" (cadr city))
             (string=? "" (caddr city))))))
    
; TODO: Briefly describe what each of the above procedures does:
; zip-ends-with checks if the zip code end with the 3 numbers given
; zip-starts-with checks if the zip code start with the 3 numbers given
 
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 1 : Plotting Cities")

; a. Recall how to read in data from files using with-file-chooser
; and write code to turn the file us-zip-codes.csv into a table

; b. Using `filter`, write an expression that selects only a select 
; number of cities based on the prefix or suffix of the zip code, and 
; save it in a local binding called zips1

; c. Using map, extract only the latitude and longitude from zips1.
; You may want to write a separate helper that extracts 
; a latitude and longitude from a single entry, and changes them 
; from a string to a number. These data should be stored as a pair.

; d. Display the data using Scamper's plotting functionality. Note: 
; you may need to filter out data with missing latitude and or longitude
; in part c, which you may not have done. You're looking to remove
; empty strings for either latitude or longitude. (There may be a 
; helper function at the top of this file)

; TODO: Your code goes below. Rather than writing a separate solution for
; each of a, b, c, and d, you can build on your previous answer once you 
; get one piece working. 

; NOTE: Your final solution may have a form like: 
(define coordinateExtractor
  (lambda (city)
    (pair (string->number (cadr city)) (string->number (caddr city)))))

(with-file-chooser
   (lambda (file)
     (let* ([parsedZipCodes (filter (section useful-entry? _) (parse-csv file))]
            [zips1 (filter (section zip-starts-with _ "111") parsedZipCodes)]
            [coordinatePairList-zips1 (map coordinateExtractor zips1)])
     (with-plot-options
       (list (pair "x-label" "Latitude")
             (pair "y-label" "Longtitude")
             (pair "title" "Latitude and Longitude"))
     (plot-linear
       (dataset-scatter "Latitude and Longitude"
         coordinatePairList-zips1))))))

(with-file-chooser
  (lambda (file)
    file))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 2 : Plotting Cities, revisited")
; For this problem, start with a copy of your code from Problem 1, as much of 
; the structure will be the same. 

; a. This time, create 2 different subsets of the zip code data using
; local bindings. Call them zips1 and zips2. 

; b. Write an expression or series of expressions that plots the
; two sets of points on the same figure

; c. Make sure your plot has a title, legends, and labels on 
; the x-axis and the y-axis

(with-file-chooser
   (lambda (file)
     (let* ([parsedZipCodes (filter (section useful-entry? _) (parse-csv file))]
            [zips1 (filter (section zip-starts-with _ "111") parsedZipCodes)]
            [zips2 (filter (section zip-starts-with _ "555") parsedZipCodes)]
            [coordinatePairList-zips1 (map coordinateExtractor zips1)]
            [coordinatePairList-zips2 (map coordinateExtractor zips2)])
     (with-plot-options
       (list (pair "x-label" "Latitude")
             (pair "y-label" "Longtitude")
             (pair "title" "Latitude and Longitude"))
     (plot-linear
       (dataset-scatter "(zip pre-fix: 111)"
         coordinatePairList-zips1)
       (dataset-scatter "(zip pre-fix: 555)"
         coordinatePairList-zips2))))))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 3 : Plotting Cities, re-revisited")

; a. Re-write your code from problem 2 so that zips1 has cities
; that start with "021" and zips2 has cities that end with "021"

; b. Write an expression or expressions to plot the cities in zips1 so
; that those north of 39.72 are one color and those south of 39.72 are
; another color. NOTE: North/South corresponds to latitude

; c. Write an expression or expressions to plot the cities in zips1 and
; zips2 using four colors: one for `zips1` north of 39.72, one for 
; `zips1` south of 39.72, one for `zips2` north of 39.72, and one for
; `zips2` south of 39.72.

(with-file-chooser
   (lambda (file)
     (let* ([parsedZipCodes (filter (section useful-entry? _) (parse-csv file))]
            [zips1 (filter (section zip-starts-with _ "021") parsedZipCodes)]
            [zips1-coordinatePairList (map coordinateExtractor zips1)]
            [zips1-north (filter (section >= (car _) 39.72) zips1-coordinatePairList)]
            [zips1-south (filter (section < (car _) 39.72) zips1-coordinatePairList)]
            [zips2 (filter (section zip-ends-with _ "021") parsedZipCodes)]
            [zips2-coordinatePairList (map coordinateExtractor zips2)]
            [zips2-north (filter (section >= (car _) 39.72) zips2-coordinatePairList)]
            [zips2-south (filter (section < (car _) 39.72) zips2-coordinatePairList)])
     ; zips1-coordinatePairList)))
     (with-plot-options
       (list (pair "x-label" "Latitude")
             (pair "y-label" "Longtitude")
             (pair "title" "Latitude and Longitude"))
     (plot-linear
       (with-dataset-options
         (list (pair "background-color" "blue"))
         (dataset-scatter "zips1-north"
           zips1-north))
       ; (with-dataset-options
       ;   (list (pair "background-color" "purple"))
       ;   (dataset-scatter "Latitude and Longitude"
       ;     zips1-south))
       (with-dataset-options
         (list (pair "background-color" "red"))
         (dataset-scatter "zips2-north"
           zips2-north))
       (with-dataset-options
         (list (pair "background-color" "orange"))
         (dataset-scatter "zips2-south"
           zips2-south)))))))


  




;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 4 : Detour: Exploring colors")

; Here's a simple expression to plot some points.

(plot-linear
  (with-dataset-options
    (list (pair "background-color" "magenta")
          (pair "border-color" "purple"))
    (dataset-scatter "sample-data"
      (list (pair 0 12)
            (pair 1 9)
            (pair 4 15)))))

; Experiment with color names to find out which colors are 
; supported in Scamper

; The following colors are supported: red, grey, orange, blue, purple, white, yellow, brown, black
; violet, cyan, magenta, etc.
; The following colors are not supported: We are not sure, as many colors are supported.

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 5 : Categorical Data")

; a. Consider the following helper function, and write 
; documentation for it

; TODO: Documentation for `categorize`




;;; (categorize city) -> string?
;;; city : list?
;;; This returns a string describing the city entry.
(define categorize
  (lambda (city)
    (cond
      [(not (useful-entry? city)) "Unknown"]
      [(> (string->number (cadr city)) 39.72) "North"]
      [(< (string->number (cadr city)) 39.72) "South"]
      [else "Other"])))


; b. Using `tally-all`, `map`, and `categorize`, create summary
; information for `zips1`. 

(with-file-chooser
   (lambda (file)
     (let* ([parsedZipCodes (filter (section useful-entry? _) (parse-csv file))]
            [zips1 (filter (section zip-starts-with _ "021") parsedZipCodes)]
            [zips1-coordinatePairList (map coordinateExtractor zips1)]
            [zips1-north (filter (section >= (car _) 39.72) zips1-coordinatePairList)]
            [zips1-south (filter (section < (car _) 39.72) zips1-coordinatePairList)]
            [zips2 (filter (section zip-ends-with _ "021") parsedZipCodes)]
            [zips2-coordinatePairList (map coordinateExtractor zips2)]
            [zips2-north (filter (section >= (car _) 39.72) zips2-coordinatePairList)]
            [zips2-south (filter (section < (car _) 39.72) zips2-coordinatePairList)]
            [zips1-tallied (tally-all (map categorize zips1))]
            [zips2-tallied (tally-all (map categorize zips1))])
        (with-plot-options
     (list (pair "x-label" "Latitude")
           (pair "y-label" "Longtitude")
           (pair "title" "Latitude and Longitude"))
   (plot-linear
     (with-dataset-options
       (list (pair "background-color" "blue"))
       (dataset-bar "North/South Frequencies"
         (list zips1-tallied zips2-tallied))))))))

; c. Using a plot, make a histogram (bar chart) of these
; values.

; d. Repeat your work for other samples of the zips data. 

; e. Then repeat for the entire zips data.

; f. Given those results, how representative do you feel 
; your sample data are?


;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 6 : Tallying different types")

; a. Write a procedure, `tally-alphabetic`, that, given a list of characters,
; determines how many are alphabetic.  

(define tally-alphabetic
  {??})

(test-case "tally-alphabetic: mix of alphabetic and numeric"
  equal? 3
  (lambda () (tally-alphabetic (list #\a #\b #\3 #\d))))
(test-case "tally-alphabetic: ncluding special characters"
  equal? 6
  (lambda () (tally-alphabetic (string->list "a and b3 & q4"))))


; *Hint*: One approach is to filter the alphabetic characters and then
; find out how long the list is.

; *Hint*: `char-alphabetic?` is a built-in Scheme procedure.

; b. Write a procedure, `tally-digits`, that, given a list of characters,
; determines how many are digits.

(define tally-digits
  {??})

(test-case "tally-digits:mix of alphabetic and numeric"
  equal? 1
  (lambda () (tally-digits (list #\a #\b #\3 #\d))))
(test-case "tally-digits: including special characters"
  equal? 2
  (lambda () (tally-digits (string->list "a and b3 & q4"))))

; *Hint*: `char-numeric?` is a built-in Scheme procedure.

; c. Write a procedure, `tally-whitespace`, that, given a list of characters,
; determines how many are whitespace.

(define tally-whitespace
  {??})

(test-case "tally-whitespace:mix of types"
  equal? 4
  (lambda () (tally-whitespace (string->list "a and b3 & q4"))))

; *Hint*: `char-whitespace?` is a built-in Scheme procedure.

; d. Write a procedure, `tally-other`, that, given a list of characters,
; determines how many are neither alphabetic, nor digits, nor whitespace.

(define tally-other
  {??})

(test-case "tally-other: mix of types"
  equal? 1
  (lambda () (tally-other (string->list "a and b3 & q4"))))
  
; e. Write a procedure, `char-tallies`, that, given a string, produces
; a list of four numbers corresponding to the four numbers above.

(define char-tallies
  {??})

(test-case "char-tallies: mix of types"
  equal? (list 6 2 4 1)
  (lambda () (char-tallies "a and b3 & q4")))
  


;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 7 : Visualizing tallies")

; a. Write a procedure, `explore-strings`, that takes a 
; list of 3 strings as input and produces a histogram 
; (bar chart) of the distribution of characters
; in the strings using `char-tallies`.


(define explore-strings
  {??})

; b. Run `explore-strings` on a few sample inputs.

; (explore-strings 
;   (list 
;    "Now is the time for all good men to come to the aid of their country."
;    "A 1 and a 2 and a 3 and ...."
;    "'Twas brillig and the slithy toves; did gyre and gimble in the wabe."))

; (explore-strings
;   (list
;     "And it's 2-4-6-8, ain't never too late Me and my radio truckin' on through the night 3-5-7-9 on a double white line Motorway sun comin' up with the mornin' light"
;     "And all the world is biscuit-shaped It's just for me to feed my face And I can see, hear, smell, touch, taste And I've got 1, 2, 3, 4, 5"
;     "Workin' 9 to 5, what a way to make a livin' Barely gettin' by, it's all takin' and no givin' They just use your mind and they never give you credit It's enough to drive you crazy if you let it"))

; c. Run `explore-strings` on lines 100-103 of Jane Eyre.

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Problem 8: Exploring strings, revisited")

; Arrange for the histogram you created in the previous exercise to have
; an appropriate legend, title, and other labels.  

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; For those with extra time
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Extra 1: More data")

; Find the Chernobyl_Chemical_Radiation.csv dataset from a previous lab
; Remind yourself of the dataset and the structure 

; a. Write a procedure to categorize the values stored in index 5 
; (those correspond to I_131_(Bq/m3) ). Note that we are not telling you
; what the categories should be, do a little analysis of the data to 
; determine appropriate categories. 

; b. Create a bar char to display the categorical data you created in
; part a.

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(part "Extra 2: Other types of plots")

; a. Review the scamper documentation for dataset-pie, dataset-polar, and
; dataset-radar. Explore making plots with plot-radial as a wrapper. 
; You can use made up data or find some data. 

; b. In what instances might any of these types of plots be useful?

