; final-project.scm
(import data)
(import lab)


;;; (comp1 vec1 vec2) -> bool?
;;; vec1: vector?, non-empty
;;; vec2: vector?, non-empty
;;; Returns a comparator that sorts a vector by the first element of a vector, in ascending order
(define comp1
  (lambda (vec1 vec2)
    (let ([open1 (vector-ref vec1 0)]
          [open2 (vector-ref vec2 0)])
      (> open1 open2))))

;;; (comp2 vec1 vec2) -> bool?
;;; vec1: vector?, non-empty
;;; vec2: vector?, non-empty
;;; Returns a comparator that sorts a vector by the first element of a vector, in descending order
(define comp2
  (lambda (vec1 vec2)
    (let ([open1 (vector-ref vec1 0)]
          [open2 (vector-ref vec2 0)])
      (< open1 open2))))

;;; (swap vec i j) -> void:
;;; vec: vector?, non-empty
;;; i: integer, positive
;;; j : integer, positive
;;; Swaps element in index i and index j
(define swap
  (lambda (vec i j)
    (let ([temp (vector-ref vec i)])
      (begin
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j temp)
    vec))))

;;; find minimum index for selection sort
;;; (find-min-index vec compare i j min-index) -> integer?
;;; vec: vector?
;;; compare: procedure?
;;; i: non-negative integer?
;;; j: non-negative integer?
;;; min-index: non-negative integer?
;;; Return the index of the minimum element in vec starting from index i.
(define find-min-index
  (lambda (vec compare i j min-idx)
    (if (>= j (vector-length vec))
        min-idx
        (if (compare (vector-ref vec j) (vector-ref vec min-idx))
            (find-min-index vec compare i (+ j 1) j)
            (find-min-index vec compare i (+ j 1) min-idx)))))

;;; (sort-loop vec compare i) -> void
;;; vec: vector?
;;; compare: procedure?
;;; i: non-negative integer?
;;; Reorder vector by finding the smallest remaining element in the 
;;; vector and moves it into the next position. 
(define sort-loop
  (lambda (vec compare i)
    (if (< i (vector-length vec))
        (let ([min-idx (find-min-index vec compare i (+ i 1) i)])
          (begin
            (if (not (= i min-idx))
                (swap vec i min-idx)
                void)
            (sort-loop vec compare (+ i 1))))
        void)))


;;; sort function for vector 
;;; (vector-sort! vec compare) -> void
;;; vec: vector?
;;; compare: procedure?
;;; Sorts vector by  selecting the smallest remaining element 
;;; and swapping it into its correct position.
(define vector-sort!
  (lambda (vec compare)
    (sort-loop vec compare 0)))

;;; make copy function for vector
;;; (Copy-vector ori new i) -> void
;;; ori: vector?
;;; new: vector?
;;; Copies the contents of original vector into new vector.
(define copy-vector
  (lambda (ori new i)
    (if (< i (vector-length ori))
        (begin
          (vector-set! new i (vector-ref ori i))
          (copy-vector ori new (+ i 1)))
        void)))

;;; sort procedure for vector
;;; (vector-sort vec compare) -> vec?
;;; vec: vector?
;;; compare: procedure?
;;; Returns a new sorted copy of vec using sort.
(define vector-sort
  (lambda (vec compare)
    (let ([new-vec (make-vector (vector-length vec) 0)])
      (begin
        (copy-vector vec new-vec 0)
        (vector-sort! new-vec compare)
        new-vec))))

;;; (3Highest vec) -> vector?
;;; vec: vector?, a vector with at least 3 elements
;;; Returns a vector with the 3 highest elements
(define 3Highest
  (lambda (vec)
    (let* ([fin (vector-sort vec comp1)]
          [max-data (vector (vector-ref fin 0)(vector-ref fin 1)(vector-ref fin 2))])
        max-data)))

;;; (3Lowest vec) -> vector?
;;; vec: vector?, a vector with at least 3 elements
;;; Returns a vector with the 3 lowest elements
(define 3Lowest
  (lambda (vec)
    (let* ([fin (vector-sort vec comp2)]
          [min-data (vector (vector-ref fin 0)(vector-ref fin 1)(vector-ref fin 2))])
        min-data)))

;;; (new-pair vec) -> pair?
;;; vec: vector?, 2 elements
;;; Returns a pair of 2 elements inside the vector
(define new-pair
  (lambda (vec)
    (pair (vector-ref vec 0)(vector-ref vec 1))))
;;; (extreme-open vec) -> scatterplot?
;;; vec: vector?, non-empty
;;; Creates a scatterplot containing the 3 highest and lowest data points. 
(define extreme-open
  (lambda (vec)
    (let* ([3highest (3Highest vec)]
          [3lowest (3Lowest vec)]
          [highestPoint (vector-map new-pair 3highest)]
          [lowestPoint (vector-map new-pair 3lowest)]
          [highestList (vector->list highestPoint)]
          [lowestList (vector->list lowestPoint)])
  (with-plot-options
    (list 
    (pair "x-label" "Prices at Open")
    (pair "y-label" "Prices at Close")
    (pair "title" "Highest Prices and Lowest Prices at Open"))
     (with-plot-options
        (list
         (pair "x-label" "Prices at Open")
         (pair "y-label" "Prices at Close")
         (pair "title" "Highest Prices and Lowest Prices at Open"))
        (plot-linear
         (with-dataset-options
           (list (pair "background-color" "violet"))
           (dataset-scatter "Highest" highestList))
         (with-dataset-options
           (list (pair "background-color" "cyan"))
           (dataset-scatter "Lowest" lowestList))))))))


;;; (max-roc-2-weeks-largest-increase-helper res vec start data) -> list?
;;; res: number?
;;; vec: vector?, non-empty
;;; start: integer
;;; data: list?, 2 elements
;;; For each window, we compare the rate of change to res,
;;; and if that rate of change is greater (+/- depending on what we are searching for) than res, 
;;; we update res to that rate of change. 
;;; Then we also update data, so that the start of that entry is the same as the first day of the 14-day window of res. 
;;; Otherwise, the function continues to check for a window with a bigger rate of change than res. 
(define max-roc-2-weeks-largest-increase-helper
  (lambda (res vec start data)
    (if (>= (+ start 13) (vector-length vec))
        data
        (let* ([diff-between-2-weeks (/ (- (vector-ref (vector-ref vec (+ start 13)) 0)
                                      (vector-ref (vector-ref vec start) 0)) 14)])
        (if (> diff-between-2-weeks res)
            (max-roc-2-weeks-largest-increase-helper diff-between-2-weeks
                              vec (+ 1 start) (list start diff-between-2-weeks))
            (max-roc-2-weeks-largest-increase-helper res vec (+ 1 start) data))))))

;;; (max-roc-2-weeks-largest-increase vec) -> list?
;;; vec: vector?
;;; Returns the list called from max-roc-2-weeks-largest-increase-helper.
(define max-roc-2-weeks-largest-increase
  (lambda (vec)
    (max-roc-2-weeks-largest-increase-helper -11111111 vec 0 (list))))

;;;; (max-roc-2-weeks-largest-decrease-helper res vec start data) -> list?
;;; res: number?
;;; vec: vector?, non-empty
;;; start: integer
;;; data: list?, 2 elements
;;; For each window, we compare the rate of change to res,
;;; and if that rate of change is smaller (+/- depending on what we are searching for) than res, 
;;; we update res to that rate of change. 
;;; Then we also update data, so that the start of that entry is the same as the first day of the 14-day window of res. 
;;; Otherwise, the function continues to check for a window with a smaller rate of change than res.  
(define max-roc-2-weeks-largest-decrease-helper
  (lambda (res vec start data)
    (if (>= (+ start 13) (vector-length vec))
        data
        (let* ([diff-between-2-weeks (/ (- (vector-ref (vector-ref vec (+ start 13)) 0)
                                      (vector-ref (vector-ref vec start) 0)) 14)])
        (if (< diff-between-2-weeks res)
            (max-roc-2-weeks-largest-decrease-helper diff-between-2-weeks
                              vec (+ 1 start) (list start diff-between-2-weeks))
            (max-roc-2-weeks-largest-decrease-helper res vec (+ 1 start) data))))))

;;; (max-roc-2-weeks-largest-decrease vec) -> list?
;;; vec: vector?
;;; Returns the list called from max-roc-2-weeks-largest-decrease-helper.
(define max-roc-2-weeks-largest-decrease
  (lambda (vec)
    (max-roc-2-weeks-largest-decrease-helper 9999999999 vec 0 (list))))

;;; (roc-covid-helper start end vec) -> list?
;;; start: integer?, positive
;;; end: integer?, positive
;;; vec: vector?, non-empty
;;; Returns a list containing the rate of change during COVID, the starting and ending of COVID
(define roc-covid-helper
  (lambda (start end vec)
    (let* ([diff-in-covid (/ (- (vector-ref (vector-ref vec end) 0)
                          (vector-ref (vector-ref vec start) 0)) (- end start))])
    (list diff-in-covid start end))))

;;; (roc-covid vec) -> list?
;;; vec: vector?
;;; Returns the list generated from roc-covid-helper
(define roc-covid
  (lambda (vec)
    (roc-covid-helper 0 (- (vector-length vec) 1) vec)))


;;; graph 1
(define get-price-at
  (lambda (filtered-data i)
    (vector-ref (vector-ref filtered-data i) 0)))

(define make-price-pair
  (lambda (filtered-data i)
    (pair i (get-price-at filtered-data i))))

(define graph1-time-series-with-windows
  (lambda (filtered-data)
    (let* ([n (vector-length filtered-data)]
           [days (range 0 n)]
           [line-data (map (section make-price-pair filtered-data _) days)])
      (with-plot-options
        (list 
          (pair "x-label" "14-day intervals")
          (pair "y-label" "Opening Price")
          (pair "title" "Prices with Two-Week Window Markers"))
        (plot-linear
          (with-dataset-options
            (list (pair "background-color" "blue"))
            (dataset-line "Opening Prices" line-data)))))))

;;; graph 2
(define compute-two-week-change
  (lambda (filtered-data i)
    (let ([start-price (vector-ref (vector-ref filtered-data i) 0)]
          [end-price (vector-ref (vector-ref filtered-data (+ i 13)) 0)])
      (/ (- end-price start-price) 14))))

(define graph2-two-week-changes
  (lambda (filtered-data)
    (let* ([n (vector-length filtered-data)]
           [max-start (- n 14)]
           [days (range 0 (+ max-start 1))]
           [labels (map number->string days)] 
           [changes (map (section compute-two-week-change filtered-data _)
                        (range 0 (+ max-start 1)))])
      (with-plot-options
        (list 
          (pair "x-label" "Starting Day of Window")
          (pair "y-label" "Two-Week Price Change")
          (pair "title" "Two-Week Rate of Change"))
        (plot-category
          labels
          (with-dataset-options
            (list (pair "background-color" "orange"))
            (dataset-bar "Rate of Change" changes)))))))

(define get-open-prices
  (lambda (filtered-data)
    (map (lambda (i)
           (vector-ref (vector-ref filtered-data i) 0))
         (range 0 (vector-length filtered-data)))))

;;; graph 3
(define graph3-covid-period-analysis
  (lambda (filtered-data)
    (let* ([n (vector-length filtered-data)]
           [covid-start 5]
           [covid-end (- n 4)]
           [days (range 0 n)]
           [prices (get-open-prices filtered-data)]
           [line-data (map pair days prices)] 
           [min-price (apply min prices)]
           [max-price (apply max prices)]
           [start-price (vector-ref (vector-ref filtered-data covid-start) 0)]
           [end-price (vector-ref (vector-ref filtered-data covid-end) 0)]
           [covid-change (- end-price start-price)]
           [start-line (list (pair covid-start min-price) 
                            (pair covid-start max-price))]
           [end-line (list (pair covid-end min-price)      
                          (pair covid-end max-price))])
      (with-plot-options
        (list 
          (pair "x-label" "14-day intervals")
          (pair "y-label" "Opening Price")
          (pair "title" "COVID Period Analysis"))
        (plot-linear
          (with-dataset-options
            (list (pair "background-color" "red")
                  (pair "border-color" "red"))
            (dataset-line "Period Start" start-line))
          (with-dataset-options
            (list (pair "background-color" "green")
                  (pair "border-color" "green"))
            (dataset-line "Period End" end-line))
          (with-dataset-options
            (list (pair "background-color" "pink")
                  (pair "border-color" "pink"))
            (dataset-line "Opening Prices" line-data)))))))

;;; (leap year) -> boolean?
;;; year: integer?
;;; Checks if year is leap year or not. 
(define leap
  (lambda (year)
    (or (and (equal? (remainder year 4) 0) (not (zero? (remainder year 100)))) 
        (equal? (remainder year 400) 0))))
    
;;; (cal-year days years) -> pair?
;;; days: integer?
;;; years: integer?
;;; Calculates the year based on how many days have passed since 1970 (the start
;;; of the timestamp)
(define cal-year
  (lambda (days years)
    (let* ([days_year (if (leap years) 
                          366 
                          365)])
    (if (>= days days_year)
        (cal-year (- days days_year) (+ years 1))
        (pair days years)))))

(define leap-year (vector 31 29 31 30 31 30 31 31 30 31 30 31))
(define norm-year (vector 31 28 31 30 31 30 31 31 30 31 30 31))

;;; (cal-month vec-month days months idx) -> pair?
;;; vec-month: vector?
;;; days: integers?
;;; months: integers?
;;; idx: integers?
;;; Calculates the month based on how many days are left after calculated the 
;;; year
(define cal-month
  (lambda (vec-month days months idx)
    (let* ([days_months (vector-ref vec-month idx)])
    (if (> idx 11)
        (pair days months)
        (if (>= days days_months)
          (cal-month vec-month (- days days_months) (+ months 1) (+ idx 1) )
          (pair days months))))))

;;; (timestamp-change timestamp) -> list?
;;; timestamp: integers?, non-negative
;;; Returns a list contain year, month, day, hour and minute
(define timestamp-change 
  (lambda (timestamp)
    (let* ([min 60]
          [hour 3600]
          [day  86400]
          [days (quotient timestamp day)]
          [leftover (remainder timestamp day)]
          [hours (quotient leftover hour)]
          [leftover-1 (remainder leftover hour)]
          [minutes (quotient leftover-1 min)]
          [middle (cal-year days 1970)] ;timestamp starts from 1970
          [leftover-2 (car middle)]
          [year (cdr middle)]
          [vec-month (if (leap year)
                          leap-year
                          norm-year)]
          [middle-2 (cal-month vec-month leftover-2 1 0)]
          [days-final (+ (car middle-2) 1)]
          [months (cdr middle-2)])
  (list year months days-final hours minutes))))
    

;;; (push head1 head2 sofar) -> vector?
;;; head1: vector?
;;; head2: vector?
;;; sofar: vector?
;;; Returns a vector that contains subvector with opening, closing price and
;;; date of head1 and head2
(define push
  (lambda (head1 head2 sofar)
    (let ([open (string->number (list-ref head1 1))]
          [close (string->number (list-ref head2 2))]
          [date (timestamp-change (string->number (list-ref head1 0)))])
    (if (equal? sofar 0)
      (vector (vector open close date))
      (vector-append sofar (vector (vector open close date)))))))

;;; (filter-data data pos sofar) -> vector?
;;; data: vector?
;;; pos: integer?
;;; sofar: vector?
;;; Returns a vector that contains subvector with opening, closing price and
;;; date of head1 and head2. It actively chooses vector containing price at the start
;;; of the day (00:00) and at the end of the day (23:59) by adding the number of 
;;; minutes of each day 1440. The index of the vector at the start of the day is 0
;;; and the end of the day is 1439 (1440 is 00:00 next day). 
(define filter-data
  (lambda (data pos sofar)
    (if (< (+ pos 1439) (vector-length data))
         (filter-data data (+ pos 1440) (push (vector-ref data pos) (vector-ref data (+ pos 1439)) sofar))
        sofar)))
(part "Scatterplot of 3 highest and lowest data points:")
(with-file-chooser
  (lambda (x)
    (let* ([data (list->vector (list-drop (parse-csv x) 1))]
           [filtered-data (filter-data data 0 0)])
    (extreme-open filtered-data))))
(part "Prices with two-week window markers:")
(with-file-chooser
  (lambda (x)
    (let* ([data (list->vector (list-drop (parse-csv x) 1))]
           [filtered-data (filter-data data 0 0)])
    (graph1-time-series-with-windows filtered-data))))

(part "Two-week rate of change plotted:")
(with-file-chooser
  (lambda (x)
    (let* ([data (list->vector (list-drop (parse-csv x) 1))]
           [filtered-data (filter-data data 0 0)])
    (graph2-two-week-changes filtered-data))))

(part "Largest increase in rate of change:")
(with-file-chooser
  (lambda (x)
    (let* ([data (list->vector (list-drop (parse-csv x) 1))]
           [filtered-data (filter-data data 0 0)])
    (max-roc-2-weeks-largest-increase filtered-data))))

(part "Largest decrease in rate of change:")
(with-file-chooser
  (lambda (x)
    (let* ([data (list->vector (list-drop (parse-csv x) 1))]
           [filtered-data (filter-data data 0 0)])
    (max-roc-2-weeks-largest-decrease filtered-data))))

(part "COVID period rate of change plotted:")
(with-file-chooser
  (lambda (x)
    (let* ([data (list->vector (list-drop (parse-csv x) 1))]
           [filtered-data (filter-data data 0 0)])
    (graph3-covid-period-analysis filtered-data))))

(part "Overall COVID period rate of change:")
(with-file-chooser
  (lambda (x)
    (let* ([data (list->vector (list-drop (parse-csv x) 1))]
           [filtered-data (filter-data data 0 0)])
    (roc-covid filtered-data))))


