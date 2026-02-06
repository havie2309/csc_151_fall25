; exploring-algorithms.scm
;; CSC 151 (SEMESTER)
;; Coding Challenge 1
;; Authors: NGUYEN THI HA VY
;; Date: 9th September 2025
;; Acknowledgements: Website for recipe: https://www.allrecipes.com/recipe/9861/chocolate-crinkles-ii/

; PROBLEM 1: Recipes as algorithms
; a) 
; CHOCO CRINKLES RECIPE
; 2 cups white sugar
; 1 cup unsweetened cocoa powder
; 1/2 cup vegetable oil
; 4 large eggs
; 2 teaspoons vanilla extract
; 2 cups all-purpose flour
; 2 teaspoons baking powder
; 1/2 teaspoon salt
; 1/2 cup confectioners' sugar
; Step 1: Gather ingredients
; Step 2: Mix sugar, cocoa, and oil together in a medium bowl. Beat in eggs, one at a time, until combined. Stir in vanilla.
; Step 3: Combine flour, baking powder, and salt in another bowl. Gradually stir flour mixture into the cocoa mixture until thoroughly mixed. Cover dough and refrigerate for at least 4 hours.
; Step 4: Preheat the oven to 350 degrees F (175 degrees C). Line two cookie sheets with parchment paper. Roll or scoop chilled dough into 1-inch balls. Coat each ball in confectioners' sugar and place 1 inch apart on the prepared cookie sheets.
; Step 5: Bake in the preheated oven for 10 to 12 minutes. Let stand on the cookie sheet for a few minutes before transferring to wire racks to cool. Repeat Step 4 to make the remaining batches.

; b) 
; Parameter: flour, sugar, cocoa powder, baking powder, salt, oil, eggs, vanilla, confectioners' sugar
; Variable: DB = "Dough balls", Oven-temp = "Oven temperature", Texture = "cookie texture".

; c)
; Flour must be wheat flour
; Sugar must be granulated sugar
; Cocoa powder must be unsweetened
; Baking powder is different from baking soda
; Salt should be fine salt
; Oil must be a natural vegetable oil
; Eggs are at room temperature
; Vanilla is in the liquid 
; Confectioners' sugar should be powdered for coating

; d)
; Repetition:
; Rolling dough into multiple balls and coating each in sugar
; Placing dough balls onto the baking sheet until all the dough is used
; Conditionals:
; Dough must be refrigerated until firm
; Bake until tops are cracked

; e) 
; Basic operations or subroutines the recipe author assumes that the reader already knows: 
; Measure all ingredients accurately.
; Mix wet and dry ingredients separately.
; Beat an egg into the batter, then repeat for the remaining eggs.
; Roll the dough into balls, coat them, and repeat for the remaining dough balls.
; Line a baking sheet with parchment paper each time and repeat.
; Preheat the oven before baking and repeat.

; f) 
; STEP 1: Gather ingredients
; Get all the ingredients listed in the recipe
; Measure them carefully: use dry cups for flour, sugar, cocoa; liquid cups for oil; table spoons for salt, baking powder, vanilla

; STEP 2: Mix cocoa mixture
; In a bowl, add sugar, cocoa powder, and oil. Stir until it looks smooth
; Beat in eggs into other bowl, then pour it into the cocoa
; Stir in vanilla until everything is blended

; STEP 3: Mix flour mixture
; In another bowl, add flour, baking powder, and salt and stir until it is combined
; Slowly add this flour mixture into the cocoa mixture
; Cover the bowl with plastic wrap and put it in the refrigerator for at least 4 hours

; STEP 4: Prepare oven and cookie dough
; Preheat oven: Turn the oven to 350°F
; Cover the baking sheets with parchment paper
; Take the chilled dough from the fridge. Scoop a spoonfulof dough, roll it into a ball with hands. Then roll the ball in powdered sugar unitll fully coated.
; Place the coated dough balls on the baking sheet

; STEP 5: Bake the cookies
; Put the baking sheet in the oven
; Bake for 10-12 minutes
; Let the cookies cool down for a few minutes
; Roll the cookies in powdered sugar again before enjoy

; PROBLEM 2: Timestamps
; This timestamp is for 12:00am GMT on May 25, 1977, the original release of Star Wars Episeode IV
; This day was a Wednesday

(define seconds-in-day (* 24 60 60))

; Example 1:
(define timestamp-1 233366400)
(define day-of-week-1 (remainder (+ 4 (quotient timestamp-1 seconds-in-day)) 7)) 
; day-of-week-1 should now be define to 3, which represents Wednesday

; Example 2:
(define timestamp-2 86400)
(define day-of-week-2 (remainder(+ 4 (quotient timestamp-2 seconds-in-day)) 7)) 
; day-of-week-2 should now be define to 5, which represents Friday

; Example 3: 
(define timestamp-3 604800)
(define day-of-week-3 (remainder(+ 4 (quotient timestamp-3 seconds-in-day)) 7)) 
; day-of-week-3 should now be define to 4, which represents Thursday

; Example 4:
(define timestamp-4 1451520)
(define day-of-week-4 (remainder(+ 4 (quotient timestamp-4 seconds-in-day)) 7)) 
; day-of-week-4 should now be define to 6, which represents Saturday


; PROBLEM 3: Scoring athletes
(define judge1 7)

(define judge2 10)

(define judge3 5)

(define judge4 8)

(define judge5 6)

(define judge6 9)

(define judge7 8)

(define judge8 6)


; a) 
(define total-score (+ judge1 judge2 judge3 judge4 judge5 judge6 judge7 judge8))

(define average-score (/ total-score 8))

(define lowest-score (min judge1 judge2 judge3 judge4 judge5 judge6 judge7 judge8))

(define highest-score (max judge1 judge2 judge3 judge4 judge5 judge6 judge7 judge8))

(define robust-average (/ (- total-score highest-score lowest-score) 6))


; b)
; Calculate a version of robust-average rounded to the nearest tenth
(define round-robust-average 
  (/ (round (* robust-average 10)) 10) )
round-robust-average
 
; Another set of instructions for rounding robust-average to use exactly precision digits after the decimal point
(define precision 3)
(define round-robust-average-precision 
  (/ (round (* robust-average (expt 10 precision))) (expt 10 precision) ))
round-robust-average-precision





