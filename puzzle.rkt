;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following line is REQUIRED (do not remove)
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Davison Tieu 20601826
;; CS 135 Fall 2015
;; Assignment 10 Question 02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define puzz04 (read-puzzle "puzzle04.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions:

;; (al-assign lst) takes a (listof Any) and creates a (listof (list Any Nat))
;; where Nat is the corresponding index for that element in the original list
;; al-assign: (listof Any) -> (listof (list Any Nat))
;; Examples:
(check-expect (al-assign '("yes" "no" "maybe"))
              '(("yes" 0) ("no" 1) ("maybe" 2)))
(check-expect (al-assign '(1 2 3)) '((1 0) (2 1) (3 2)))


(define (al-assign lst)
       (map list lst (build-list (length lst) identity)))



;; REQUIRED FUNCTIONS:


;; (transpose g) takes a Grid and tranposes it, where the rows and columns are
;; switched in the new Grid
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose empty) empty)
(check-expect (transpose '((#\A #\X) (#\B #\Y) (#\C #\Z))) grid-abc)
(check-expect (transpose '((#\A))) '((#\A)))


(define (transpose g)
  (cond
    [(or (empty? g) (empty? (first g))) empty]
    [else (cons (map (lambda (x) (first x)) g)
                (transpose (map (lambda (x) (rest x)) g)))]))


;; Local helpers for find-wpos:

;; (find-wpos-al loc row) consumes a (listof Char) that has been processed and
;; has assigned index values and a row #. Produces a (listof WPos) that are
;; possible within the row
;; find-wpos-al: (listof (list Char Nat)) Nat -> (listof WPos)

;; (wpos-create loc wpos) is called when one requires a WPos to be created
;; starting with the beginning of the list - calling find-wpos-al when the WPos
;; terminates. Thus producing a (listof WPos)
;; wpos-create: (listof (list Char Nat)) WPos -> (listof WPos)

;; (find-wpos loc row) Given a (listof Char) and a row #, it creates an
;; association list for the characters with 0 as the first index value. It then
;; passes the list onto find-wpos-al and filters out any values in the WPos
;; that are not of length > 1 from the result of find-wpos-al
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))
(check-expect (find-wpos (string->list ".#.##.###.....####") 1)
              (list (make-wpos 1 3 true 2)
                    (make-wpos 1 6 true 3)
                    (make-wpos 1 14 true 4)))
(check-expect (find-wpos (string->list "") 2) empty)
(check-expect (find-wpos (string->list "...") 3) empty)
(check-expect (find-wpos (string->list "#..#") 4) empty)
(check-expect (find-wpos (string->list "#") 5) empty)
(check-expect (find-wpos (string->list "##") 6)
              (list (make-wpos 6 0 true 2)))


(define (find-wpos loc row)
  (local [(define (find-wpos-al loc) 
            (cond
              [(empty? loc) empty]
              [(char=? #\# (first (first loc)))
               (wpos-create loc (make-wpos row (second (first loc)) true 0))]
              [else (find-wpos-al (rest loc))]))
          (define (wpos-create loc wpos)
            (cond
              [(empty? loc) (list wpos)]
              [(char=? #\# (first (first loc)))
               (wpos-create (rest loc)
                            (make-wpos row (wpos-col wpos) true
                                       (add1 (wpos-len wpos))))]
              [else (cons wpos (find-wpos-al (rest loc)))]))]
    (filter (lambda (wpos) (> (wpos-len wpos) 1))
            (find-wpos-al (al-assign loc)))))

         
;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
;; the order does not matter: here is an example
;; that uses lists-equiv?
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))　true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)


;; Local helpers for initial-state:

;; (each-row g) takes a Grid and produces a (listof WPos) of each row in
;; the Grid
;; each-row: Grid -> (listof WPos)

;; (each-col g) takes a Grid, transposes it, and finds the (listof WPos) for
;; the col of the Grid
;; each-col: Grid -> (listof WPos)

;; (initial-state puzzle) consumes a Puzzle and produces the initial state for
;; the search to start from
;; initial-state: Puzzle -> State
;; Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
(check-expect (initial-state puzz04)
              (make-state
               (list
                (list #\# #\# #\# #\# #\# #\# #\#)
                (list #\# #\. #\# #\. #\# #\. #\#)
                (list #\# #\# #\# #\# #\# #\# #\#)
                (list #\# #\. #\# #\. #\# #\. #\#)
                (list #\# #\# #\# #\# #\# #\# #\#)
                (list #\# #\. #\# #\. #\# #\. #\#)
                (list #\# #\# #\# #\# #\# #\# #\#))
               (list
                (make-wpos 0 0 true 7) (make-wpos 2 0 true 7)
                (make-wpos 4 0 true 7) (make-wpos 6 0 true 7)
                (make-wpos 0 0 false 7) (make-wpos 0 2 false 7)
                (make-wpos 0 4 false 7) (make-wpos 0 6 false 7))
               (list
                "AVERAGE" "CASSIUS" "CUSTARD" "DESSERT"
                "IMITATE" "SECTION" "SUCCESS" "SUNBELT")))
(check-expect (initial-state (list empty empty)) (make-state empty empty empty))
(check-expect (initial-state (list empty (list "yes")))
              (make-state empty empty (list "yes")))
(check-expect (initial-state (list (list "###") empty))
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3)) empty))


(define (initial-state puzzle)
  (local [(define grid (map (lambda (x) (string->list x)) (first puzzle)))
          (define (each-row g)
            (foldr (lambda (x y)
                     (append (find-wpos (first x) (second x)) y)) empty
                   (al-assign g)))
          (define (each-col g)
            (cond
              [(empty? (each-row (transpose g))) empty]
              [else (map flip (each-row (transpose g)))]))]
    (make-state grid (append (each-row grid)
                             (each-col grid)) (second puzzle))))

;; local helpers for extract-wpos:

;; (filter-out-row g row) consumes a Grid and a row # and produces only that
;; specific row
;; filter-out-row: Grid Nat -> (listof Char)

;; (horiz-string row) takes a row which is a (listof Char) and produces a
;; (listof Char) according to the intially given WPos in extract-wpos,
;; given that the row in question was originally horizontal
;; horiz-string: (listof Char) -> (listof Char)

;; (vert-string row) takes a row which is a (listof Char) and produces a
;; (listof Char) according to the intially given WPos in extract-wpos,
;; given that the row in question was originally vertical
;; vert-string: (listof Char) -> (listof Char)

;; (extract-wpos g wp) consumes a Grid and WPos and produces the (listof Char)
;; corresponding to that position
;; extract-wpos: Grid WPos -> (listof Char)
;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))
(check-expect (extract-wpos empty (make-wpos 0 2 false 2)) empty)
 

(define (extract-wpos g wp) 
  (local
    [(define (filter-out-row g row)
       (first (first (filter (lambda (x) (= (second x) row)) (al-assign g)))))
     (define (horiz-string row)
       (map (lambda (x) (first x))
            (filter (lambda (x) (and
                            (<= (wpos-col wp) (second x))
                            (<= (second x) (sub1 (+ (wpos-len wp)
                                                    (wpos-col wp))))))
                    (al-assign row))))
     (define (vert-string row)
       (map (lambda (x) (first x))
            (filter (lambda (x) (and
                            (<= (wpos-row wp) (second x))
                            (<= (second x) (sub1 (+ (wpos-len wp)
                                                    (wpos-row wp))))))
                            (al-assign row))))]
    (cond
      [(empty? g) empty]
      [(wpos-horiz? wp)
       (horiz-string (filter-out-row g (wpos-row wp)))]
      [else (vert-string (filter-out-row (transpose g) (wpos-col wp)))]))) 

;; Tests:

;; local helpers for replace-wpos

;; (find-row g row-num col-num) consumes a Grid row-number and col-number and
;; produces a new Grid whilst passing on the row with the corresponding row
;; number to the find-col
;; requires: that the Grid has already been processed by al-assign and has index
;; find-row: (listof (list (listof Char) Nat)) Nat Nat -> Grid

;; (find-col row col-num loc) consumes a row, a col-num and a (listof Char) and
;; produces the a new row - keeping the original values except for the ones
;; that need to be replaced by the loc
;; find-col: (listof Char) Nat (listof Char) -> (listof Char)

;; (replacer row loc) consumes a row and and (listof Char) and replaces the
;; first n amount of characters in the row, where n is the length of the loc
;; replacer: (listof Char) (listof Char) -> (listof Char)

;; (replace-wpos g wp loc) Consumes a Grid WPos and (listof Char) and produces
;; a new Grid with the loc in the position of the WPos
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))
(check-expect (replace-wpos empty (make-wpos 0 2 true 2) '(#\J #\K)) empty)


(define (replace-wpos g wp loc)
  (local
    [(define (find-row g row-num col-num) ; assuming index
       (cond
         [(empty? g) empty]
         [(= (second (first g)) row-num)
          (cons (find-col (al-assign (first (first g))) col-num loc)
                (find-row (rest g) row-num col-num))]
         [else (cons (first (first g)) (find-row (rest g) row-num col-num))]))
     (define (find-col row col-num loc)
       (cond
         [(empty? row) empty]
         [(= (second (first row)) col-num)
          (replacer (map (lambda (x) (first x)) row) loc)]
         [else (cons (first (first row)) (find-col (rest row) col-num loc))]))
     (define (replacer row loc) ; assuming removed index
       (cond
         [(empty? loc) row]
         [else (cons (first loc) (replacer (rest row) (rest loc)))]))]
    (cond
      [(empty? g) empty]
      [(wpos-horiz? wp) (find-row (al-assign g) (wpos-row wp) (wpos-col wp))]
      [else (transpose (find-row (al-assign (transpose g))
                                 (wpos-col wp) (wpos-row wp)))])))

;; Tests:


;; (fit? word cells)
;; fit? (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)
(check-expect (fit? empty empty) true)
(check-expect (fit? (string->list "STARWARS") (string->list "###")) false)


(define (fit? word cells)
  (cond
    [(empty? word) true]
    [(not (= (length word) (length cells))) false]
    [(char=? #\# (first cells)) (fit? (rest word) (rest cells))]
    [(char=? (first cells) (first word)) (fit? (rest word) (rest cells))]
    [else false]))

;; Tests:

;; local helper functions:

;; (count-extract wp) takes a WPos and counts the amount of letters that have
;; already been filled in for that WPos in the Grid
;; count-extact: WPos -> Nat

;; (find-optimal-wp wp-list) compares all of the WPos that are available and
;; produces the one with the most already filled in spaces
;; find-optimal-wp: (listof WPos) -> WPos

;; (replace-words words) takes the words parameter of the original state and
;; produces a list of only the ones that can fit inside of the optimal WPos
;; replace-words: (listof String) -> (listof String

;; (stater wtr) takes a (listof String) which are the words to be replaced
;; inside the optimal WPos and r

;; (neighbours s) consumes a State and produces a (listof State) where each
;; of the States are valid neighbour States that have one additional word
;; placed in the puzzle
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(define (neighbours s)
  (local
    [(define (count-extract wp)
       (length (filter (lambda (x) (not (char=? #\# x)))
                       (extract-wpos (state-grid s) wp))))
     (define (find-optimal-wp wp-list)
       (foldr (lambda (x y)
                (cond
                  [(>= (count-extract x) (count-extract y)) x]
                  [else y])) (make-wpos 0 0 true 0) wp-list))
     (define optimal-wpos (find-optimal-wp (state-positions s)))
     (define (replace-words words)
       (filter (lambda (x)
                 (fit? (string->list x)
                       (extract-wpos (state-grid s) optimal-wpos)))
               (state-words s)))
     (define (stater wtr)
       (map (lambda (a) (make-state (replace-wpos (state-grid s) optimal-wpos
                                                  (string->list a))
                                    (filter (lambda (x)
                                              (not (equal? optimal-wpos x)))
                                            (state-positions s))
                                    (remove a (state-words s)))) wtr))]
    (cond
      [(empty? (state-positions s)) empty]
      [else (stater (replace-words (state-words s)))])))

;; Tests:
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

(check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

;(disp (criss-cross (read-puzzle "puzzle04.txt")))

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

