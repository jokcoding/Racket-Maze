#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require typed/racket/date)

(require typed/test-engine/racket-tests)

;; DATA DEFINITIONS
(define-struct Tile
  ([tile-type : (U 'origin 'destination 'walkable 'unwalkable)]
   ))

(define-struct TileFormat
  ([tile-size : Integer]
   [walkable-color : Image-Color]
   [unwalkable-color : Image-Color]
   [origin-color : Image-Color]
   [destination-color : Image-Color]))

(define-struct Maze
  ([tileformat : TileFormat]
   [tiles : (Listof Tile)]
   [rows : Integer]))

;;ROW STARTS AT ONE JACK HOPPUS!!! DONT FORGET :)
(define-struct SmartTile
  ([tilevalue : Tile]
   [row : Integer]))

;; TESTING DEFINITIONS

(define fmt0
  (TileFormat 30 'lightblue 'black 'red 'green))

(define fmt1
  (TileFormat 50 'silver 'red 'gold 'blue))

(define otile
  (Tile 'origin))

(define dtile
  (Tile 'destination))

(define wtile
  (Tile 'walkable))

(define utile
  (Tile 'unwalkable))

(define tinymazelist
  (list otile wtile utile utile
        utile wtile utile utile
        utile wtile wtile utile
        utile utile wtile dtile))

(define tinymaze
  (Maze fmt0
        tinymazelist
        4))

(define 3x5maze
  (Maze fmt1
        (list wtile dtile utile
              wtile utile utile
              wtile wtile wtile
              utile utile wtile
              utile otile wtile)
        5))


 


;; FUNCTIONS
(: draw-tile : Tile TileFormat -> Image)
;; takes a tile, tileformat and draws it :)
(define (draw-tile tile fmt)
  (match fmt
    [(TileFormat size w u o d)
     (match tile
       [(Tile 'origin)
        (overlay
         (circle (/ size 4) "solid" "white")
         (circle (/ size 3) "solid" "black")
         (rectangle size size "solid" o))]
       [(Tile 'destination)
        (overlay
         (star (/ size 3) "solid" "gold")
         (rectangle size size "solid" d))]
       [(Tile 'walkable)
        (rectangle size size "solid" w)]
       [(Tile 'unwalkable)
        (rectangle size size "solid" u)])]))

(draw-tile otile fmt0)
(draw-tile dtile fmt0)
(draw-tile wtile fmt0)
(draw-tile utile fmt0)

(: position-in-list : All (A) (Listof A) A -> Integer)
;;takes a list, an element in that list, and returns the position in the list
;; STARTS W 1!!!!!!!
;; assumes the element in in such list.
(define (position-in-list original-list element)
  (match original-list
    ['() 0]
    [(cons head tail)
     (cond
       [(equal? head element)
        (+ 1 (position-in-list '() element))]
       [else
        (+ 1 (position-in-list tail element))])]))

(check-expect (position-in-list (list "A" "B" "C" "D" "E" "F" "G")
                                "A") 1)
(check-expect (position-in-list (list "A" "B" "C" "D" "E" "F" "G")
                                "G") 7)
(check-expect (position-in-list (list 1 4 3 2 9 7 100)
                                3) 3)

(: make-smart-list-position-based : (Listof Tile) Integer
   -> (Listof SmartTile))
;;a smartlist is basically a list that returns a list of SmartTiles,
;;which has the position starting at 1, and you have to input your first
;;index position (which i choose 1 cuz my brain works better like that)
;; *shrug*
(define (make-smart-list-position-based original-list index)
  (match original-list
    ['() '()]
    [(cons head tail) (append (list (SmartTile head index))
                              (make-smart-list-position-based
                               tail (+ 1 index)))]))

(check-expect (make-smart-list-position-based
               (list utile utile wtile wtile otile otile) 1)
              (list (SmartTile utile 1)
                    (SmartTile utile 2)
                    (SmartTile wtile 3)
                    (SmartTile wtile 4)
                    (SmartTile otile 5)
                    (SmartTile otile 6)))

(: smrt-position->row : (Listof SmartTile) Integer -> (Listof SmartTile))
;;takes in a position based list of smarttiles, rows desired and outputs it
;;instead of position based, (ex. 1 2 3 4 5), row-based!
;;with ex, if 2 rows, it would be (1 1 2 2 3), bc first two elements in first
;;row... kinda hard to explain but should work out correctly in the end
(define (smrt-position->row original rows)
  (match original
    ['() '()]
    [(cons head tail)
     (match head
       [(SmartTile element position)
        (append (list (SmartTile element
                                 (ceiling (/ position rows))))
                (smrt-position->row tail rows))])]))

;; i tested this in the command line with the command
;; (smrt-position->row (make-smart-list-position-based tinymazelist 1) 4)
;; and it worked. I don't want to put a check expect below as it is like
;; 20+ lines long of a check expect.
    
(: isolate-row-list : (Listof SmartTile) Integer -> (Listof Tile))
;;takes a row that you want, and filters only the SmartTiles
;;that are in that row.
(define (isolate-row-list original rows)
  (match original
    ['() '()]
    [(cons head tail)
     (match head
       [(SmartTile element row-its-in)
        (cond
          [(= row-its-in rows)
           (append (list element) (isolate-row-list tail rows))]
          [else
           (isolate-row-list tail rows)])])]))

(check-expect (isolate-row-list
               (smrt-position->row (make-smart-list-position-based
                                    tinymazelist 1) 4)
               2)
              (list utile wtile utile utile))

;;since this check expect works, its ok that i didn't check the fxn
;;two fxns ago, as it involves it in this working check expect :)

(: draw-maze-row : (Listof Tile) TileFormat -> Image)
;;draws one row of the maze
(define (draw-maze-row tilerow fmt)
  (match tilerow
    ['() (rectangle 0 0 "solid" "white")]
    [(cons head tail)
     (beside (draw-tile head fmt)
             (draw-maze-row tail fmt))]))

(draw-maze-row (isolate-row-list
               (smrt-position->row (make-smart-list-position-based
                                    tinymazelist 1) 4)
               2) fmt0)

(: draw-maze : Maze Integer -> Image)
;; draws a maze. has to input also how many rows in the original maze.
(define (draw-maze maze original-rows)
  (match maze
    [(Maze fmt tiles rows)
     (match rows
       [0 (rectangle 0 0 "solid" "white")]
       [numrows (above
                 (draw-maze (Maze fmt tiles (- numrows 1)) original-rows)
                 (draw-maze-row (isolate-row-list
                                   (smrt-position->row
                                    (make-smart-list-position-based
                                     tiles 1) (floor
                                               (/ (length tiles)
                                                  original-rows)))
                                   numrows) fmt))])]))


(draw-maze tinymaze (Maze-rows tinymaze))
(draw-maze 3x5maze (Maze-rows 3x5maze))

(: create-random-integers : Integer Integer -> (Listof Integer))
;;takes in a the desired list length, then how many random options you want to
;;sort thru (for our case it's 4 - walk, unwalk, dest, origin)
;;and makes a random list of those parameters
(define (create-random-integers listlength randoms)
  (match listlength
    [0 '()]
    [rest
     (append
      (list (random randoms))
      (create-random-integers (- listlength 1) randoms))]))

(create-random-integers 10 5)
(create-random-integers 20 10)

(: list-of-occurrences : (Listof Integer) Integer Integer -> (Listof Integer))
;;takes an integer, and creates a list of their occurrences in the original list
;; also needs length of original list untouched
(define (list-of-occurrences original int untouched)
  (match original
    ['() '()]
    [(cons head tail)
     (cond
       [(= head int)
        (append (list (- untouched (length tail)))
                (list-of-occurrences tail int untouched))]
       [else
        (list-of-occurrences tail int untouched)])]))

(check-expect (list-of-occurrences '(10 0 10 0 0 0 10 10 10 10) 10
                                   (length '(10 0 10 0 0 0 10 10 10 10)))
              '(1 3 7 8 9 10))

;;(: remove-randomly : (Listof Integer) Integer -> (Listof Integer))
;; takes a list of integers, one integer inside of it, and randomly removes it


;(: create-random-tilelist : Integer Integer -> (Listof Tile))
;;;takes rows, columns and creates a tilelist of those dimensions.
;;;DOESNT CARE ABOUT IF it IS CORRECT
;;; i.e. solvable (clear path from origin to destination)
;;;      or if there exists more than one origin or destination
;;(define (create-random-maze rows columns)
 
  


(test)