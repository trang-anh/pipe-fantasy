;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pipe-fantasy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: A pipe with openings in the given directions. A  #t for one of top, bottom, left, right indicates an opening in that direction.

;; Examples

;; Corner Pipes
(define PIPE-TL (make-pipe #t #f #t #f))
(define PIPE-TR (make-pipe #t #f #f #t))
(define PIPE-BL (make-pipe #f #t #t #f))
(define PIPE-BR (make-pipe #f #t #f #t))

;; Straight Pipes
(define PIPE-TB (make-pipe #t #t #f #f))
(define PIPE-LR (make-pipe #f #f #t #t))

;; 4 Way Pipe - Cross pipe
(define PIPE-C (make-pipe #t #t #t #t))

;; List of all Pipes
(define ALL-PIPES (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TB PIPE-LR PIPE-C))

;; Template:
#;(define (pipe-temp p)
  (... (pipe-top p) ...
        (pipe-bot p) ...
        (pipe-left p) ...
        (pipe-right p) ...))

;--------------------------------------------

; Task 3:
; During the game, the player places pipes onto square tiles on the board. Design a function with the following signature and purpose statement:
; You do not need to write check-expects for pipe->image.

(require 2htdp/image)

;-------------------------

;; align :
;; helper function for pipe->image, helps aligning everything into the middle.

(define (align pipe left-or-right top-or-bottom tile-side-length pipe-width)
  (overlay/align left-or-right top-or-bottom
                 (square (/ pipe-width 1.6) "solid" "gray")
                 (square pipe-width "solid" "black")
                 (square tile-side-length "solid" "gray")))

(check-expect (align PIPE-TL "left" "top" 200 100)
              (overlay/align "left" "top" 
                             (square (/ 100 1.6) "solid" "gray")
                             (square 100 "solid" "black")
                             (square 200 "solid" "gray")))

;; pipe->image: Pipe Integer Integer -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length

(define (pipe->image pipe tile-side-length pipe-width)
  (cond
    ; PIPE-TL 
    [(and (pipe-top pipe) (pipe-left pipe))
     (overlay/align "middle" "middle"
                    (align pipe "left" "top" tile-side-length pipe-width)
                    (square tile-side-length "solid" "gray"))]
#;     (overlay/align "left" "top"
                    (square (/ pipe-width 1.6) "solid" "gray")
                    (square pipe-width "solid" "black")
                    (square tile-side-length "solid" "gray"))
    ; PIPE-TR
    [(and (false? (pipe-bot pipe)) (false? (pipe-left pipe)))
     (overlay/align "right" "top"
                    (square (/ pipe-width 1.6) "solid" "gray")
                    (square pipe-width "solid" "black")
                    (square tile-side-length "solid" "gray"))]
    ; PIPE-BR
    [(and (false? (pipe-top pipe)) (false? (pipe-left pipe)))
     (overlay/align "right" "bottom"
                    (square (/ pipe-width 1.6) "solid" "gray")
                    (square pipe-width "solid" "black")
                    (square tile-side-length "solid" "gray"))]
    ; PIPE-BL
    [(and (false? (pipe-top pipe)) (false? (pipe-right pipe)))
     (overlay/align "left" "bottom" 
                    (square (/ pipe-width 1.6) "solid" "gray")
                    (square pipe-width "solid" "black")
                    (square tile-side-length "solid" "gray"))]
    ; PIPE-TB 
    [(and (false? (pipe-left pipe)) (false? (pipe-right pipe)))
     (overlay/align "middle" "middle"
                    (rectangle pipe-width tile-side-length "solid" "black")
                    (square tile-side-length "solid" "gray"))]
    ; PIPE-LR 
    [(and (false? (pipe-top pipe)) (false? (pipe-bot pipe)))
     (overlay/align "middle" "middle"
                    (rectangle tile-side-length pipe-width "solid" "black")
                    (square tile-side-length "solid" "gray"))]
    ; PIPE-C
    [(and (pipe-bot pipe) (pipe-top pipe) (pipe-left pipe) (pipe-right pipe)) 
     (overlay/align "middle" "middle"
                    (rectangle pipe-width tile-side-length "solid" "black")
                    (rectangle tile-side-length pipe-width "solid" "black")
                    (square tile-side-length "solid" "gray"))]))

;-----------------------------------------

; Part 2: Designing the Grid and Placing Pipes

; Task 4: Complete a data design called Grid that represents a grid. You should construct several examples of varying sizes and with different pipes places on them.

(define-struct pipe-with-coord (pipe x y))
; A pipe-with-coord is a (make-pipe-with-coord Pipe Integer Integer)
; - pipe is the type of pipe
; - x is the x-coordinate of the pipe
; - y is the y-coordinate of the pipe
; Intepretation: represents a pipe by its type and its coordinates on the grid

(define PIPE-WITH-COORD1 (make-pipe-with-coord PIPE-TL 0 2))
(define PIPE-WITH-COORD2 (make-pipe-with-coord PIPE-TB 2 0))
(define PIPE-WITH-COORD3 (make-pipe-with-coord PIPE-BR 5 3))

(define (pwc-temp p)
  (... pipe-with-coord-pipe p ...
       pipe-with-coord-x p ...
       pipe-with-coord-y p ...))


(define-struct Grid (size list-of-pipes-with-coord))
; A Grid is a (make-Grid Integer List-of pipe-with-coord)
; - size is the size (n x n) of the grid
; - list-of-pipes-with-coord is the list representing placed pipes on the grid
; Intepretation: represents a grid with its size and the list of placed pipes on the grid.

(define STARTING-GRID (make-Grid 7 (list)))
(define GRID-1 (make-Grid 5 (list PIPE-WITH-COORD1)))
(define GRID-2 (make-Grid 10 (list PIPE-WITH-COORD1 PIPE-WITH-COORD2 PIPE-WITH-COORD3)))

(define (Grid-temp g)
  (... Grid-size g
       Grid-list-of-pipes-with-coord g ...))

;--------------------------------------------

; Task 6: Complete the following function designs (do not modify the signatures):

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.

;; so place-pipe takes in a grid (which is a struct with grid-size and the list of pipes placed in that grid), a pipe, a row and a column.
;; the function essentially appends (sticks/assigns) the given pipe to that column and row by a make-pipe-with-coords struct into the grid-list-of-pipes, basically adding a new pipe with the given row and column into the list of pipes in the grid.

(check-expect (place-pipe STARTING-GRID PIPE-TL 2 3)
              (list
               (make-pipe-with-coord PIPE-TL 2 3)))

(check-expect (place-pipe GRID-1 PIPE-TB 0 5)
              (list
               (make-pipe-with-coord PIPE-TL 0 2)
               (make-pipe-with-coord PIPE-TB 0 5)))

(check-expect (place-pipe GRID-2 PIPE-C 0 0)
              (list
               (make-pipe-with-coord PIPE-TL 0 2)
               (make-pipe-with-coord PIPE-TB 2 0)
               (make-pipe-with-coord PIPE-BR 5 3)
               (make-pipe-with-coord PIPE-C 0 0)))

(define (place-pipe grid pipe row col)
  (append (Grid-list-of-pipes-with-coord grid)
          (list (make-pipe-with-coord pipe row col))))

;-----------------------------

;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.

;; pipe-at function goes through the entire list-of-pipes in the grid and checks if the given row and column has a pipe at that location yet. If there is a pipe at that given location, the function returns that pipe and if there is no pipe at the location, it returns a false.

(check-expect (pipe-at STARTING-GRID 0 0) #f)
(check-expect (pipe-at GRID-1 0 2) (make-pipe #true #false #true #false))
(check-expect (pipe-at GRID-1 0 2) PIPE-TL)
(check-expect (pipe-at '() 0 0) #f)
(check-expect (pipe-at GRID-2 5 3) PIPE-BR)

(define (pipe-at grid row col)
  (cond
    [(empty? grid) #f]
    [(cons? (Grid-list-of-pipes-with-coord grid))
     (if
      (and
       (= (pipe-with-coord-x (first (Grid-list-of-pipes-with-coord grid))) row)
       (= (pipe-with-coord-y (first (Grid-list-of-pipes-with-coord grid))) col))

      (pipe-with-coord-pipe (first (Grid-list-of-pipes-with-coord grid)))
               
      (pipe-at (make-Grid (Grid-size grid) (rest (Grid-list-of-pipes-with-coord grid))) row col))]
    [else #false]))

;-------------------------------------

; Task 7: Complete the following function design. Do not modify its signature, and you do not need to write check-expects for it:

;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.

(define (grid->image grid tile-side-length pipe-width)
  ...)

       


               


#;  (append (Grid-list-of-pipes-with-coord grid) (list (make-pipe-with-coord pipe row col)))
#;  (cons (pipe-with-coord grid row col) (first grid)
                  (pipe-at (make-Grid Grid-size Grid-list-of-pipes-with-coord grid) ))















