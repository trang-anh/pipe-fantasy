;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |HW11 FINAL|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for
;; one of top, bot, left, right indicates an opening in that direction.
(define (pipe-template sample-pipe)
  (... (pipe-top sample-pipe) ...
       (pipe-bot sample-pipe) ...
       (pipe-left sample-pipe) ...
       (pipe-right sample-pipe) ...))

(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-C (make-pipe #true #true #true #true))
(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-L (make-pipe #false #false #true #false))
(define PIPE-R (make-pipe #false #false #false #true))
(define PIPE-T (make-pipe #true #false #false #false))
(define PIPE-B (make-pipe #false #true #false #false))

(define ALL-PIPES (list PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-C PIPE-TB PIPE-LR
                        PIPE-L PIPE-R PIPE-T PIPE-B))

;; Signature: green-or-black: Boolean -> string
;; Purpose - returns green if boolean input is true else false(either goo or no goo)
(define (green-or-black bool)
  (if bool "green" "black"))
(check-expect (green-or-black #false) "black")
(check-expect (green-or-black #true) "green")


;; Purpose - creates an image given a pipe
;; Signature - pipe->image-helper: int int string string int -> image
(define (pipe->image-helper tile-side-length pipe-width lor bot divisor filled?)
  (overlay/align lor "middle" (rectangle (/ tile-side-length divisor) pipe-width "solid"
                                         (green-or-black filled?))
                 (overlay/align "middle" bot (rectangle  pipe-width
                                                         (/ tile-side-length divisor) "solid" (green-or-black filled?))
                                (overlay/align "middle" "middle"       (square pipe-width "solid" (green-or-black filled?))
                                               (square tile-side-length "solid" "gray")))))

(check-expect (pipe->image-helper 100 20 "left" "top" 2 #false)
              (overlay/align "left" "middle" (rectangle 50 20 "solid" "black")
                             (overlay/align "middle" "top"  (rectangle 20 50 "solid" "black")
                                            (overlay/align "middle" "middle" (square 20 "solid" "black")
                                                           (square 100 "solid" "gray")))))

(check-expect (pipe->image-helper 200 50 "right" "bottom" 2 #false)
              (overlay/align "right" "middle" (rectangle 100 50 "solid" "black")
                             (overlay/align "middle" "bottom" (rectangle 50 100 "solid" "black")
                                            (overlay/align "middle" "middle" (square 50 "solid" "black")
                                                           (square 200 "solid" "gray")))))


;; pipe->image: Pipe Integer Integer Boolean -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.
;; If filled? then draw the pipe with goo.
(define (pipe->image pipe tile-side-length pipe-width filled? dir)
  (cond
    [(and (false? (pipe-top pipe)) (false? (pipe-bot pipe)) (false? (pipe-right pipe)))
     (overlay/align "left" "middle"
                    (rectangle (/ tile-side-length 2) pipe-width "solid" "green")
                    (square tile-side-length "solid" "gray"))]
    [(and (false? (pipe-top pipe)) (false? (pipe-bot pipe)) (false? (pipe-left pipe)))
     (overlay/align "right" "middle"
                    (rectangle (/ tile-side-length 2) pipe-width "solid" "green")
                    (square tile-side-length "solid" "gray"))]
    [(and (false? (pipe-top pipe)) (false? (pipe-left pipe)) (false? (pipe-right pipe)))
     (overlay/align "middle" "bottom"
                    (rectangle pipe-width (/ tile-side-length 2) "solid" "green")
                    (square tile-side-length "solid" "gray"))]
    [(and (false? (pipe-bot pipe)) (false? (pipe-left pipe)) (false? (pipe-right pipe)))
     (overlay/align "middle" "top"
                    (rectangle pipe-width (/ tile-side-length 2) "solid" "green")
                    (square tile-side-length "solid" "gray"))]
    [(and (false? (pipe-bot pipe)) (false? (pipe-right pipe)))
     (pipe->image-helper tile-side-length pipe-width "left" "top" 2 filled?)]
    [(and (false? (pipe-bot pipe)) (false? (pipe-left pipe)))
     (pipe->image-helper tile-side-length pipe-width "right" "top" 2 filled?)]
    [(and (false? (pipe-top pipe)) (false?(pipe-right pipe)))
     (pipe->image-helper tile-side-length pipe-width "left" "bottom" 2 filled?)]
    [(and (false? (pipe-top pipe)) (false? (pipe-left pipe)))
     (pipe->image-helper tile-side-length pipe-width "right" "bottom" 2 filled?)]
    [(and (pipe-bot pipe) (pipe-left pipe) (not filled?))
     (pipe->image-helper tile-side-length pipe-width "middle" "middle" 1 filled?)]
    [(and (pipe-bot pipe) (pipe-left pipe) (or (string-ci=? "left" dir) (string-ci=? "right" dir)))
     (overlay/align "middle" "middle"
                    (rectangle tile-side-length pipe-width "solid" "green")
                    (overlay/align "middle" "middle"
                                   (rectangle  pipe-width tile-side-length "solid" "black")
                                   (overlay/align "middle" "middle"
                                                  (square pipe-width "solid" "black")
                                                  (square tile-side-length "solid" "gray"))))]
    [(and (pipe-bot pipe) (pipe-left pipe) (or (string-ci=? "top" dir) (string-ci=? "bottom" dir)))
     (overlay/align "middle" "middle"
                    (rectangle pipe-width tile-side-length "solid" "green")
                    (overlay/align "middle" "middle"
                                   (rectangle tile-side-length pipe-width "solid" "black")
                                   (overlay/align "middle" "middle"
                                                  (square pipe-width "solid" "black")
                                                  (square tile-side-length "solid" "gray"))))]
    [(and (pipe-bot pipe) (pipe-left pipe) (string-ci=? "all" dir))
     (pipe->image-helper tile-side-length pipe-width "middle" "middle" 1 #true)]
    [(and (false? (pipe-left pipe)) (false? (pipe-right pipe)))
     (overlay/align "middle" "middle"
                    (rectangle pipe-width tile-side-length "solid" (green-or-black filled?))
                    (square tile-side-length "solid" "gray"))]
    [(and (false? (pipe-top pipe)) (false? (pipe-bot pipe)))
     (overlay/align "middle" "middle"
                    (rectangle tile-side-length pipe-width "solid" (green-or-black filled?))
                    (square tile-side-length "solid" "gray"))]))


(define-struct pipe-with-coordinates (pipe x y))
;; A pipe-with-coordinates is a (make-pipe-with-coordinates pipe integer integer)
;; pipe is the type of pipe
;; x is the x coordinate of its position on the grid
;; y is the y coordinate of its position on the grid
;; Interpretation: A pipe with its coordinates on the grid

(define (pipe-with-coordinates-template sample-pipe)
  (...
   (pipe-template (pipe-with-coordinates-pipe sample-pipe)) ...
   (pipe-with-coordinates-x sample-pipe) ...
   (pipe-with-coordinates-y sample-pipe) ... ))

(define PIPE-EX1 (make-pipe-with-coordinates PIPE-TB 2 2))
(define PIPE-EX2 (make-pipe-with-coordinates PIPE-TL 2 2))
(define PIPE-EX3 (make-pipe-with-coordinates PIPE-TR 2 1))
(define PIPE-EX4 (make-pipe-with-coordinates PIPE-BL 1 2))
(define PIPE-EX5 (make-pipe-with-coordinates PIPE-BR 0 0))
(define PIPE-EX6 (make-pipe-with-coordinates PIPE-C 3 1))
(define PIPE-EX7 (make-pipe-with-coordinates PIPE-LR 0 0))
(define PIPE-EX8 (make-pipe-with-coordinates PIPE-B 2 1))
(define PIPE-EX9 (make-pipe-with-coordinates PIPE-L 3 3))
(define PIPE-EX10 (make-pipe-with-coordinates PIPE-T 3 3))
(define PIPE-EX11 (make-pipe-with-coordinates PIPE-R 1 2))
(define HW11PIPE-1 (make-pipe-with-coordinates PIPE-R 1 1))
(define HW11PIPE-2 (make-pipe-with-coordinates PIPE-BL 2 1))
(define HW11PIPE-3 (make-pipe-with-coordinates PIPE-C 2 2))
(define HW11PIPE-4 (make-pipe-with-coordinates PIPE-TB 2 3))
(define HW11PIPE-5 (make-pipe-with-coordinates PIPE-TL 2 4))
(define HW11PIPE-6 (make-pipe-with-coordinates PIPE-TR 1 4))
(define HW11PIPE-7 (make-pipe-with-coordinates PIPE-C 1 3))
(define HW11PIPE-8 (make-pipe-with-coordinates PIPE-BR 1 2))
(define HW11PIPE-9 (make-pipe-with-coordinates PIPE-LR 3 2))
(define HW11PIPE-10 (make-pipe-with-coordinates PIPE-TL 4 2))



(define (list-template-for-pipes lop)

  (cond [(empty? lop) ...]
        [(cons? lop) (... (pipe-template (first lop))
                          (list-template-for-pipes (rest lop) ...))]))




(define-struct Grid (grid-size list-of-pipes))
(define (Grid-template sample-grid)
  (...
   (Grid-grid-size sample-grid) ...
   (list-template-for-pipes (Grid-list-of-pipes sample-grid)) ...
   ... ))


;; A Grid is a (make-grid integer [List-of pipe-with-coordinates])
;; grid-size is the length and width of the grid
;; list-of-pipes is the list of all pipes and their x and y coordinates on the grid
;; Interpretation: Meant to represent a grid with pipes and their locations

(define STARTING-GRID (make-Grid 7 (list )))
(define GRID-WITH-TBBLBR  (make-Grid 5 (list PIPE-EX1 PIPE-EX4 PIPE-EX5)))
(define GRID-WITH-CTLTR (make-Grid 5 (list PIPE-EX6 PIPE-EX2 PIPE-EX3 PIPE-EX7)))
(define GOO-GRID (make-Grid 5 (list PIPE-EX8 PIPE-EX2)))
(define GOO-GRID2 (make-Grid 7 (list PIPE-EX9 PIPE-EX1 PIPE-EX3 PIPE-EX7 PIPE-EX5 PIPE-EX2)))
(define HW11GRID (make-Grid 6 (list HW11PIPE-1 HW11PIPE-2 HW11PIPE-3 HW11PIPE-4 HW11PIPE-5
                                    HW11PIPE-6 HW11PIPE-7 HW11PIPE-8 HW11PIPE-9 HW11PIPE-10)))

;; created 4 constants to represent the 4 directions the goo could flow in.
(define LEFT "left")
(define RIGHT "right")
(define TOP "top")
(define BOTTOM "bottom")

(define-struct GooFlow (goo-filled-pipes direction))
;; A GooFlow is a (make-GooFlow [List-of pipes-with-coordinates] string)
;; where goo-filled-pipes is all the pipes that are filled with goo. Another way
;; to think about it is that the goo-filled-pipes list will be a subset of the list of pipes-with-coordinates
;; in the struct Grid. dierction is the direction of the GooFlow represented by a string.
;; Interpretation: Represents the path of the Goo with its current direction.

(define EX-GOO (make-GooFlow (list PIPE-EX8) BOTTOM))
(define EX-GOO2 (make-GooFlow (list PIPE-EX9) LEFT))
(define EX-GOO3 (make-GooFlow (list PIPE-EX10) TOP))
(define EX-GOO4 (make-GooFlow (list PIPE-EX11) RIGHT))
(define HW11GOO (make-GooFlow (list HW11PIPE-1) RIGHT))

(define (GooFlowTemplate ex-goo-flow)
  (...
   (list-template-for-pipes (GooFlow-goo-filled-pipes ...))
   (GooFlow-direction ...)))
                       
;; Signature - xy-check: GooFlow -> posn
;; Purpose - Returns a posn(x, y) given a goo-flow consisting of the starting pipe and its location
;; The posn is later used to check if there is a pipe at that position in other functions.
(define (xy-check goo-flow)
  (cond
    [(string-ci=? RIGHT (GooFlow-direction goo-flow))
     (make-posn (+ 1 (pipe-with-coordinates-x
                      (list-ref (reverse (GooFlow-goo-filled-pipes goo-flow)) 0)))
                (pipe-with-coordinates-y
                 (list-ref (reverse (GooFlow-goo-filled-pipes goo-flow)) 0)))]
    [(string-ci=? LEFT (GooFlow-direction goo-flow))
     (make-posn (- (pipe-with-coordinates-x
                    (list-ref (reverse (GooFlow-goo-filled-pipes goo-flow)) 0)) 1)
                (pipe-with-coordinates-y
                 (list-ref (reverse (GooFlow-goo-filled-pipes goo-flow)) 0)))]
    [(string-ci=? TOP (GooFlow-direction goo-flow))
     (make-posn (pipe-with-coordinates-x
                 (list-ref (reverse (GooFlow-goo-filled-pipes goo-flow)) 0))
                (- (pipe-with-coordinates-y
                    (list-ref (reverse (GooFlow-goo-filled-pipes goo-flow)) 0)) 1))]
    [(string-ci=? BOTTOM (GooFlow-direction goo-flow))
     (make-posn
      (pipe-with-coordinates-x
       (list-ref (reverse (GooFlow-goo-filled-pipes goo-flow)) 0))
      (+ 1 (pipe-with-coordinates-y
            (list-ref (reverse (GooFlow-goo-filled-pipes goo-flow)) 0))))]))

(check-expect (xy-check EX-GOO) (make-posn 2 2))
(check-expect (xy-check EX-GOO2) (make-posn 2 3))
(check-expect (xy-check EX-GOO3) (make-posn 3 2))
(check-expect (xy-check EX-GOO4) (make-posn 2 2))

;; Signature - new-goo-state: GooFlow Pipe Number Number -> GooFlow
;; Purpose - Creates a new GooFlow given the pipe found at the x and y location based on the
;; current direction of the goo flow.

(define (new-goo-state goo-flow eval-pipe x y)
  (cond
    [(and (string-ci=? RIGHT (GooFlow-direction goo-flow)) (pipe-left eval-pipe))
     (if (pipe-right eval-pipe)
         (make-GooFlow (append (GooFlow-goo-filled-pipes goo-flow)
                               (list (make-pipe-with-coordinates eval-pipe x y)))
                       (GooFlow-direction goo-flow))
         (make-GooFlow (append (GooFlow-goo-filled-pipes goo-flow)
                               (list (make-pipe-with-coordinates eval-pipe x y)))
                       (dir-maker (GooFlow-direction goo-flow) eval-pipe)))]
     
   
    [(and (string-ci=? LEFT (GooFlow-direction goo-flow)) (pipe-right eval-pipe))
     (if (pipe-left eval-pipe)
         (make-GooFlow (append (GooFlow-goo-filled-pipes goo-flow)
                               (list (make-pipe-with-coordinates eval-pipe x y)))
                       (GooFlow-direction goo-flow))
         (make-GooFlow (append (GooFlow-goo-filled-pipes goo-flow)
                               (list (make-pipe-with-coordinates eval-pipe x y)))
                       (dir-maker (GooFlow-direction goo-flow) eval-pipe)))]
   
    [(and (string-ci=? TOP (GooFlow-direction goo-flow)) (pipe-bot eval-pipe))
     (if (pipe-top eval-pipe)
         (make-GooFlow (append (GooFlow-goo-filled-pipes goo-flow)
                               (list (make-pipe-with-coordinates eval-pipe x y)))
                       (GooFlow-direction goo-flow))
         (make-GooFlow (append (GooFlow-goo-filled-pipes goo-flow)
                               (list (make-pipe-with-coordinates eval-pipe x y)))
                       (dir-maker (GooFlow-direction goo-flow) eval-pipe)))]
   
    [(and (string-ci=? BOTTOM (GooFlow-direction goo-flow)) (pipe-top eval-pipe))
     (if (pipe-bot eval-pipe)
         (make-GooFlow (append (GooFlow-goo-filled-pipes goo-flow)
                               (list (make-pipe-with-coordinates eval-pipe x y)))
                       (GooFlow-direction goo-flow))
         (make-GooFlow (append (GooFlow-goo-filled-pipes goo-flow)
                               (list (make-pipe-with-coordinates eval-pipe x y)))
                       (dir-maker (GooFlow-direction goo-flow) eval-pipe)))]

    [else goo-flow]))

(check-expect (new-goo-state EX-GOO (make-pipe #true #true #false #false) 2 2)
              (make-GooFlow (append (GooFlow-goo-filled-pipes EX-GOO)
                                    (list (make-pipe-with-coordinates
                                           (make-pipe #true #true #false #false) 2 2)))
                            (GooFlow-direction EX-GOO)))
(check-expect (new-goo-state EX-GOO2 (make-pipe #true #true #false #false) 2 3)
              EX-GOO2)

(check-expect (new-goo-state EX-GOO3 (make-pipe #true #true #false #false) 3 2)
              (make-GooFlow (append (GooFlow-goo-filled-pipes EX-GOO3)
                                    (list (make-pipe-with-coordinates
                                           (make-pipe #true #true #false #false) 3 2)))
                            (GooFlow-direction EX-GOO3)))

;; Signature - dir-maker: string pipe -> string
;; Purpose - Generates a new direction based on the pipe found at the xy-check location. 
(define (dir-maker current-dir eval-pipe)
  (cond
    [(or (string-ci=? current-dir "right") (string-ci=? current-dir "left"))
     (if (pipe-top eval-pipe) "top" "bottom")]
    [(or (string-ci=? current-dir "top") (string-ci=? current-dir "bottom"))
     (if (pipe-left eval-pipe) "left" "right")]))

(check-expect (dir-maker "right" (make-pipe #true #false #true #false)) "top")
(check-expect (dir-maker "left" (make-pipe #false #true #true #false)) "bottom")
(check-expect (dir-maker "top" (make-pipe #true #false #true #false)) "left")
(check-expect (dir-maker "bottom" (make-pipe #true #false #false #true)) "right")

;; Signature - grid-goo-propagate: GooFlow Grid -> GooFlow
;; Purpose - takes in a GooFlow and a grid and moves the goo by 1 tile if is not stuck.
(define (grid-goo-propagate goo-flow grid)
  (if (not (boolean? (pipe-at grid (posn-x (xy-check goo-flow)) (posn-y (xy-check goo-flow)))))
      (new-goo-state goo-flow (pipe-at grid (posn-x (xy-check goo-flow)) (posn-y (xy-check goo-flow)))
                     (posn-x (xy-check goo-flow))
                     (posn-y (xy-check goo-flow)))
      goo-flow))

(check-expect (grid-goo-propagate EX-GOO GOO-GRID)
              (new-goo-state EX-GOO (pipe-at GOO-GRID (posn-x (xy-check EX-GOO))
                                             (posn-y (xy-check EX-GOO)))
                             (posn-x (xy-check EX-GOO))
                             (posn-y (xy-check EX-GOO))))
(check-expect (grid-goo-propagate EX-GOO2 GOO-GRID2)
              EX-GOO2)
(check-expect (grid-goo-propagate EX-GOO3 GOO-GRID2)
              EX-GOO3)
(check-expect (grid-goo-propagate EX-GOO4 GOO-GRID)
              (new-goo-state EX-GOO4 (pipe-at GOO-GRID (posn-x (xy-check EX-GOO4))
                                              (posn-y (xy-check EX-GOO4)))
                             (posn-x (xy-check EX-GOO4))
                             (posn-y (xy-check EX-GOO4))))

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(define (place-pipe grid pipe row col)
  (make-Grid (Grid-grid-size grid) (append (Grid-list-of-pipes grid)
                                           (list (make-pipe-with-coordinates pipe row col)))))

(check-expect (place-pipe GRID-WITH-TBBLBR PIPE-TL 0 1)
              (make-Grid 5 (append (Grid-list-of-pipes GRID-WITH-TBBLBR)
                                   (list (make-pipe-with-coordinates PIPE-TL 0 1)))))
(check-expect (place-pipe STARTING-GRID PIPE-TB 3 2)
              (make-Grid 7 (append (Grid-list-of-pipes STARTING-GRID)
                                   (list (make-pipe-with-coordinates PIPE-TB 3 2)))))
(check-expect (place-pipe GRID-WITH-CTLTR PIPE-BR 2 1)
              (make-Grid 5 (append (Grid-list-of-pipes GRID-WITH-CTLTR)
                                   (list (make-pipe-with-coordinates PIPE-BR 2 1)))))

;; : Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(define (pipe-at grid row col)
  (cond
    [(cons? (Grid-list-of-pipes grid))
     (if (and (= (pipe-with-coordinates-x (first (Grid-list-of-pipes grid))) row)
              (= (pipe-with-coordinates-y (first (Grid-list-of-pipes grid))) col))

         (pipe-with-coordinates-pipe (first (Grid-list-of-pipes grid)))
               
         (pipe-at (make-Grid (Grid-grid-size grid) (rest (Grid-list-of-pipes grid)))
                  row col))]
    [else #false]))

(check-expect (pipe-at STARTING-GRID 2 3) #false)
(check-expect (pipe-at GRID-WITH-TBBLBR 2 2) (make-pipe #true #true #false #false))
(check-expect (pipe-at GRID-WITH-CTLTR 3 2) #false)


;; Purpose - returns a (make-posn x y) given a pipe and a tile side length
;; Signature - make-posn-creator-func: pipe-with-coordinates int -> make-posn(int, int)
(define (make-posn-creator-func pipe tile-side-length)
  (make-posn (+ (* (pipe-with-coordinates-x pipe) tile-side-length) (/ tile-side-length 2))
             (+ (* (pipe-with-coordinates-y pipe) tile-side-length) (/ tile-side-length 2))))

(check-expect (make-posn-creator-func PIPE-EX1 100) (make-posn 250 250))
(check-expect (make-posn-creator-func PIPE-EX2 150) (make-posn 375 375))
(check-expect (make-posn-creator-func PIPE-EX3 200) (make-posn 500 300)) 

;; Purpose - creates a list of all positions(x, y) of the pipes placed on the grid
;; Signature - make-posn-creator: (list int -> make-posn(int, int)) int X -> Y
(define (make-posn-creator make-posn-creator-func tile-side-length list)
  (cond
    [(empty? list) '()]
    [(cons? list) (cons (make-posn-creator-func (first list) tile-side-length)
                        (make-posn-creator make-posn-creator-func tile-side-length (rest list)))]))

(check-expect (make-posn-creator make-posn-creator-func 200
                                 (Grid-list-of-pipes STARTING-GRID)) (list ))

(check-expect (make-posn-creator make-posn-creator-func 200
                                 (Grid-list-of-pipes GRID-WITH-CTLTR))
              (list (make-posn 700 300) (make-posn 500 500) (make-posn 500 300) (make-posn 100 100)))

(check-expect (make-posn-creator make-posn-creator-func 100
                                 (Grid-list-of-pipes GRID-WITH-TBBLBR))
              (list (make-posn 250 250) (make-posn 150 250) (make-posn 50 50))) 

;; Signature - is-member?: pipe [List-of pipe-with-coordinates] -> Boolean
;; Purpose - checks if the given pipe is a member of the given list of pipes.
(define (is-member? goo-pipe goo-pipes)
  (ormap (lambda (x) (and (= (pipe-with-coordinates-x goo-pipe)
                             (pipe-with-coordinates-x x))
                          (= (pipe-with-coordinates-y goo-pipe)
                             (pipe-with-coordinates-y x)))) goo-pipes))

(check-expect (is-member? PIPE-EX8 (GooFlow-goo-filled-pipes EX-GOO)) #true)
(check-expect (is-member? PIPE-EX9 (GooFlow-goo-filled-pipes EX-GOO)) #false)
(check-expect (is-member? PIPE-EX10 (GooFlow-goo-filled-pipes EX-GOO)) #false)

;; Purpose - Counts how many times a pipe appears in a list of goo-filled-pipes
;; Signature - counter: pipe-with-coordinates [List-of-goo-filled-pipes] -> number
(define (counter mainp logfp)
  (cond [(empty? logfp) 0]
        [else (+ (if (and (= (pipe-with-coordinates-x mainp)
                             (pipe-with-coordinates-x (first logfp)))
                          (= (pipe-with-coordinates-y mainp)
                             (pipe-with-coordinates-y (first logfp)))) 1 0)
                 (counter mainp (rest logfp)))]))
(check-expect (counter HW11PIPE-1 (GooFlow-goo-filled-pipes HW11GOO)) 1)
(check-expect (counter (make-pipe-with-coordinates (make-pipe #true #true #true #true) 2 2)
                       (list (make-pipe-with-coordinates (make-pipe #true #true #true #true) 2 2)
                             (make-pipe-with-coordinates (make-pipe #true #true #true #true) 2 2))) 2)
(check-expect (counter (make-pipe-with-coordinates (make-pipe #true #true #true #true) 2 2)
                       (list (make-pipe-with-coordinates (make-pipe #true #true #true #true) 2 2)
                             (make-pipe-with-coordinates (make-pipe #true #true #true #true) 2 2)
                             (make-pipe-with-coordinates (make-pipe #true #false #true #false) 3 2)
                             (make-pipe-with-coordinates (make-pipe #false #false #true #true) 2 3))) 2)

;; Purpose - creates a direction based on the surrounding pipes of the current cross pipe.
;; Signature - str-dir-maker: pipe-with-coordinates pipe-with-coordinates -> string
(define (str-dir-maker mainp eval-pipe)
  (if (= (abs (- (pipe-with-coordinates-x mainp) (pipe-with-coordinates-x eval-pipe))) 1) "right" "top"))
(check-expect (str-dir-maker (make-pipe-with-coordinates (make-pipe #true #true #true #true) 2 2)
                             (make-pipe-with-coordinates (make-pipe #true #true #false #false) 2 1)) "top")
(check-expect (str-dir-maker (make-pipe-with-coordinates (make-pipe #true #true #true #true) 2 2)
                             (make-pipe-with-coordinates (make-pipe #true #true #false #false) 3 2)) "right")
(check-expect (str-dir-maker (make-pipe-with-coordinates (make-pipe #true #true #true #true) 2 2)
                             (make-pipe-with-coordinates (make-pipe #true #true #false #false) 3 2)) "right")

;; Purpose - gets the pipe in the list that comes right before the given pipe.
;; Signature - previous-pipe-getter: pipe-with-coordinates [list-of goo-filled-pipes] -> pipe-with-coordinates
(define (previous-pipe-getter mainp logfp)
  (local [(define (track-previous-pipe logfp2 prev-pipe)
            (if (and (= (pipe-with-coordinates-x (first logfp2))
                        (pipe-with-coordinates-x mainp))
                     (= (pipe-with-coordinates-y (first logfp2))
                        (pipe-with-coordinates-y mainp))) prev-pipe
                                                          (track-previous-pipe (rest logfp2) (first logfp2))))]
    (track-previous-pipe logfp "")))
(check-expect (previous-pipe-getter (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 2)
                                    (list (make-pipe-with-coordinates (make-pipe #true #true #false #false) 3 2)
                                          (make-pipe-with-coordinates (make-pipe #true #false #false #true) 2 2)
                                          (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 2)))
              (make-pipe-with-coordinates (make-pipe #true #false #false #true) 2 2))
(check-expect (previous-pipe-getter (make-pipe-with-coordinates (make-pipe #true #false #false #true) 2 2)
                                    (list (make-pipe-with-coordinates (make-pipe #true #true #false #false) 3 2)
                                          (make-pipe-with-coordinates (make-pipe #true #false #false #true) 2 2)
                                          (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 2)))
              (make-pipe-with-coordinates (make-pipe #true #true #false #false) 3 2))
(check-expect (previous-pipe-getter (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 1)
                                    (list (make-pipe-with-coordinates (make-pipe #true #true #false #false) 3 2)
                                          (make-pipe-with-coordinates (make-pipe #true #false #false #true) 2 2)
                                          (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 2)
                                          (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 1)))
              (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 2))
;; Purpose - creates a list of all images of the pipes that are placed on the grid
;; Signature - list->image int int [List-of pipes-with-coordinates] GooFlow GameState -> [List-of Image]
(define (list->image tile-side-length pipe-width list goo-pipes gs)
  (cond
    [(empty? list) '()]
    [(cons? list)
     (cons (pipe->image
            (pipe-with-coordinates-pipe (first list)) tile-side-length pipe-width
            (is-member? (first list) (GooFlow-goo-filled-pipes goo-pipes))
            (cond
              [(and (is-member? (first list) (GooFlow-goo-filled-pipes goo-pipes))
                    (pipe-bot (pipe-with-coordinates-pipe (first list)))
                    (pipe-top (pipe-with-coordinates-pipe (first list)))
                    (pipe-left (pipe-with-coordinates-pipe (first list)))
                    (= 2 (counter (first list) (GooFlow-goo-filled-pipes goo-pipes))))
               "all"]
              [(and (is-member? (first list) (GooFlow-goo-filled-pipes goo-pipes))
                    (pipe-bot (pipe-with-coordinates-pipe (first list)))
                    (pipe-top (pipe-with-coordinates-pipe (first list)))
                    (pipe-left (pipe-with-coordinates-pipe (first list))))
               (str-dir-maker
                (first list)
                (previous-pipe-getter (first list) (GooFlow-goo-filled-pipes goo-pipes)))]
              [else "null"]))
           (list->image  tile-side-length pipe-width (rest list) goo-pipes gs))]))

(check-expect  (list->image 100 20 (Grid-list-of-pipes STARTING-GRID) EX-GOO EX-GS) (list))
(check-expect  (list->image 200 40 (Grid-list-of-pipes GRID-WITH-TBBLBR) EX-GOO EX-GS)
               (list (overlay/align "middle" "middle"
                                    (rectangle 40 200 "solid" "black")
                                    (square 200 "solid" "gray"))
                     (pipe->image-helper 200 40 "left" "bottom" 2 #false)
                     (pipe->image-helper 200 40 "right" "bottom" 2 #false)))

;; Purpose - Creates a row with the number of squares equal to grid size with length tile-side-length
;; Signature - row-creator: int int -> image
(define (row-creator tile-side-length size)
  (cond
    [(= size 0) (square 0 "outline" "white")]
    [else
     (beside (square tile-side-length "outline" "gray") (row-creator tile-side-length (- size 1)))]))
(check-expect (row-creator 100 7) (beside (square 100 "outline" "gray") (row-creator 100 6)))
(check-expect (row-creator 200 3) (beside (square 200 "outline" "gray") (row-creator 200 2)))

;; Purpose - Creates the grid by multiplying the row from row-creator function by the grid size
;; Signature - grid-creator: int int int -> image
(define (grid-creator tile-side-length variable-size absolute-size)
  (cond
    [(= variable-size 0) (square 0 "outline" "white")]
    [else
     (above (row-creator tile-side-length absolute-size)
            (grid-creator tile-side-length (- variable-size 1) absolute-size))]))
(check-expect (grid-creator 100 20 20)
              (above (row-creator 100 20) (grid-creator 100 19 20)))
(check-expect (grid-creator 200 5 5)
              (above (row-creator 200 5) (grid-creator 200 4 5)))

;; grid->image: Grid Integer Integer GameState -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width gs)
  (cond
    [(empty? (Grid-list-of-pipes grid))
     (grid-creator tile-side-length (Grid-grid-size grid) (Grid-grid-size grid))]
   
    [(cons? (Grid-list-of-pipes grid))
     (place-images

      (list->image tile-side-length pipe-width (Grid-list-of-pipes grid) (GameState-GooFlow gs) gs)

      (make-posn-creator make-posn-creator-func tile-side-length (Grid-list-of-pipes grid))
                 
      (grid-creator tile-side-length (Grid-grid-size grid) (Grid-grid-size grid)))]))


;; Signature - side-bar-image: [List-of pipes] int int -> image
;; Purpose - takes in a list of incoming pipes, tile length and pipe width to display the next
;; upcoming 4 pipes on the screen.
(define (side-bar-image ip-list tile-side-length pipe-width gs)
  (cond
    [(>= (length ip-list) 4)
     (above (text (string-append "Score: "  (number->string (get-score gs))) 16 "green")
            (text (string-append "Pipes Left: "  (number->string (length ip-list))) 16 "green")
            (pipe->image (first ip-list) tile-side-length pipe-width #false "")
            (pipe->image (first (rest ip-list)) tile-side-length pipe-width #false "")
            (pipe->image (second (rest ip-list)) tile-side-length pipe-width #false "")
            (pipe->image (third (rest ip-list)) tile-side-length pipe-width #false ""))]
    [(= (length ip-list) 3)
     (above (text (string-append "Score: "  (number->string (get-score gs))) 16 "green")
            (text "Pipes Left: 3" 16 "red")
            (pipe->image (first ip-list) tile-side-length pipe-width #false "")
            (pipe->image (first (rest ip-list)) tile-side-length pipe-width #false "")
            (pipe->image (second (rest ip-list)) tile-side-length pipe-width #false "")
            (square tile-side-length "outline" "black"))]
    [(= (length ip-list) 2)
     (above (text (string-append "Score: "  (number->string (get-score gs))) 16 "green")
            (text "Pipes Left: 2" 16 "red")
            (pipe->image (first ip-list) tile-side-length pipe-width #false "")
            (pipe->image (first (rest ip-list)) tile-side-length pipe-width #false "")
            (square tile-side-length "outline" "black")
            (square tile-side-length "outline" "black"))]
    [(= (length ip-list) 1)
     (above (text (string-append "Score: "  (number->string (get-score gs))) 16 "green")
            (text "Pipes Left: 1" 16 "red")
            (pipe->image (first ip-list) tile-side-length pipe-width #false "")
            (square tile-side-length "outline" "black")
            (square tile-side-length "outline" "black")
            (square tile-side-length "outline" "black"))]
    [(= (length ip-list) 0)
     (above (text (string-append "Score: "  (number->string (get-score gs))) 16 "green")
            (text "Pipes Left: 0" 16 "red")
            (square tile-side-length "outline" "black")
            (square tile-side-length "outline" "black")
            (square tile-side-length "outline" "black")
            (square tile-side-length "outline" "black"))]))

(check-expect (side-bar-image (list ) 100 20 EMPTY)
              (above (text "Score: 0" 16 "green") (text "Pipes Left: 0" 16 "red")
                     (square 100 "outline" "black") (square 100 "outline" "black")
                     (square 100 "outline" "black") (square 100 "outline" "black")))
(check-expect (side-bar-image (list PIPE-BL) 100 20 EMPTY)
              (above (text "Score: 0" 16 "green") (text "Pipes Left: 1" 16 "red")
                     (pipe->image PIPE-BL 100 20 #false "")
                     (square 100 "outline" "black") (square 100 "outline" "black")
                     (square 100 "outline" "black")))
(check-expect (side-bar-image (list PIPE-BL PIPE-C) 100 20 EMPTY)
              (above
               (text "Score: 0" 16 "green") (text "Pipes Left: 2" 16 "red")
               (pipe->image PIPE-BL 100 20 #false "") (pipe->image PIPE-C 100 20 #false "")
               (square 100 "outline" "black") (square 100 "outline" "black")))
(check-expect (side-bar-image (list PIPE-BL PIPE-C PIPE-TB) 100 20 EX-GS)
              (above (text "Score: 0" 16 "green") (text "Pipes Left: 3" 16 "red")
                     (pipe->image PIPE-BL 100 20 #false "") (pipe->image PIPE-C 100 20 #false "")
                     (pipe->image PIPE-TB 100 20 #false "") (square 100 "outline" "black")))
(check-expect (side-bar-image (list PIPE-BL PIPE-C PIPE-TB PIPE-TR) 100 20 EX-GS)
              (above (text "Score: 0" 16 "green") (text "Pipes Left: 4" 16 "green")
                     (pipe->image PIPE-BL 100 20 #false "") (pipe->image PIPE-C 100 20 #false "")
                     (pipe->image PIPE-TB 100 20 #false "") (pipe->image PIPE-TR 100 20 #false "")))

(define-struct GameState(grid incoming-pipes tile-side-length pipe-width GooFlow pipes-replaced time-till))

;; A GameState is a (make-GameState Grid [List-of Pipe] Number Number GooFlow Number Number
;; where Grid is the struct Grid
;; incoming-pipes is a list of incoming pipes
;; and tile-side-length is the lenght and width of the each tile on the grid.
;; and GooFlow is the GooFlow of the of goo, and pipes-replaced is the # of pipes the user replaced
;; and time-till represents the time until the next goo flow propogation
;; Interpretation: Represents the current GameState including all the pipes on the grid,
;; ,the pipes on the grid filled with goo and the list of incoming pipes as well.

(define (GameState-template sample-GameState)
  (...
   (Grid-template (GameState-grid sample-GameState)) ...
   (list-template-for-pipes (GameState-incoming-pipes sample-GameState)) ...
   (GameState-tile-side-length sample-GameState)...
   (GameState-pipe-width sample-GameState)...
   (GooFlowTemplate (GameState-GooFlow sample-GameState))...
   (GameState-pipes-repalced sample-GameState) ...
   (GameState-time-till sample-GameState)))

(define EMPTY (make-GameState STARTING-GRID
                              (list ) 100 20 EX-GOO 0 140))
(define EX-GS (make-GameState GRID-WITH-TBBLBR
                              (list PIPE-BL PIPE-C PIPE-LR PIPE-TL PIPE-TR PIPE-BR PIPE-BL)
                              100 20 EX-GOO 0 140))
(define EX-GS2 (make-GameState GRID-WITH-CTLTR
                               (list PIPE-BL PIPE-C PIPE-TB PIPE-LR) 150 30 EX-GOO 0 140))
(define EX-GS-GOO (make-GameState GOO-GRID
                                  (list PIPE-BL PIPE-C PIPE-LR PIPE-TL PIPE-TR PIPE-BR)
                                  100 20 EX-GOO 0 140))
(define EX-GS-GOO2 (make-GameState GOO-GRID2
                                   (list  PIPE-C PIPE-C PIPE-C PIPE-BR PIPE-C PIPE-TR
                                          PIPE-C PIPE-C PIPE-C PIPE-TL PIPE-BL PIPE-C
                                          PIPE-C PIPE-C PIPE-C PIPE-BL PIPE-BR PIPE-C PIPE-C PIPE-C
                                          PIPE-TL PIPE-TR PIPE-C PIPE-C PIPE-C PIPE-BL PIPE-TL
                                          PIPE-C PIPE-C PIPE-C PIPE-BR PIPE-TR)
                                   100 20 EX-GOO2 0 540))

(define HW11-GS (make-GameState HW11GRID
                                (list PIPE-BR PIPE-TB PIPE-LR PIPE-C PIPE-TL) 100 20 HW11GOO 0 140))

;; Signature: GameStateHelper: string -> pipe
;; Purpose - takes in a direction as a string and creates a starting pipe based on that direction.
(define (GameStateHelper direction)
  (cond [(string-ci=? direction "left")
         (make-pipe #false #false #true #false)]
        [(string-ci=? direction "right")
         (make-pipe #false #false #false #true)]
        [(string-ci=? direction "top")
         (make-pipe #true #false #false #false)]
        [(string-ci=? direction "bottom")
         (make-pipe #false #true #false #false)]))
(check-expect (GameStateHelper "left") (make-pipe #false #false #true #false))
(check-expect (GameStateHelper "right") (make-pipe #false #false #false #true))
(check-expect (GameStateHelper "top") (make-pipe #true #false #false #false))
(check-expect (GameStateHelper "bottom") (make-pipe #false #true #false #false))


;; Signature - gamestate-init: num num num string [List-of Pipes] num num -> GameState
;; Purpose - creates a gamestate given dimensions, x, y coordinates, and direction of the starting pipe,
;; the list of incoming pipes, tile side length and the pipe width.
(define (gamestate-init grid-dimension x y direction incoming-pipes-list tile-side-length pipe-width)
  (make-GameState (make-Grid grid-dimension
                             (list (make-pipe-with-coordinates (GameStateHelper direction) x y)))
                  incoming-pipes-list tile-side-length pipe-width
                  (make-GooFlow (list (make-pipe-with-coordinates (GameStateHelper direction) x y))
                                direction) 0 140))

(check-expect (gamestate-init 5 2 2 "left" (list ) 100 20)
              (make-GameState (make-Grid 5 (list (make-pipe-with-coordinates
                                                  (GameStateHelper "left") 2 2)))
                              '() 100 20
                              (make-GooFlow (list (make-pipe-with-coordinates
                                                   (GameStateHelper "left") 2 2))
                                            "left") 0 140))
(check-expect (gamestate-init 10 3 4 "bottom" (list PIPE-C) 150 30)
              (make-GameState (make-Grid 10 (list (make-pipe-with-coordinates
                                                   (GameStateHelper "bottom") 3 4)))
                              (list PIPE-C) 150 30
                              (make-GooFlow (list (make-pipe-with-coordinates
                                                   (GameStateHelper "bottom") 3 4))
                                            "bottom") 0 140))
(check-expect (gamestate-init 7 2 1 "right" (list PIPE-C PIPE-TL) 80 20)
              (make-GameState (make-Grid 7 (list (make-pipe-with-coordinates
                                                  (GameStateHelper "right") 2 1)))
                              (list PIPE-C PIPE-TL) 80 20
                              (make-GooFlow (list (make-pipe-with-coordinates
                                                   (GameStateHelper "right") 2 1))
                                            "right") 0 140))

;; This is the last task of hw8 where we are supposed to write an example or two showing the features
;; we implemented this hw. Alternatively, you could also run the game by uncommenting the last line of
;; this file to see the "actual" game.
(define EX-GSINIT (gamestate-init 7 2 3
                                  "top" (list PIPE-BL PIPE-C PIPE-LR PIPE-TL PIPE-TR PIPE-BR) 100 20))
(define EX-GSINIT2 (gamestate-init 4 2 1
                                   "bottom" (list PIPE-BR PIPE-C PIPE-TL PIPE-TR PIPE-TB PIPE-C) 150 30))

;; Signature: any-goo-pipe?: Number Number GooFlow -> Boolean
;; Purpose - checks if the given x and y coordinates match with any of the goo filled pipes' coordinates
(define (any-goo-pipe? x y gf)
  (ormap (lambda (var) (and (= x (pipe-with-coordinates-x var))
                            (= y (pipe-with-coordinates-y var)))) (GooFlow-goo-filled-pipes gf)))
(check-expect (any-goo-pipe? 1 2 (make-GooFlow (list HW11PIPE-1) RIGHT)) #false)
(check-expect (any-goo-pipe? 1 1 (make-GooFlow (list HW11PIPE-1) RIGHT)) #true)
(check-expect (any-goo-pipe? 2 2  (make-GooFlow (list HW11PIPE-1 HW11PIPE-2 HW11PIPE-3) BOTTOM)) #true)


;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(define (place-pipe-on-click gs x y event)
  (if (string=? event "button-down")
      (cond
        [(empty? (GameState-incoming-pipes gs)) gs]
        [(not (any-goo-pipe? (floor (/ x (GameState-tile-side-length gs)))
                             (floor (/ y (GameState-tile-side-length gs)))
                             (GameState-GooFlow gs)))
         (make-GameState
          (make-Grid
           (Grid-grid-size (GameState-grid gs))
           (append   (list (make-pipe-with-coordinates
                            (if (cons? (GameState-incoming-pipes gs))
                                (first (GameState-incoming-pipes gs)) '())
                            (floor (/ x (GameState-tile-side-length gs)))
                            (floor (/ y (GameState-tile-side-length gs)))))
                     (Grid-list-of-pipes (GameState-grid gs))
                     ))
          (if (> (length (GameState-incoming-pipes gs)) 1)
              (rest (GameState-incoming-pipes gs)) '())
          (GameState-tile-side-length gs)
          (GameState-pipe-width gs)
          (GameState-GooFlow gs)
          (if (pipe? (pipe-at (GameState-grid gs) (floor (/ x (GameState-tile-side-length gs)))
                              (floor (/ y (GameState-tile-side-length gs)))))
              (add1 (GameState-pipes-replaced gs))
              (GameState-pipes-replaced gs)
              )(GameState-time-till gs))]
        [else gs]) gs))

(check-expect (place-pipe-on-click EX-GS 210 140 "button-down") EX-GS)
(check-expect (place-pipe-on-click EX-GS 20 40 "button-up") EX-GS)
(check-expect (place-pipe-on-click EX-GS 190 80 "button-down")
              (make-GameState (make-Grid (Grid-grid-size (GameState-grid EX-GS))
                                         (reverse
                                          (append
                                           (reverse
                                            (Grid-list-of-pipes (GameState-grid EX-GS)))
                                           (list (make-pipe-with-coordinates (first
                                                                              (GameState-incoming-pipes EX-GS)) 1 0)))))
                              (rest (GameState-incoming-pipes EX-GS))
                              100 20 EX-GOO 0 140))
(check-expect (place-pipe-on-click EMPTY 290 280 "button-down") (make-GameState (make-Grid 7 '()) '()
                                                                                100 20 (make-GooFlow (list (make-pipe-with-coordinates
                                                                                                            (make-pipe #false #true #false #false) 2 1)) "bottom") 0 140))
(check-expect (place-pipe-on-click EMPTY 2900 280 "button-down") (make-GameState (make-Grid 7 '()) '()
                                                                                 100 20 (make-GooFlow (list (make-pipe-with-coordinates
                                                                                                             (make-pipe #false #true #false #false) 2 1)) "bottom") 0 140))

;; get-score: GameState -> Integer
;; Computes the score of the current GameState
(define (get-score gs)
  (* 50 (- (- (length (foldr (lambda (x y) (cons x
                                                 (filter (lambda (z) (not (and (= (posn-y x) (posn-y z))
                                                                               (= (posn-x x) (posn-x z))) )) y))) '()
                                                                                                                  (map (lambda (x) (make-posn (pipe-with-coordinates-x x)
                                                                                                                                              (pipe-with-coordinates-y x)))
                                                                                                                       (GooFlow-goo-filled-pipes (GameState-GooFlow gs))))) 1)
           (GameState-pipes-replaced gs))))

(check-expect (get-score HW11-GS) 0)
(check-expect (get-score (make-GameState (make-Grid  6 (list
                                                        (make-pipe-with-coordinates (make-pipe #false #true #false #true) 1 3)
                                                        (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 1)
                                                        (make-pipe-with-coordinates (make-pipe #false #true #true #false) 2 1)
                                                        (make-pipe-with-coordinates (make-pipe #true #true #true #true) 1 3))) '() 100 20 (make-GooFlow (list
                                                                                                                                                         (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 1)
                                                                                                                                                         (make-pipe-with-coordinates (make-pipe #false #true #true #false) 2 1)
                                                                                                                                                         (make-pipe-with-coordinates (make-pipe #false #true #false #true) 1 3)) "right") 1 28)) 50)
(check-expect (get-score (make-GameState (make-Grid  6 (list
                                                        (make-pipe-with-coordinates (make-pipe #false #true #false #true) 1 3)
                                                        (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 1)
                                                        (make-pipe-with-coordinates (make-pipe #false #true #true #false) 2 1)
                                                        (make-pipe-with-coordinates (make-pipe #true #true #true #true) 1 3))) '() 100 20 (make-GooFlow (list
                                                                                                                                                         (make-pipe-with-coordinates (make-pipe #false #false #false #true) 1 1)
                                                                                                                                                         (make-pipe-with-coordinates (make-pipe #false #true #true #false) 2 1)
                                                                                                                                                         (make-pipe-with-coordinates (make-pipe #false #true #false #true) 1 3)) "right") 3 28)) -50)
;; Purpose - function can be passed in to-draw in big bang and uses grid->image to convert gs to image
;; Signature - draw-grid: GameState -> image
(define (draw-grid gs)
  (beside/align "middle" (grid->image (GameState-grid gs)
                                      (GameState-tile-side-length gs)
                                      (GameState-pipe-width gs) gs)
                (side-bar-image (GameState-incoming-pipes gs)
                                (GameState-tile-side-length gs)
                                (GameState-pipe-width gs)
                                gs)))
(check-expect (draw-grid EX-GS2)
              (beside/align "middle"
                            (grid->image (GameState-grid EX-GS2)
                                         (GameState-tile-side-length EX-GS2)
                                         (GameState-pipe-width EX-GS2)
                                         EX-GS2)
                            (side-bar-image (GameState-incoming-pipes EX-GS2)
                                            (GameState-tile-side-length EX-GS2)
                                            (GameState-pipe-width EX-GS2)
                                            EX-GS2)))
(check-expect (draw-grid EX-GS)
              (beside/align "middle"
                            (grid->image (GameState-grid EX-GS)
                                         (GameState-tile-side-length EX-GS)
                                         (GameState-pipe-width EX-GS)
                                         EX-GS)
                            (side-bar-image
                             (GameState-incoming-pipes EX-GS)
                             (GameState-tile-side-length EX-GS)
                             (GameState-pipe-width EX-GS)
                             EX-GS)))

;; Purpose - handles tick events so goo flows automatically after 5 seconds and then on 1 sec interval
;; Signature - on-tick-handler: GameState -> GameState
(define (on-tick-handler gs)
  (cond
    [(> (GameState-time-till gs) 0) (make-GameState (GameState-grid gs)
                                                    (GameState-incoming-pipes gs)
                                                    (GameState-tile-side-length gs)
                                                    (GameState-pipe-width gs)
                                                    (GameState-GooFlow gs)
                                                    (GameState-pipes-replaced gs)
                                                    (sub1 (GameState-time-till gs)))]
    [(= (GameState-time-till gs) 0) (make-GameState (GameState-grid gs)
                                                    (GameState-incoming-pipes gs)
                                                    (GameState-tile-side-length gs)
                                                    (GameState-pipe-width gs)
                                                    (grid-goo-propagate (GameState-GooFlow gs)
                                                                        (GameState-grid gs))
                                                    (GameState-pipes-replaced gs)
                                                    28)]))
;; Purpose - Stops the game when the goo cannot propagate anymore automatically
;; Signature - stopper: GameState -> GameState
(define (stopper gs)
  (and (< (GameState-time-till gs) 29)
       (or    (boolean? (pipe-at (GameState-grid gs) (posn-x (xy-check (GameState-GooFlow gs)))
                                 (posn-y (xy-check (GameState-GooFlow gs)))))
              (and (string-ci=? (GooFlow-direction (GameState-GooFlow gs)) "top")
                   (not (pipe-bot (pipe-at (GameState-grid gs) (posn-x (xy-check (GameState-GooFlow gs)))
                                           (posn-y (xy-check (GameState-GooFlow gs)))))))
              (and (string-ci=? (GooFlow-direction (GameState-GooFlow gs)) "bottom")
                   (not (pipe-top (pipe-at (GameState-grid gs) (posn-x (xy-check (GameState-GooFlow gs)))
                                           (posn-y (xy-check (GameState-GooFlow gs)))))))
              (and (string-ci=? (GooFlow-direction (GameState-GooFlow gs)) "right")
                   (not (pipe-left (pipe-at (GameState-grid gs) (posn-x (xy-check (GameState-GooFlow gs)))
                                            (posn-y (xy-check (GameState-GooFlow gs)))))))
              (and (string-ci=? (GooFlow-direction (GameState-GooFlow gs)) "left")
                   (not (pipe-right (pipe-at (GameState-grid gs) (posn-x (xy-check (GameState-GooFlow gs)))
                                             (posn-y (xy-check (GameState-GooFlow gs))))))))))

;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
    [on-mouse place-pipe-on-click]
    [on-tick on-tick-handler]
    [to-draw draw-grid]
    [stop-when stopper draw-grid]))

(pipe-fantasy HW11-GS)

