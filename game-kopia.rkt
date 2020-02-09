#lang racket
;loading libraries
(require 2htdp/image)
(require 2htdp/universe)
(require racket/include)

;loading assets
(require "assets.rkt")
;(require "cards.rkt")
;(provide (all-from-out "assets.rkt"))

;loading config
(require "config.rkt")

;Manipulating assets

;preparing variables for the pipes
(define green-down-pipe-img pipe-green-img)
(define green-top-pipe-img    (flip-vertical green-down-pipe-img))
(define red-down-pipe-img pipe-red-img)
(define red-top-pipe-img   (flip-vertical red-down-pipe-img))


;;; DIMENSIONS
(define width  (image-width  background-img))
(define height (image-height background-img))
(define bird-height (image-height (first horse-img)))
(define bird-width  (image-width  (first horse-img)))
(define bird-x (/ width 4))
(define pipe-width (image-width green-top-pipe-img))

;;; PIPES

(define-struct pipe (x y color) #:transparent)

;;; STATES

(define-struct get-ready ()                                           #:transparent)
(define-struct playing   (time points pipes y velocity acceleration)  #:transparent)
(define-struct game-over (death-image)                                #:transparent)

; get-ready
;   The game is waiting for the player to get ready.
; playing
;   The game has started.
;     time     = number of ticks since game start
;     points   = number of pipes passed
;     pipes    = a list of pipes on screen
;     y        = y coordinate of bird
;     velocity = number of pixels to move bird per tick (positive is down, negative is up)
;     acceleartion = number of pixels per tick per tick to change velocity
; game-over
;   The game is over. The number of points is displayed.
;     death-image = the last image of the game, i.e. screen shot of where the payer died

(define gravity 0.5)
(define pipe-gap-size 50)

(define (random-pipe)
  (define y (list-ref (list 0 50 100 150) (random 4)))
  (define color (list-ref (list "green" "red") (random 2)))
  (make-pipe (* 2 width) y color))

(define initial-playing-state 
  (make-playing 0 0 
                (list (make-pipe width 100 "green") (random-pipe))
                (/ height 2) 0 gravity))

;;; PLAYING STATE MANIPULATORS

(define (increase-time p)
  (make-playing (+ (playing-time p) 1) (playing-points p) (playing-pipes p)
                (playing-y p) (playing-velocity p) (playing-acceleration p)))

(define (increase-points p)
  (make-playing (playing-time p) (+ (playing-points p) 1) (playing-pipes p)
                (playing-y p) (playing-velocity p) (playing-acceleration p)))

(define (set-playing-pipes p ps)
  (make-playing (playing-time p) (playing-points p) ps
                (playing-y p) (playing-velocity p) (playing-acceleration p)))

(define (set-y p y)
  (make-playing (playing-time p) (playing-points p) (playing-pipes p)
                y (playing-velocity p) (playing-acceleration p)))

(define (set-velocity p v)
  (make-playing (playing-time p) (playing-points p) (playing-pipes p)
                (playing-y p) v (playing-acceleration p)))

(define (set-acceleration p a)
  (make-playing (playing-time p) (playing-points p) (playing-pipes p)
                (playing-y p) (playing-velocity p) a))
  

;;; DRAW STATES

; draw : state -> image
(define (draw s)  
  (scale 2 (draw-unscaled s)))

; draw-unscaled : state -> image
(define (draw-unscaled s)
  (cond
    [(get-ready? s) (draw-get-ready s)]
    [(playing?   s) (draw-playing   s)]
    [(game-over? s) (draw-game-over s)]
    [else           (error 'draw "unknown state")]))

; draw-get-ready : get-ready -> image
(define (draw-get-ready g)
  (place-text get-ready-img
              (place-foreground initial-playing-state background-img)))

; place-get-ready-text : image -> image
(define (place-text image-with-text i)
  (place-image image-with-text (/ width 2) (* 1/4 height) i))
  
; draw-playing : playing -> image
(define (draw-playing p)
  (overlay-bird p 
    (overlay-points p
      (place-foreground p 
        (place-all-pipes p                       
                         background-img)))))

; draw-game-over : game-over -> image
(define (draw-game-over g)
  (place-text game-over-img (game-over-death-image g)))

; overlay-bird : playing image -> image
;   draw a bird on top of the image
(define (overlay-bird p image)
  (define t (playing-time p))
  (define y (playing-y p))
  (define bird-size (image-width (first horse-img)))
  (place-image/align (list-ref horse-img (quotient (remainder t 6) 2))
                     bird-x y
                     ; (+ y (* bird-size (sin (* (/ t 40) 2 pi))))
                     "left" "top"
                     image))

; place-foreground : playing image -> image
(define (place-foreground p i)
  (define t (playing-time p))
  (place-image/align floor-img
                     ; works because foreground is large than background image
                     (- (- 12 (remainder t 12)) 12) (image-height i)
                     "left" "bottom" i))

; overlay-points : playing image -> image
(define (overlay-points p i)
  (place-image (points->image (playing-points p))
               (/ width 2) (/ height 8)
               i))

; digit->image : number-from-0-to-9 -> image
(define (digit->image n)
  (list-ref digits-img n))

; points->image : integer -> image
(define (points->image n)
  (cond
    [(< n 10) (digit->image n)]
    [else     (beside (points->image (quotient n 10))
                      (digit->image  (remainder n 10)))]))  

(define (place-all-pipes p i)
  (place-pipes (playing-pipes p) i))

(define (place-pipes ps i)
  (cond [(empty? ps) i]
        [else        (place-pipes (rest ps)
                                  (place-pipe (first ps) i))]))

(define (place-pipe p i)
  (place-bottom-pipe p (place-top-pipe p i)))

(define (color->top-pipe c)
  (cond
    [(string=? c "green") green-top-pipe-img]
    [(string=? c "red")   red-top-pipe-img]))

(define (color->bottom-pipe c)
  (cond
    [(string=? c "green") green-down-pipe-img]
    [(string=? c "red")   red-down-pipe-img]))

(define (place-top-pipe p i)
  (define pipe (color->top-pipe (pipe-color p)))
  (place-image/align pipe (pipe-x p) (pipe-y p) "left" "bottom" i))

(define (place-bottom-pipe p i)
  (define pipe (color->bottom-pipe (pipe-color p)))
  (place-image/align pipe (pipe-x p) (+ (pipe-y p) pipe-gap-size) "left" "top" i))


;;; EVENT HANDLERS

; handle-key-event : state key -> state
(define (handle-key-event state key)
  ; each state has its own key handler
  (cond
    [(get-ready? state) (handle-key/get-ready state key)]
    [(playing?   state) (handle-key/playing   state key)]
    [(game-over? state) (handle-key/game-over state key)]))

; handle-key/get-ready : get-ready key -> state
(define (handle-key/get-ready g key)
  ; no matter, what key is pressed, start a new game where
  ; time and points are both zero
  initial-playing-state)

; handle-key/playing : playing key -> state
(define (handle-key/playing p key)
  (define after-flap (update-physics (set-velocity (set-acceleration p 0) -100)))
  (set-acceleration after-flap gravity))

; handle-key/game-over : game-over key -> state
(define (handle-key/game-over g key)
  (make-get-ready))

;;; TICK EVENTS

; handle-tick-event : state -> state
(define (handle-tick-event s)
  (cond 
    [(playing? s) (handle-tick-event/playing s)]
    [else         s]))

; handle-tick-event/playing : playing -> playing
(define (handle-tick-event/playing p)
  (handle-collisions 
   (update-points
    (update-pipes
     (update-physics (increase-time p))))))

; update-physics : playing -> playing
(define (update-physics p)
  (define y (playing-y p))
  (define v (playing-velocity p))
  (define a (playing-acceleration p))
  (define new-v (max (min (+ v a) 5) -5))
  (define new-y (+ y new-v))
  (set-y (set-velocity p new-v) new-y))

; update-pipe : pipe -> pipe
(define (update-pipe p)
  (define x (pipe-x p))
  (define y (pipe-y p))
  (cond 
    [(< x (- (image-width green-down-pipe-img)))
     (random-pipe)]
    [else
     (make-pipe (- x 1) y (pipe-color p))]))

; update-pipes : playing -> playing
(define (update-pipes p)
  (set-playing-pipes p (sort-pipes (map update-pipe (playing-pipes p)))))

(define (sort-pipes ps)
  (cond 
    [(< (pipe-x (first ps)) (pipe-x (second ps))) ps]
    [else (reverse ps)]))

; handle-collisions : playing -> playing
(define (handle-collisions p)
  (cond
    [(or (bird-touches-ground? p) (bird-touches-pipe? p)) 
     (make-game-over (draw-unscaled p))]
    [else p]))

; bird-touches-ground? : playing -> boolean
(define (bird-touches-ground? p)
  (> (playing-y p) (- height (image-height floor-img) bird-height)))

(define (bird-touches-pipe? p)
  (define pipe (first (playing-pipes p)))
  (define xmin (pipe-x pipe))
  (define xmax (+ xmin pipe-width))
  (define ymin (pipe-y pipe))
  (define ymax (+ ymin pipe-gap-size))
  (define y    (playing-y p))
  (and (> (+ bird-x bird-width) xmin)
       (< bird-x xmax)
       (or (< y ymin) (> y ymax))))
  
(define (update-points p)
  (cond
    [(= bird-x (+ (pipe-x (first (playing-pipes p))) pipe-width))
     (increase-points p)]
    [else p]))
        

;;; GAME LOOP

(big-bang (make-get-ready)
          [on-draw draw]
          [on-key  handle-key-event]
          [on-tick handle-tick-event])

