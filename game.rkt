#lang racket
;loading libraries
(require 2htdp/image)
(require 2htdp/universe)

;loading assets
(require "assets.rkt")

;loading config
(require "config.rkt")

;preparing variables for the obstacles fliping images verticaly
(define rainbow-down-img rainbow-img)
(define rainbow-top-img    (flip-vertical rainbow-img))
(define rainbow-flip-down-img rainbow-flip-img)
(define rainbow-flip-top-img   (flip-vertical rainbow-flip-img))

; Defining rainbows 
(define-struct rainbow (x y color) #:transparent)

; Defining states for universe library
(define-struct get-ready ()                                           #:transparent)
(define-struct gaming   (time points rainbows y velocity acceleration)  #:transparent)
(define-struct gameover (death-image)                                #:transparent)

; get-ready
;   The game is waiting for the player to get ready.
; gaming
;   The game has started.
;     time     = number of ticks since game start
;     points   = number of rainbows passed
;     rainbows    = a list of rainbows on screen
;     y        = y coordinate of bird
;     velocity = number of pixels to move bird per tick (positive is down, negative is up)
;     acceleartion = number of pixels per tick per tick to change velocity
; gameover
;   The game is over. The number of points is displayed.
;     death-image = the last image of the game, i.e. screen shot of where the payer died



(define (random-rainbow)
  (define y (list-ref (list 0 50 100 150) (random 4)))
  (define color (list-ref (list "green" "red") (random 2)))
  (make-rainbow (* 2 width) y color))

(define initial-gaming-state 
  (make-gaming 0 0 
                (list (make-rainbow width 100 "green") (random-rainbow))
                (/ height 2) 0 gravity))

;;; gaming STATE MANIPULATORS

(define (increase-time p)
  (make-gaming (+ (gaming-time p) 1) (gaming-points p) (gaming-rainbows p)
                (gaming-y p) (gaming-velocity p) (gaming-acceleration p)))

(define (increase-points p)
  (make-gaming (gaming-time p) (+ (gaming-points p) 1) (gaming-rainbows p)
                (gaming-y p) (gaming-velocity p) (gaming-acceleration p)))

(define (set-gaming-rainbows p ps)
  (make-gaming (gaming-time p) (gaming-points p) ps
                (gaming-y p) (gaming-velocity p) (gaming-acceleration p)))

(define (set-y p y)
  (make-gaming (gaming-time p) (gaming-points p) (gaming-rainbows p)
                y (gaming-velocity p) (gaming-acceleration p)))

(define (set-velocity p v)
  (make-gaming (gaming-time p) (gaming-points p) (gaming-rainbows p)
                (gaming-y p) v (gaming-acceleration p)))

(define (set-acceleration p a)
  (make-gaming (gaming-time p) (gaming-points p) (gaming-rainbows p)
                (gaming-y p) (gaming-velocity p) a))
  

;;; DRAW STATES

; draw : state -> image
(define (draw s)  
  (scale 2 (draw-unscaled s)))

; draw-unscaled : state -> image
(define (draw-unscaled s)
  (cond
    [(get-ready? s) (draw-get-ready s)]
    [(gaming?   s) (draw-gaming   s)]
    [(gameover? s) (draw-gameover s)]
    [else           (error 'draw "unknown state")]))

; draw-get-ready : get-ready -> image
(define (draw-get-ready g)
  (place-text get-ready-img
              (place-foreground initial-gaming-state background-img)))

; place-get-ready-text : image -> image
(define (place-text image-with-text i)
  (place-image image-with-text (/ width 2) (* 1/4 height) i))
  
; draw-gaming : gaming -> image
(define (draw-gaming p)
  (overlay-bird p 
    (overlay-points p
      (place-foreground p 
        (place-all-rainbows p                       
                         background-img)))))

; draw-gameover : gameover -> image
(define (draw-gameover g)
  (place-text gameover-img (gameover-death-image g)))

; overlay-bird : gaming image -> image
;   draw a bird on top of the image
(define (overlay-bird p image)
  (define t (gaming-time p))
  (define y (gaming-y p))
  (define bird-size (image-width (first horse-img)))
  (place-image/align (list-ref horse-img (quotient (remainder t 6) 2))
                     bird-x y
                     "left" "top"
                     image))

; place-foreground : gaming image -> image
(define (place-foreground p i)
  (define t (gaming-time p))
  (place-image/align floor-img
                     ; works because foreground is large than background image
                     (- (- 12 (remainder t 12)) 12) (image-height i)
                     "left" "bottom" i))

; overlay-points : gaming image -> image
(define (overlay-points p i)
  (place-image (points->image (gaming-points p))
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

(define (place-all-rainbows p i)
  (place-rainbows (gaming-rainbows p) i))

(define (place-rainbows ps i)
  (cond [(empty? ps) i]
        [else        (place-rainbows (rest ps)
                                  (place-rainbow (first ps) i))]))

(define (place-rainbow p i)
  (place-bottom-rainbow p (place-top-rainbow p i)))

(define (color->top-rainbow c)
  (cond
    [(string=? c "green") rainbow-top-img]
    [(string=? c "red")   rainbow-flip-top-img]))

(define (color->bottom-rainbow c)
  (cond
    [(string=? c "green") rainbow-down-img]
    [(string=? c "red")   rainbow-flip-down-img]))

(define (place-top-rainbow p i)
  (define rainbow (color->top-rainbow (rainbow-color p)))
  (place-image/align rainbow (rainbow-x p) (rainbow-y p) "left" "bottom" i))

(define (place-bottom-rainbow p i)
  (define rainbow (color->bottom-rainbow (rainbow-color p)))
  (place-image/align rainbow (rainbow-x p) (+ (rainbow-y p) rainbow-gap-size) "left" "top" i))


;;; EVENT HANDLERS

; handle-key-event : state key -> state
(define (handle-key-event state key)
  ; each state has its own key handler
  (cond
    [(get-ready? state) (handle-key/get-ready state key)]
    [(gaming?   state) (handle-key/gaming   state key)]
    [(gameover? state) (handle-key/gameover state key)]))

; handle-key/get-ready : get-ready key -> state
(define (handle-key/get-ready g key)
  ; no matter, what key is pressed, start a new game where
  ; time and points are both zero
  initial-gaming-state)

; handle-key/gaming : gaming key -> state
(define (handle-key/gaming p key)
  (define after-flap (update-physics (set-velocity (set-acceleration p 0) -100)))
  (set-acceleration after-flap gravity))

; handle-key/gameover : gameover key -> state
(define (handle-key/gameover g key)
  (make-get-ready))

;;; TICK EVENTS

; handle-tick-event : state -> state
(define (handle-tick-event s)
  (cond 
    [(gaming? s) (handle-tick-event/gaming s)]
    [else         s]))

; handle-tick-event/gaming : gaming -> gaming
(define (handle-tick-event/gaming p)
  (handle-collisions 
   (update-points
    (update-rainbows
     (update-physics (increase-time p))))))

; update-physics : gaming -> gaming
(define (update-physics p)
  (define y (gaming-y p))
  (define v (gaming-velocity p))
  (define a (gaming-acceleration p))
  (define new-v (max (min (+ v a) 5) -5))
  (define new-y (+ y new-v))
  (set-y (set-velocity p new-v) new-y))

; update-rainbow : rainbow -> rainbow
(define (update-rainbow p)
  (define x (rainbow-x p))
  (define y (rainbow-y p))
  (cond 
    [(< x (- (image-width rainbow-down-img)))
     (random-rainbow)]
    [else
     (make-rainbow (- x 1) y (rainbow-color p))]))

; update-rainbows : gaming -> gaming
(define (update-rainbows p)
  (set-gaming-rainbows p (sort-rainbows (map update-rainbow (gaming-rainbows p)))))

(define (sort-rainbows ps)
  (cond 
    [(< (rainbow-x (first ps)) (rainbow-x (second ps))) ps]
    [else (reverse ps)]))

; handle-collisions : gaming -> gaming
(define (handle-collisions p)
  (cond
    [(or (bird-touches-ground? p) (bird-touches-rainbow? p)) 
     (make-gameover (draw-unscaled p))]
    [else p]))

; bird-touches-ground? : gaming -> boolean
(define (bird-touches-ground? p)
  (> (gaming-y p) (- height (image-height floor-img) bird-height)))

(define (bird-touches-rainbow? p)
  (define rainbow (first (gaming-rainbows p)))
  (define xmin (rainbow-x rainbow))
  (define xmax (+ xmin rainbow-width))
  (define ymin (rainbow-y rainbow))
  (define ymax (+ ymin rainbow-gap-size))
  (define y    (gaming-y p))
  (and (> (+ bird-x bird-width) xmin)
       (< bird-x xmax)
       (or (< y ymin) (> y ymax))))
  
(define (update-points p)
  (cond
    [(= bird-x (+ (rainbow-x (first (gaming-rainbows p))) rainbow-width))
     (increase-points p)]
    [else p]))
        

;; Starting the game function for menu button
(define (start-flappy-horse) 
  (cond
    [(big-bang (make-get-ready)
          [on-draw draw]
          [on-key  handle-key-event]
          [on-tick handle-tick-event])
  ]
    [else #f]
    ))

;; providing whole code to main menu
(provide (all-defined-out))