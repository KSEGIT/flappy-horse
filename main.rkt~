#lang racket/gui


(require "game.rkt")
;making frame 
(define myframe ( new frame%
                      [label "main menu"]
                      [width 350] [height 350]))
(instantiate frame%
            ("Display a bitmap"))
(send myframe show #t)

(define hi ( new frame%
                 [label "HIGHSCORE"]
                 [width 350] [height 350]))
(define go ( new frame%
                 [label "Start game"]
                 [width 350]
                 [height 350]))
 
;(instantiate frame%
;           ("Display a bitmap"))
;(define the-bitmap%
;  (make-object bitmap% "assets/hi_s.png"))

;image
(define the-bitmap%
  (make-object bitmap% "assets/kutas.png"))

(define canvas
  (instantiate canvas%
    (myframe)
    (paint-callback
     (lambda (canvas dc)
       (send dc draw-bitmap the-bitmap% 0 0)))
    (min-width (send the-bitmap% get-width))
    (min-height (send the-bitmap% get-height))))

;buttons
(define butt ( new button%
                   [parent myframe]
                   [label "START GAME"]
                   [callback (lambda (o e ) (send go show #t))]))
(define butt2 ( new button%
                   [parent myframe]
                   [label "HIGHSCORE"]
                   [callback (lambda (o e ) (and (send hi show #t) ))]))
( define butt3 ( new button%
                     [parent myframe]
                     [label "EXIT"]
                     [callback   (lambda (b e)   (exit)  )]))

(define butt4 ( new button%
                    [parent hi]
                    [label "BACK"]
                    [callback (lambda (b e ) (send myframe show #t))]))

(define butt5 ( new button%
                    [parent go]
                    [label "GO!"]
                    [callback (lambda (b e) (start-flappy-horse))]))

(define butt6 ( new button%
                    [parent go]
                    [label "BACK"]
                    [callback (lambda( b e) (send myframe show #t))]))
