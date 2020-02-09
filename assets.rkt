#lang racket
(require 2htdp/image)
;how to add image
;(define my-image (bitmap "path/to/image.png"))


;main character
(define horse-img (list (bitmap "assets/horse1.png") (bitmap "assets/horse2.png") (bitmap "assets/horse3.png") ))

;digits
  (define digits-img (list (bitmap "assets/0.png")
                     (bitmap "assets/1.png")
                     (bitmap "assets/2.png")
                     (bitmap "assets/3.png")
                     (bitmap "assets/4.png")
                     (bitmap "assets/5.png")
                     (bitmap "assets/6.png")
                     (bitmap "assets/7.png")
                     (bitmap "assets/8.png")
                     (bitmap "assets/9.png")
                     ))

;pipes
(define pipe-green-img (bitmap "assets/pipe-green.png"))
(define pipe-red-img (bitmap "assets/pipe-red.png"))
 
;floor
(define floor-img (bitmap "assets/floor.png"))

;get ready
(define get-ready-img (bitmap "assets/getready.png"))

;game over
(define game-over-img (bitmap "assets/gameover.png"))

;background
(define background-img (bitmap "assets/background.png"))

(provide (all-defined-out))