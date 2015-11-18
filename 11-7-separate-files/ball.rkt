#lang racket

(require "interfaces.rkt")

;; for circle
(require 2htdp/image)    

;; for superclass
(require "draggablewidget.rkt")

;; for tests:
(require rackunit)
(require "wall.rkt")  
(require "extras.rkt")

(provide Ball%)

;; The Ball% class

;; A Ball is a (new Ball% [w Wall])

(define Ball%
  (class*

    ;; inherit method implementations from DraggableWidget%
    DraggableWidget%
    
    ;; must implement SBall + the open hooks from the superclass
    (SWidgetListener<%> DraggableWidgetHooks<%>)

    ;; inherit all these fields from the superclass:

    ;; the Wall that the ball should bounce off of
    (inherit-field w)  

    ;; initial values of x, y (center of ball) and speed:
    (inherit-field x y speed)

    ; is this selected? Default is false.
    (inherit-field selected?) 

    ;; position of the wall, updated by update-wall-pos
    (inherit-field wall-pos)
    
    ;; this field is local to Ball%
    (field [radius 20])

    (super-new)

    ;; make this a method instead of a function:

    ;; -> Integer
    ;; position of the ball at the next tick
    ;; STRATEGY: use the ball's cached copy of the wall position to
    ;; set the upper limit of motion 
    (define/override (next-x-pos)
      (limit-value
        radius
        (+ x speed)
        (-  wall-pos radius)))

    ;; Number^3 -> Number
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi]
    (define (limit-value lo val hi)
      (max lo (min val hi)))

    ;; -> Integer
    ;; RETURNS: the velocity of the ball at the next tick
    ;; STRATEGY: if the ball will not be at its limit, return it
    ;; unchanged. Otherwise, negate the velocity.
    (define/override (next-speed)
      (if
        (< radius (next-x-pos) (- wall-pos radius))
        speed
        (- speed)))

    ;; the image of the ball.  This could be dynamic.
    (define/override (get-image)
      (circle radius 
        "outline"
        "red"))

    ;; in-this? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this.
    (define/override (in-this? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr radius)))
    
    ))

;; unit tests for ball:

(begin-for-test
  (local
    ((define wall1 (new Wall% [pos 200]))
     (define ball1 (new Ball% [x 110][speed 50][w wall1])))

    (check-equal? (send ball1 for-test:speed) 50)
    (check-equal? (send ball1 for-test:wall-pos) 200)

    (check-equal? (send ball1 for-test:next-speed) 50)
    (check-equal? (send ball1 for-test:next-x) 160)

    (send ball1 after-tick)

    (check-equal? (send ball1 for-test:x) 160)
    (check-equal? (send ball1 for-test:speed) 50)

    (send ball1 after-tick)

    (check-equal? (send ball1 for-test:x) 180)
    (check-equal? (send ball1 for-test:speed) -50)

    ))

(begin-for-test
  (local
    ((define wall1 (new Wall% [pos 200]))
     (define ball1 (new Ball% [x 160][speed 50][w wall1])))

    (check-equal? (send ball1 for-test:x) 160)
    (check-equal? (send ball1 for-test:speed) 50)

    (check-equal? (send ball1 for-test:next-x) 180)
    (check-equal? (send ball1 for-test:next-speed) -50)

    (send ball1 after-tick)

    (check-equal? (send ball1 for-test:x) 180)
    (check-equal? (send ball1 for-test:speed) -50)

    ))
