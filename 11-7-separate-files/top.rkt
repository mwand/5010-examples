#lang racket

;; 11-7-separate-files
;; same as 11-6, but split into separate files for easier navigation.

;; before, the file was 816 lines.
;; now, the longest file is interfaces.rkt, at 179 lines,
;; and the other files are all around 130 lines-- much more compact.

(require "wall.rkt")
(require "ball.rkt")
(require "world.rkt")
(require "widgetfactory.rkt")

;; start with (run framerate).  Typically: (run 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : -> World<%>
;; RETURNS: a world with a wall, a ball, and a factory
(define (initial-world)
  (local
    ((define the-wall (new Wall%))
     (define the-ball (new Ball% [w the-wall]))
     (define the-world
       (make-world 
         empty 
         (list the-wall)))
     (define the-factory
       (new WidgetFactory% [wall the-wall][world the-world])))
    (begin
      ;; put the factory in the world
      (send the-world add-stateful-widget the-factory)
      ;; tell the factory to start a ball
      (send the-factory after-key-event "b")
      the-world)))

; run-world : PosReal -> StatefulWorld<%>
; GIVEN: a frame rate, in secs/tick
; EFFECT: runs the given at the given frame rate
; RETURNS: the initial world in its final state
(define (run framerate)
  (run-world (initial-world) framerate))
     
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

