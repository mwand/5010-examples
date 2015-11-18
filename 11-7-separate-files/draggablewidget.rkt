#lang racket

(require 2htdp/image)                   ; for place-image
(require "interfaces.rkt")

(provide DraggableWidget%)
(provide DraggableWidgetHooks<%>)

;; A DraggableWidget is a (new DraggableWidget% [w Wall]) 

(define DraggableWidget%
  (class* object%

    ;; these guys are all stateful Widget Listeners
    (SWidgetListener<%>)  

    ;; the Wall that the ball should bounce off of
    (init-field w)  

    ;; initial values of x, y (center of widget)
    (init-field [x INIT-WIDGET-X])
    (init-field [y INIT-WIDGET-Y])
    (init-field [speed INIT-WIDGET-SPEED])

    ; is this selected? Default is false.
    (init-field [selected? false]) 

    ;; if this is selected, the position of
    ;; the last button-down event inside this, relative to the
    ;; object's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; register this ball with the wall, and use the result as the
    ;; initial value of wall-pos
    (field [wall-pos (send w register this)])
    
    (super-new)

    ;; Int -> Void
    ;; EFFECT: updates the widget's idea of the wall's position to the
    ;; given integer.
    (define/public (update-wall-pos n)
      (set! wall-pos n))

    ;; after-tick : -> Void
    ;; state of this ball after a tick.  A selected widget doesn't move.
    (define/public (after-tick)
      (if selected?
        this
        (let ((x1     (send this next-x-pos))
              (speed1 (send this next-speed)))
          (begin
            (set! speed speed1)
            (set! x x1)))))

    ;; to be supplied by each subclass
    (abstract next-x-pos)
    (abstract next-speed)

    (define/public (add-to-scene s)
      (place-image
        (send this get-image)
        x y s))

    ;; to be supplied by each subclass
    (abstract get-image)

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in this
    (define/public (after-button-down mx my)
      (if (send this in-this? mx my)
        (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
        this))

    ;; to be supplied by the subclass
    (abstract in-this?)

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; STRATEGY: Cases on whether the event is in this
    ; If this is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (send this in-this? mx my)
        (set! selected? false)
        this))

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the (x, y) location of a drag event
    ; STRATEGY: Cases on whether the ball is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
        this))   

    ;; the widget ignores key events
    (define/public (after-key-event kev) this)
    (define/public (for-test:x)          x)
    (define/public (for-test:speed)      speed)
    (define/public (for-test:wall-pos)   wall-pos)
    (define/public (for-test:next-speed) (next-speed))
    (define/public (for-test:next-x)     (next-x-pos))
    
    ))

;; Hooks left over: these methods must be filled in from subclass.
(define DraggableWidgetHooks<%>
  (interface ()

    ;; Int Int -> Boolean
    ;; is the given location in this widget?
    in-this?

    ;; -> Int
    ;; RETURNS: the next x position or speed of this widget
    next-x-pos
    next-speed

    ;; -> Image
    ;; RETURNS: the image of this widget for display
    get-image

    ))
