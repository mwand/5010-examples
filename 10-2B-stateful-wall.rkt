#lang racket

;; 10-2A-ball-and-wall.rkt

;; the world will be an object with exactly two widgets:
;; a ball and a wall.  

;; In this version, the wall is draggable, but the ball is not.
;; The ball should bounce off the wall.

;; The ball is told about the wall when it's created

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")


;; start with (run framerate).  Typically: (run 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


(define INIT-BALL-X (/ CANVAS-HEIGHT 2))
(define INIT-BALL-Y (/ CANVAS-WIDTH 3))
(define INIT-BALL-SPEED 8)

(define INITIAL-WALL-POSITION 300)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A Widget is an object whose class implements Widget<%>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES

;; The World implements the WorldState<%> interface

(define WorldState<%>
  (interface ()

    ; -> WorldState
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          

    ; Integer Integer MouseEvent-> WorldState
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event

    ; KeyEvent : KeyEvent -> WorldState
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     

    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))


;; Every object that lives in the world must implement the Widget<%>
;; interface.

(define Widget<%>
  (interface ()

    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event and a time
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;; Every stable (stateful) object that lives in the world must implement the
;; SWidget<%> interface.

(define SWidget<%>
  (interface ()

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this widget to the state it should have
    ; following a tick.
    after-tick          

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; KeyEvent : KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this widget to the state it should have
    ; following the given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))



;; Additional method for Wall:

(define Wall<%>
  (interface (SWidget<%>)

    ; -> Int
    ; RETURNS: the x-position of the wall
    get-pos

    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-world : -> WorldState
;; RETURNS: a world with a stateful wall, and a ball that knows about
;; the wall.
(define (initial-world)
  (local
    ((define the-wall (new Wall%))
     (define the-ball (new Ball% [w the-wall])))
    (make-world-state
      (list the-ball)
      (list the-wall))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; run : PosReal -> World
; GIVEN: a frame rate, in secs/tick
; EFFECT: runs an initial world at the given frame rate
; RETURNS: the final state of the world
(define (run rate)
  (big-bang (initial-world)
    (on-tick
      (lambda (w) (send w after-tick))
      rate)
    (on-draw
      (lambda (w) (send w to-scene)))
    (on-key
      (lambda (w kev)
        (send w after-key-event kev)))
    (on-mouse
      (lambda (w mx my mev)
        (send w after-mouse-event mx my mev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The World% class

;; The World% class

; ListOfWidget ListOfSWidget -> WorldState
(define (make-world-state objs sobjs)
  (new WorldState% [objs objs][sobjs sobjs]))

(define WorldState%
  (class* object% (WorldState<%>)
    
    (init-field objs)  ; ListOfWidget
    (init-field sobjs)  ; ListOfSWidget

    (super-new)

    ;; apply the fn to both the objs and the sobjs, and return a new
    ;; WorldState% with new values of the objs, and with sobjs updated
    ;; appropriately. 
    ;; fn must satisfy the contracts
    ;; (Widget -> Widget)
    ;; AND (SWidget -> Void)
    (define (process-widgets fn)
      (new WorldState%
        [objs (map fn objs)]
        [sobjs (begin 
                 (for-each fn sobjs)
                 sobjs)]))

    ;; after-tick : -> Void
    ;; Use map on the Widgets in this World; use for-each on the
    ;; stateful widgets

    (define/public (after-tick)
      (new WorldState%
        [objs (map
               (lambda (obj) (send obj after-tick))
               objs)]
        [sobjs (begin
                 (for-each
                  (lambda (obj) (send obj after-tick))
                  sobjs)
                 sobjs)]))

    #;(define/public (after-tick)
      (process-widgets
       (lambda (obj) (send obj after-tick))))
    

    ;; to-scene : -> Scene
    ;; Use HOFC foldr on the Widgets and SWidgets in this World
    ;; Note: the append is inefficient, but clear..
      
    (define/public (to-scene)
      (foldr
        (lambda (obj scene)
          (send obj add-to-scene scene))
        EMPTY-CANVAS
        (append sobjs objs)))

    ;; after-key-event : KeyEvent -> WorldState
    ;; STRATEGY: Pass the KeyEvents on to the objects in the world.

    (define/public (after-key-event kev)
      (process-widgets
        (lambda (obj) (send obj after-key-event kev))))

    ;; world-after-mouse-event : Nat Nat MouseEvent -> WorldState
    ;; STRATGY: Cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mx my)]
        [(mouse=? mev "drag")
         (world-after-drag mx my)]
        [(mouse=? mev "button-up")
         (world-after-button-up mx my)]
        [else this]))

    ;; the next few functions are local functions, not in the interface.

    (define (world-after-button-down mx my)
      (process-widgets
       (lambda (obj) (send obj after-button-down mx my))))
    
     
    (define (world-after-button-up mx my)
      (process-widgets
        (lambda (obj) (send obj after-button-up mx my))))


    (define (world-after-drag mx my)
      (process-widgets
        (lambda (obj) (send obj after-drag mx my))))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Ball% class

;; A Ball is a (new Ball% [x Int][y Int][speed Int][w Wall])

;; In this example, Balls are  draggable

(define Ball%
  (class* object% (Widget<%>)

    (init-field w)  ;; the Wall that the ball should bounce off of

    ;; initial values of x, y (center of ball)
    (init-field [x INIT-BALL-X])
    (init-field [y INIT-BALL-Y])
    (init-field [speed INIT-BALL-SPEED])

    ; is this selected? Default is false.
    (init-field [selected? false]) 

    ;; if this is selected, the position of
    ;; the last button-down event inside this, relative to the
    ;; ball's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])
   
    (field [radius 20])

    (super-new)
    
    ;; after-tick : -> Ball
    ;; state of this ball after a tick.  A selected ball doesn't move.
    (define/public (after-tick)
      (if selected? this
        (new Ball%
          [x (next-x-pos)]
          [y y]
          [speed (next-speed)]
          [selected? selected?]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [w w])))

    ;; -> Integer
    ;; position of the ball at the next tick
    ;; STRATEGY: ask the wall for its position and use that to
    ;; calculate the upper bound for the ball's x position
    (define (next-x-pos)
      (limit-value
        radius
        (+ x speed)
        (- (send w get-pos) radius)))

    ;; Number^3 -> Number
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi]
    (define (limit-value lo val hi)
      (max lo (min val hi)))

    ;; -> Integer
    ;; RETURNS: the velocity of the ball at the next tick
    ;; STRATEGY: if the ball will be at its limit, negate the
    ;; velocity, otherwise return it unchanged
    (define (next-speed)
      (if (or
            (= (next-x-pos) radius)
            (= (next-x-pos) (- (send w get-pos) radius)))
        (- speed)
        speed))

    (define/public (add-to-scene s)
      (place-image
        (circle radius 
          "outline"
          "red")
        x y s))

    ; after-button-down : Integer Integer -> Ball
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in this
    (define/public (after-button-down mx my)
      (if (in-this? mx my)
        (new Ball%
          [x x][y y][speed speed]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)]
          [w w])
        this))

    ;; in-this? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this.
    (define (in-this? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr radius)))

    ; after-button-up : Integer Integer -> Ball
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in this
    ; If this is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-this? mx my)
        (new Ball%
          [x x][y y][speed speed]
          [selected? false]
          [saved-mx 127]
          [saved-my 98]   ; the invariant says that if selected? is
                           ; false, you can put anything here.
          [w w])
        this))

    ; after-drag : Integer Integer -> Ball
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the ball is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (new Ball%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [speed speed]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [w w])
        this))   

    ;; the ball ignores key events
    (define/public (after-key-event kev) this)
    

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The Wall% class

;; A Wall is (new Wall% [pos Integer]
;;                      [saved-mx Integer]
;;                      [selected? Boolean])
;; all these fields have default values.

(define Wall%
  (class* object% (Wall<%>)

    (init-field [pos INITIAL-WALL-POSITION]) ; the x position of the wall

    ; is the wall selected? Default is false.
    (init-field [selected? false]) 

    ;; if the wall is selected, the x position of
    ;; the last button-down event near the wall
    (init-field [saved-mx 0])
       
    (super-new)

    ;; the extra behavior for Wall<%>
    (define/public (get-pos) pos)
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the (x, y) location of a button-down event
    ; EFFECT: if the event is near the wall, make the wall selected.
    ; STRATEGY: Cases on whether the event is near the wall
    (define/public (after-button-down mx my)
      (if (near-wall? mx)
        ;; (new Wall%
        ;;   [pos pos]
        ;;   [selected? true]
        ;;   [saved-mx (- mx pos)])
        (begin
          (set! selected? true)
          (set! saved-mx (- mx pos))
          ;; this
          )
        42))

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes the Wall unselected
    (define/public (after-button-up mx my)
      ;; (new Wall%
      ;;   [pos pos]
      ;;   [selected? false]
      ;;   [saved-mx saved-mx])
      (begin
        (set! selected? false)
        this))

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the wall is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx
    (define/public (after-drag mx my)
      (if selected?
        ;; (new Wall%
        ;;   [pos (- mx saved-mx)]
        ;;   [selected? true]
        ;;   [saved-mx saved-mx])
          (set! pos (- mx saved-mx))
        38))


    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    (define/public (add-to-scene scene)
      (scene+line scene pos 0 pos CANVAS-HEIGHT  "black"))
    
    ;; is mx near the wall?  We arbitrarily say it's near if its
    ;; within 5 pixels.
    (define (near-wall? mx)
      (< (abs (- mx pos)) 5))

    ;; the wall has no other behaviors
    (define/public (after-tick) this)
    (define/public (after-key-event kev) this)

    
 
    ))

