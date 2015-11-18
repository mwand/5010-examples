#lang racket

;; interfaces and other global data

(require 2htdp/image)  ;; for empty-scene

(provide
  CANVAS-WIDTH CANVAS-HEIGHT EMPTY-CANVAS
  INIT-WIDGET-X INIT-WIDGET-Y INIT-WIDGET-SPEED
  INITIAL-WALL-POSITION)

(provide
  StatefulWorld<%>
  Widget<%>
  SWidget<%>
  SWidgetListener<%>
  SWidgetPublisher<%>
  )

;;; CONSTANTS

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


(define INIT-WIDGET-X (/ CANVAS-HEIGHT 2))
(define INIT-WIDGET-Y (/ CANVAS-WIDTH 3))
(define INIT-WIDGET-SPEED 25)

(define INITIAL-WALL-POSITION 300)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A Widget is an object whose class implements Widget<%>
;; An SWidget is an object whose class implements SWidget<%>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES

;; The World implements the StatefulWorld<%> interface

(define StatefulWorld<%>
  (interface ()

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this world to its state after a tick
    after-tick          

    ; Integer Integer MouseEvent-> Void
    ; GIVEN: an (x,y) location
    ; EFFECT: updates this world to the state that should follow the
    ; given mouse event at the given location.
    after-mouse-event

    ; KeyEvent : KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this world to the state that should follow the
    ; given key event
    after-key-event     

    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene

   ; Widget -> Void
   ; GIVEN: A widget
   ; EFFECT: add the given widget to the world
   add-widget

   ; SWidget -> Void
   ; GIVEN: A stateful widget
   ; EFFECT: add the given widget to the world
   add-stateful-widget

    ))


;; Every functional object that lives in the world must implement the
;; Widget<%> interface.

(define Widget<%>
  (interface ()

    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; Integer Integer -> Widget
    ; GIVEN: an (x,y) location
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

;; while we're at it, we'll rename the interfaces to reflect their
;; generic nature.

;; Interface for Ball and other classes that receive messages
;; from the wall:

(define SWidgetListener<%>
  (interface (SWidget<%>)

    ; Int -> Void
    ; EFFECT: updates the ball's cached value of the wall's position
    update-wall-pos

    ))

;; Interface for Wall and any other classes that send message to the
;; listeners: 

(define SWidgetPublisher<%>
  (interface (SWidget<%>)

    ; SWidgetListener<%> -> Int
    ; GIVEN: An SWidgetListener<%>
    ; EFFECT: registers the listener to receive position updates from this wall.
    ; RETURNS: the current x-position of the wall
    register

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
