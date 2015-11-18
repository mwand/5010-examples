#lang racket

(require "interfaces.rkt")

(require "ball.rkt")
(require "flashingball.rkt")
(require "square.rkt")
(require 2htdp/universe)  ; for key=?

(provide WidgetFactory%)

;; The WidgetFactory% class

;; accepts key events and adds SWidgets to the world.

;; gets the world and the wall as init-fields.

;; We have only one of these.  This is called the "singleton pattern"

(define WidgetFactory%
  (class* object% (SWidget<%>)

    (init-field world)  ; the world to which the factory adds balls
    (init-field wall)   ; the wall that the new balls should bounce
                        ; off of.

    (super-new)

    ; KeyEvent -> Void
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "b")
         (send world add-stateful-widget (new Ball% [w wall]))]
         [(key=? kev "f")
         (send world add-stateful-widget (new FlashingBall% [w wall]))]
         [(key=? kev "s")
         (send world add-stateful-widget (new Square% [w wall]))]
         ))

    ;; the Ball Factory has no other behavior

    (define/public (after-tick) this)
    (define/public (after-button-down mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-drag mx my) this)
    (define/public (add-to-scene s) s)

    ))


