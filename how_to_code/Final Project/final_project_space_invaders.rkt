;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname final_project_space_invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define MTS (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; Functions:

;; WS -> WS
;; start the world with ...
;; 
(define (main ws)
  (big-bang ws                   ; WS
    (on-tick   tock)     ; WS -> WS
    (to-draw   render)   ; WS -> Image
    ;(stop-when ...)      ; WS -> Boolean
    (on-key    move-tank)))    ; WS KeyEvent -> WS

;; WS -> WS
;; produce the next tank
;; Tank starts stationary, and moves left or right on arrow-key
;; Tank will stop when it reaches the edge of the screen 
(check-expect (tock T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))                    ; Middle
(check-expect (tock T1) (make-tank (+          50 TANK-SPEED) 1))                    ; x = 50, moving right
(check-expect (tock T2) (make-tank (-          50 TANK-SPEED) -1))                   ; x = 50, moving left

(check-expect (tock (make-tank (- WIDTH 2) 1)) (make-tank WIDTH 1)) ; reaches right edge
(check-expect (tock (make-tank 2 -1)) (make-tank 0 -1))                              ; reaches left  edge

(check-expect (tock (make-tank WIDTH 1)) (make-tank WIDTH 1))                        ; tries to pass right edge
(check-expect (tock (make-tank 0 -1)) (make-tank 0 -1))                              ; tries to pass left edge

;(define (tock ws) ws) ; stub

(define (tock t)
  (cond [(> (+ (tank-x t) TANK-SPEED) WIDTH) (make-tank WIDTH (tank-dir t))]
        [(< (+ (tank-x t) (* TANK-SPEED (tank-dir t)))     0) (make-tank 0     (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t)))
                    (tank-dir t))]))

 


;; WS -> Image
;; render the tank image on the screen
(check-expect (render T0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) MTS))   ; Middle
(check-expect (render T1) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) MTS))            ; x = 50, moving right
(check-expect (render T2) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) MTS))            ; x = 50, moving left

;(define (render ws) ws) ;stub

(define (render ws)
  (place-image TANK (tank-x ws) (- HEIGHT TANK-HEIGHT/2) MTS))



;; WS -> WS
;; Move the tank left and right on arrow key
(check-expect (move-tank (make-tank (/ WIDTH 2) 1)  "left") (make-tank (/ WIDTH 2) -1))
(check-expect (move-tank T0 "right") T0)
(check-expect (move-tank T1 "left") (make-tank 50 -1))
(check-expect (move-tank T1 "right") T1)

(check-expect (move-tank (make-tank WIDTH 1) "left") (make-tank (- WIDTH TANK-SPEED) -1))

;(define (move-tank ws ke) ws) ;stub

(define (move-tank ws ke)
  (cond
    [(> (+ (tank-x ws) TANK-SPEED) WIDTH)              (make-tank (- WIDTH TANK-SPEED) -1)]
    [(< (+ (tank-x ws) (* TANK-SPEED (tank-dir ws))) 0) (make-tank TANK-SPEED            1)]
    [(key=? ke "left") (make-tank (tank-x ws) -1)]
    [(key=? ke "right") (make-tank (tank-x ws) 1)]))











































