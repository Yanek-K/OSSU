;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname final_project_space_invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))

(define INVADER-SPEED 2)                                    ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define MTS (empty-scene WIDTH HEIGHT))
(define OUTLINE (rectangle WIDTH HEIGHT "outline" "transparent"))

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
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-HEIGHT (- TANK-Y 18))
  


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



(define-struct invader (x y v))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader moves along x and by v pixels per clock tick

(define I1 (make-invader 150 100 2))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT 2))       ;exactly landed, moving left
(define I3 (make-invader 150 (- HEIGHT 10) 2)) ;> landed, moving right
(define I4 (make-invader 200 200 2))


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-v invader)))

;; ListofInvader (ListofI) is one of:
;; - empty
;; - (List Invader Invader)
;; interp. A list of Invaders
 
(define LOI1 empty)
(define LOI2 (list I1))
(define LOI3 (list I2 I1))
(define LOI4 (list I3 I2 I1))
(define LOI5 (cons I4 LOI4))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile) (first lom))
         (fn-for-lom) (rest lom)]))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 41 41))
(define M5 (make-missile 100 100))
(define M6 (make-missile 51 205))
(define M7 (make-missile 200 200))
(define M8 (make-missile 200 250))
 
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListofMissile (ListofM) is one of:
;; - empty
;; - (List Missile Missile)
;; interp. A list of missiles
 
(define LOM1 empty)
(define LOM2 (list M1))
(define LOM2.5 (list M5 M4))
(define LOM3 (list M5 M4 M6))
(define LOM4 (append LOM2 LOM3))
(define LOM5 (cons M7 LOM4))
(define LOM6 (list M8 LOM3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile) (first lom))
         (fn-for-lom) (rest lom)]))

;; Template rules used:
;; one of: 2 cases
;; - atomic distinct: empty
;; - compound (List Missile Missile)
;; - reference: (first lom)
;; - self-reference: (rest lom) is ListofMissile


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game empty LOM4 T1))
(define G5 (make-game LOI5 LOM4 T1))



;; ===================================

;; Functions:


;; * BIG BANG FUNCTIONS

;; WS -> WS
;; start the world with ...
;; 
(define (main ws)
  (big-bang ws                   ; WS
    (on-tick   next-game)     ; WS -> WS
    (to-draw   render)   ; WS -> Image
    ;(stop-when ...)      ; WS -> Boolean
    (on-key    handle-key)))    ; WS KeyEvent -> WS


;; WS -> WS 
;; Produce the next state of the game
;; with tank and missile movement !!!
(check-expect (next-game (make-game empty empty empty)) (make-game empty empty (make-tank (+ 2(/ WIDTH 2)) 1)))

(check-expect (next-game (make-game empty empty T1))
              (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1))) 

(check-expect (next-game (make-game empty LOM2 T1))
              (make-game empty
                         (next-missiles (list (make-missile 150 300)))
                         (make-tank (+ 50 TANK-SPEED) 1)))

(check-expect (next-game (make-game empty (list M5) (make-tank 100 1)))
              (make-game empty
                         (next-missiles (list M5))
                         (make-tank (+ 100 TANK-SPEED) 1)))
 
(check-expect (next-game (make-game empty LOM4 T1))
              (make-game empty
                         (next-missiles LOM4)
                         (make-tank (+ 50 TANK-SPEED) 1)))

(check-expect (next-game (make-game LOI2 LOM4 T1))
              (make-game (new-invaders LOI2)
                         (next-missiles LOM4)
                         (make-tank (+ 50 TANK-SPEED) 1)))

   
;(define (next-game ws) ws)
 
(define (next-game ws)
  (cond [(empty? (game-tank ws))
         (make-game empty
                    empty
                    (next-tank T0))] 
        [else
         (make-game (new-invaders (game-invaders ws))
                    (next-missiles (game-missiles ws))
                    (next-tank (game-tank ws)))]))
   
  

;; WS -> Image
;; render the tank and missile on MTS at correct coordinates
(check-expect (render (make-game empty empty empty)) (place-image empty-image 0 0 MTS))

(check-expect (render (make-game empty empty T0))
              (overlay
               (render-missiles empty)
               (render-tank T0)))

(check-expect (render (make-game empty LOM2 T1))
              (overlay
               (render-missiles LOM2)
               (render-tank T1)))

(check-expect (render (make-game empty (list (make-missile 100 100)) (make-tank 100 1)))
              (overlay
               (render-missiles (list (make-missile 100 100)))
               (render-tank (make-tank 100 1))))

(check-expect (render (make-game empty LOM2.5 T1))
              (overlay
               (render-missiles LOM2.5)
               (render-tank T1)))

(check-expect (render (make-game empty LOM2.5 T1))
              (overlay
               (render-missiles LOM2.5)
               (render-tank T1)))

(check-expect (render (make-game LOI3 LOM2.5 T1))
              (overlay
               (render-invaders LOI3)
               (render-missiles LOM2.5)
               (render-tank T1)))
                             
 
;(define (render ws) ws) ;stub

(define (render ws)
  (overlay
   (render-invaders (game-invaders ws))
   (render-missiles (game-missiles ws))
   (render-tank (game-tank ws))))
 
 
 
;; WS KeyEvent -> WS
;; Move the tank and shoot a missile
(check-expect (handle-key (make-game empty empty T0) "right")
              (make-game empty empty (make-tank 150 1)))         ; move tank left and right

(check-expect (handle-key G1 "left") (make-game empty empty (make-tank 50 -1)))

(check-expect (handle-key (make-game empty empty T2) "right")
              (make-game empty empty T1))

(check-expect (handle-key (make-game empty LOM2 (make-tank 50 1)) " ")
              (make-game empty (cons (make-missile 50 MISSILE-HEIGHT) LOM2) T1))                ; shoot a missile from Tank-x position
  
(check-expect (handle-key (make-game empty LOM2 (make-tank 199 1)) " ")
              (make-game empty (cons (make-missile 199 MISSILE-HEIGHT) LOM2) (make-tank 199 1)))
   
(check-expect (handle-key (make-game empty M2 T1) "up")                             ; do nothing on up click
              (make-game empty M2 T1))

(check-expect (handle-key (make-game empty M2 T1) "right")                          ; do nothing on right click ?
              (make-game empty M2 T1))

(check-expect (handle-key (make-game empty M2 T1)  "left")                           ; do nothing on left click  ?
              (make-game empty M2 T2))
                         
      

;(define (handle-key ws ke) ws) ;stub
 
(define (handle-key ws ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders ws)
                    (fire-missile (game-missiles ws) (tank-x (game-tank ws)) ke)
                    (game-tank ws))]
        [(key=? ke "left")
         (make-game (game-invaders ws)
                    (game-missiles ws)
                    (move-tank (game-tank ws) ke))]
        [(key=? ke "right")
         (make-game (game-invaders ws)
                    (game-missiles ws)
                    (move-tank (game-tank ws) ke))]
        [else ws]))


;; * INVADER FUNCTIONS

;; LOI -> LOI
;; create new invaders if there is no invader in the list
;;                  or if the (last loi) is at 100 (INVADE-RATE) in the Y-direction
 
(check-random (new-invaders empty) (list (make-invader
                                          (+ INVADER-SPEED (random 300))
                                          0
                                          (cond [(< 0.5 (random 2))
                                                 (- INVADER-SPEED)]
                                                [else INVADER-SPEED]))))
 
(check-random (new-invaders LOI2)  (list (make-invader
                                          (+ INVADER-SPEED (random 300))
                                          0
                                          (cond [(< 0.5 (random 2))
                                                 (- INVADER-SPEED)]
                                                [else INVADER-SPEED]))
                                         (make-invader 152 (+ 2 INVADE-RATE) 2)))

(check-random (new-invaders
               (list (make-invader 100 INVADE-RATE INVADER-SPEED)
                     (make-invader 200 200 INVADER-SPEED)))
              (next-invaders
               (list
                (make-invader
                 (random WIDTH)
                 -2
                 (cond [(< 0.5 (random 2))
                        (- INVADER-SPEED)]
                       [else INVADER-SPEED]))
                (make-invader 100 INVADE-RATE INVADER-SPEED)
                (make-invader 200 200 INVADER-SPEED))))
        
;(define (new-invader loi) empty)
 
(define (new-invaders loi)
  (cond [(empty? loi)
         (next-invaders (new-invader loi))]
        [(= INVADE-RATE (invader-y (first loi)))
         (next-invaders (new-invader loi))]
        [else (next-invaders loi)]))

   
 
;; LOI -> Invader
;; create a new invader if the (last loi) has moved INVADE-RATE in the Y-direction
(check-random (new-invader LOI1) (list (make-invader (random 300) -2 INVADER-SPEED)))
(check-random (new-invader LOI2) (list (make-invader (random 300) -2 INVADER-SPEED) I1))
(check-random (new-invader (list I4 I2 I1)) (list (make-invader (random 300) -2 INVADER-SPEED) I4 I2 I1))
  
;(define (new-invader loi) empty) ;stub

(define (new-invader loi)
  (cons (make-invader
         (random 300)
         -2
         (cond [(< 0.5 (random 2))
                (- INVADER-SPEED)]
               [else INVADER-SPEED])) loi))


   
;; LOI -> LOI
;; produce the next list of invader, advancing by invader-x and invader-y speed
;; HAVE TO FIGURE OUT HOW TO MAKE INVADER-RATE WORK -> HELPER FUNCTION ***
(check-expect (next-invaders empty) empty)
(check-expect (next-invaders LOI1) empty)

(check-expect (next-invaders LOI2)
              (list (make-invader 152 102 2)))
 
(check-expect (next-invaders LOI3)
              (list (make-invader 152 (+ 2 HEIGHT) 2)
                    (make-invader 152 102  2)))

(check-expect (next-invaders LOI4)
              (list (make-invader 152 (- HEIGHT 8) INVADER-SPEED)
                    (make-invader 152 (+ 2 HEIGHT) INVADER-SPEED)
                    (make-invader 152 (+ 100 2) INVADER-SPEED)))
                    
;(define (next-invaders loi) empty) ;stub
 
(define (next-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
               (next-invaders (rest loi)))]))


;; Invader -> Invader
;; Move the invader down the screen at a 45 degree angle
;; bounce off edges
(check-expect (next-invader I1) (make-invader (+ 150 INVADER-SPEED) (+ 100 INVADER-SPEED) INVADER-SPEED))
(check-expect (next-invader I2) (make-invader (+ 150 INVADER-SPEED) (+ HEIGHT INVADER-SPEED) INVADER-SPEED))

(check-expect (next-invader (make-invader 300 200 INVADER-SPEED)) (make-invader (- 300 INVADER-SPEED) (+ 200 INVADER-SPEED) (- INVADER-SPEED)))
(check-expect (next-invader (make-invader 0 240   INVADER-SPEED)) (make-invader (+ 0 INVADER-SPEED) (+ 240 INVADER-SPEED) (- INVADER-SPEED)))
 
;(define (next-invader i) empty)
 
(define (next-invader i)
  (cond [(>= (+ (invader-x i) INVADER-SPEED) WIDTH)
         (make-invader (+ (invader-x i) (- (invader-v i)))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- (invader-v i)))]
        [(<= (invader-x i) 0)
         (make-invader (+ (invader-x i) INVADER-SPEED)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- (invader-v i)))]
        (else
         (make-invader (+ (invader-x i) (invader-v i))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-v i)))))

 
;; LOI -> LOI
;; track if any invaders have reached the bottom of the screen
;; GAME OVER

;; LOI -> Image
;; render the list of invaders on the screen
(check-expect (render-invaders LOI1) OUTLINE)
(check-expect (render-invaders LOI2) (place-image INVADER 150 100 OUTLINE))
(check-expect (render-invaders LOI3) (place-image INVADER 150 HEIGHT (place-image INVADER 150 100 OUTLINE)))
(check-expect (render-invaders (list (make-invader 200 200 2)
                            (make-invader 150 (- HEIGHT 10) 2)
                            (make-invader 150 HEIGHT 2)
                            (make-invader 150 100 2)))
              (place-image INVADER 200 200
                           (place-image INVADER 150 (- HEIGHT 10)
                                        (place-image INVADER 150 HEIGHT
                                                     (place-image INVADER 150 100 OUTLINE)))))
 
;(define (render loi) empty-image) ;stub

(define (render-invaders loi)
  (cond [(empty? loi) OUTLINE]
        [else
         (render-invader (first loi)
                         (render (rest loi)))]))
  

;; Invader -> Image
;; render the invader on the screen
(check-expect (render-invader I1 OUTLINE) (place-image INVADER 150 100 OUTLINE))
(check-expect (render-invader I4 OUTLINE) (place-image INVADER 200 200 OUTLINE))

;(define (render-invader i) empty-image) ;stub

(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


   
;; * MISSILE FUNCTIONS
 
;; LOM -> LOM
;; Produce the next list of missile advancing by MISSILE SPEED in the Y direction
;; Delete missile that have left the screen area
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles LOM2) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (next-missiles LOM3) (list (make-missile 100 (- 100 MISSILE-SPEED))
                                         (make-missile 41  (- 41 MISSILE-SPEED))
                                         (make-missile 51  (- 205 MISSILE-SPEED))))

(check-expect (next-missiles LOM4) (list (make-missile 150 (- 300 MISSILE-SPEED))
                                         (make-missile 100 (- 100 MISSILE-SPEED))
                                         (make-missile 41  (- 41 MISSILE-SPEED))
                                         (make-missile 51  (- 205 MISSILE-SPEED))))

;(define (next-missiles lom) lom) ;stub

; <Template from ListofMissile>

(define (next-missiles lom)
  (onscreen-only (tick-missiles lom)))



;; LOM -> LOM
;; Produce LOM containing only Missiles that are onscreen
(check-expect (onscreen-only empty) empty)
(check-expect (onscreen-only (list (make-missile 100 100) (make-missile 100 400)))
              (list (make-missile 100 100) (make-missile 100 400)))
  
;(define (onscreen-only lom) empty) ;stub

;; <Template from LOM>

(define (onscreen-only lom)
  (cond [(empty? lom) empty]
        [else
         (if (onscreen? (first lom))
             (cons (first lom) (onscreen-only (rest lom)))
             (onscreen-only (rest lom)))]))




;; Missile -> Boolean
;; Produce true if missile has not left the top of the screen
(check-expect (onscreen? M1) true)
(check-expect (onscreen? (make-missile 100 490)) true)
(check-expect (onscreen? (make-missile 100 (- HEIGHT 1))) true)
(check-expect (onscreen? (make-missile 100 (- HEIGHT 200))) true)
(check-expect (onscreen? (make-missile 100 (- HEIGHT 10))) true)

;(define (onscreen? m) false) ;stub

(define (onscreen? m)
  (>= (missile-y m) 0))
 


;; LOM -> LOM
;; produce ticked List Of Missiles
(check-expect (tick-missiles empty) empty)
(check-expect (tick-missiles LOM2) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (tick-missiles LOM3) (list (make-missile 100 (- 100 MISSILE-SPEED))
                                         (make-missile 41  (- 41 MISSILE-SPEED))
                                         (make-missile 51  (- 205 MISSILE-SPEED))))

(define (tick-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (tick-missile (first lom)) 
               (tick-missiles (rest lom)))]))



;; Missile -> Missile
;; Move the missile by MISSILE-SPEED in the Y Direction
(check-expect (tick-missile M1) (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (tick-missile M4) (make-missile 41  (- 41 MISSILE-SPEED)))
(check-expect (tick-missile M6) (make-missile 51  (- 205 MISSILE-SPEED)))

;(define (tick-missile m) m)

(define (tick-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))


;; ListofMissile -> Image
;; produce am image of the list of missiles
(check-expect (render-missiles empty) OUTLINE)
(check-expect (render-missiles (list M5))
              (place-image MISSILE 100 100 OUTLINE))

(check-expect (render-missiles LOM2.5)
              (place-image MISSILE 100 100 (place-image MISSILE 41 41 OUTLINE))) 

(check-expect (render-missiles (list (make-missile 150 300) (make-missile 41 41)))
              (place-image MISSILE 150 300 (place-image MISSILE 41 41 OUTLINE)))

(check-expect (render-missiles (list (make-missile 150 300) (make-missile 41 41) (make-missile 100 100)))
              (place-image MISSILE 150 300 (place-image MISSILE 41 41 (place-image MISSILE 100 100 OUTLINE))))


 
;(define (render-missiles lom) empty-image) ;stub

(define (render-missiles lom)
  (cond [(empty? lom) OUTLINE]
        [else
         (render-missile (first lom)
                         (render-missiles (rest lom)))]))

  

;; Missile Image -> Image
;; render the missile on the screen
(check-expect (render-missile M1 OUTLINE) (place-image MISSILE 150 300 OUTLINE))
(check-expect (render-missile M4 OUTLINE) (place-image MISSILE 41 41 OUTLINE))
                
;(define (render-missile m img) m)

(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img)) 
   
  

;; LOM -> LOM
;; Produce a new missile on spacebar
(check-expect (fire-missile LOM1 100       " ")     (cons (make-missile 100 MISSILE-HEIGHT) empty))
(check-expect (fire-missile LOM2 200     " ")      (cons (make-missile 200 MISSILE-HEIGHT) LOM2))
(check-expect (fire-missile LOM2.5 100   "right")   LOM2.5)
   

;(define (fire-missile tank-x ke ) empty)

(define (fire-missile game-missiles tank-x ke )
  (cond [(key=? ke " ") (cons (make-missile tank-x MISSILE-HEIGHT) game-missiles)]
        [else game-missiles]))
   
 

;; * TANK FUNCTIONS

;; WS -> WS
;; produce the next tank
;; Tank starts stationary, and moves left or right on arrow-key
;; Tank will stop when it reaches the edge of the screen 
(check-expect (next-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))                    ; Middle
(check-expect (next-tank T1) (make-tank (+          50 TANK-SPEED) 1))                    ; x = 50, moving right
(check-expect (next-tank T2) (make-tank (-          50 TANK-SPEED) -1))                   ; x = 50, moving left

(check-expect (next-tank (make-tank (- WIDTH 2) 1)) (make-tank WIDTH 1)) ; reaches right edge
(check-expect (next-tank (make-tank 2 -1)) (make-tank 0 -1))                              ; reaches left  edge

(check-expect (next-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))                        ; tries to pass right edge
(check-expect (next-tank (make-tank 0 -1)) (make-tank 0 -1))                              ; tries to pass left edge

;(define (next-tank ws) ws) ; stub

(define (next-tank t)
  (cond [(> (+ (tank-x t) TANK-SPEED) WIDTH) (make-tank WIDTH (tank-dir t))]
        [(< (+ (tank-x t) (* TANK-SPEED (tank-dir t)))     0) (make-tank 0     (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t)))
                    (tank-dir t))]))

 

;; WS -> Image
;; render the tank image on the screen
(check-expect (render-tank T0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) MTS))   ; Middle
(check-expect (render-tank T1) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) MTS))            ; x = 50, moving right
(check-expect (render-tank T2) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) MTS))            ; x = 50, moving left

;(define (render ws) ws) ;stub

(define (render-tank ws)
  (cond [(empty? ws) MTS]
        [else
         (place-image TANK (tank-x ws) (- HEIGHT TANK-HEIGHT/2) MTS)]))



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











































