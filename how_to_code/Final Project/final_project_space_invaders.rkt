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
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 2)
(define MISSILE-SPEED 20)

(define HIT-RANGE 20)

(define INVADE-RATE 3)                                      ; 1-2 is easy, 3-4 is medium, 5+ is hard

(define MTS (empty-scene WIDTH HEIGHT))
(define OUTLINE
  (rectangle WIDTH HEIGHT "outline" "transparent"))

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
       (fn-for-lom       (game-missiles s))
       (fn-for-tank      (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir is 1

(define T0 (make-tank (/ WIDTH 2) 1))    ; center going right
(define T1 (make-tank         50  1))    ; going right
(define T2 (make-tank         50  -1))   ; going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y v))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader moves along x and by v pixels per clock tick

(define I1 (make-invader 150       100  2))        ; not landed, moving right
(define I2 (make-invader 150    HEIGHT  2))        ; exactly landed, moving left
(define I3 (make-invader 150 (- HEIGHT 10) 2))     ; > landed, moving right
(define I4 (make-invader 200          200  2))

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

(define M1 (make-missile 150 300))                               ; not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ; exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ; > hit U1
(define M4 (make-missile  41  41))
(define M5 (make-missile 100 100))
(define M6 (make-missile  51 205))
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
(define LOM2   (list M1))
(define LOM2.5 (list M5 M4))
(define LOM3   (list M5 M4 M6))
(define LOM4   (append LOM2 LOM3))
(define LOM5   (cons M7 LOM4))
(define LOM6   (list M8 LOM3))

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


(define G0 (make-game empty         empty       T0))
(define G1 (make-game empty         empty       T1))
(define G2 (make-game (list I1)    (list M1)    T1))
(define G3 (make-game (list I2 I1) (list M1 M2) T1))
(define G4 (make-game empty         LOM4        T1))
(define G5 (make-game LOI5          LOM4        T1))


 
;; ===================================

;; Functions:


;; BIG BANG FUNCTIONS

;; WS -> WS
;; start the world with ...
;; 
(define (main ws)
  (big-bang ws                  ; WS
    (on-tick   next-game)       ; WS -> WS
    (to-draw   render)          ; WS -> Image
    (stop-when game-over?)      ; WS -> Boolean
    (on-key    handle-key)))    ; WS KeyEvent -> WS
  
  
;; WS -> WS 
;; Call helper functions to produce the next state of the game
(check-expect (next-game (make-game empty empty empty))                                ; empty game produces a tank moving right from center
              (make-game empty empty (make-tank (+ 2(/ WIDTH 2)) 1)))

(check-random (next-game (make-game empty empty T1))                                 
              (make-game (new-invaders empty) empty (make-tank (+ 50 TANK-SPEED) 1)))  
 
(check-random (next-game (make-game empty LOM2 T1))                                    ; no invaders, one missile
              (make-game (new-invaders empty)
                         (next-missiles (list (make-missile 150 300)))
                         (make-tank (+ 50 TANK-SPEED) 1)))

(check-random (next-game (make-game empty (list M5) (make-tank 100 1)))                ; no invaders, one missile
              (make-game (new-invaders empty)
                         (next-missiles (list M5))
                         (make-tank (+ 100 TANK-SPEED) 1)))
  
(check-random (next-game (make-game empty LOM4 T1))                                    ; no invaders, four missiles
              (make-game (new-invaders empty)
                         (next-missiles LOM4)
                         (make-tank (+ 50 TANK-SPEED) 1)))

(check-random (next-game (make-game (list (make-invader 150 150 2)) LOM2 T1))          ; one invader, one missile
              (make-game (new-invaders (list (make-invader 150 150 2)))
                         (next-missiles LOM2)
                         (make-tank (+ 50 TANK-SPEED) 1)))
    
(check-random (next-game (make-game                                                    ; one invader, one missile, one collision
                          (list (make-invader 198 188 2))
                          (list (make-missile 200 200))
                          T1))
              (make-game empty empty (make-tank 52 1)))
   
(check-random (next-game (make-game                                                    ; three invaders, two missiles, two collisions
                          (list I4 I3 I1)
                          (list M7 (make-missile 150 115))
                          T1))
              (make-game (list (make-invader 152 492 2)) empty (make-tank 52 1)))
 
(check-random (next-game (make-game                                                    ; four invaders, three missiles, three collisions
                          (list (make-invader 150 300 2)
                                I4
                                I3
                                I1)
                          (list M1 M7 (make-missile 150 120))
                          T1))
              (make-game (list (make-invader 152 492 2))
                         empty
                         (make-tank 52 1)))
                          
                                 
;(define (next-game ws) ws)
 
(define (next-game ws)
  (cond [(empty? (game-tank ws))
         (make-game empty
                    empty
                    (next-tank T0))]
        [else
         (make-game (safe-invaders    (new-invaders (game-invaders ws))
                                      (game-missiles ws))
                    (next-missiles    (missed-missiles (game-missiles ws)
                                                       (game-invaders ws)))
                    (next-tank (game-tank ws)))]))
  
   
 
;; WS -> Image
;; render the tank and missile on MTS at correct coordinates
(check-expect (render (make-game empty empty empty)) (place-image empty-image 0 0 MTS))

(check-expect (render (make-game empty empty T0))                                          ; render a tank
              (overlay
               (render-invaders empty)
               (render-missiles empty)
               (render-tank T0)))

(check-expect (render (make-game empty LOM2 T1))                                           ; one missile, one tank
              (overlay
               (render-invaders empty)
               (render-missiles LOM2)
               (render-tank T1)))

(check-expect (render (make-game empty (list (make-missile 100 100)) (make-tank 100 1))) 
              (overlay
               (render-invaders empty)
               (render-missiles (list (make-missile 100 100)))
               (render-tank (make-tank 100 1))))

(check-expect (render (make-game empty LOM2.5 T1))                                         ; two missiles, one tank
              (overlay
               (render-invaders empty)
               (render-missiles LOM2.5)
               (render-tank T1)))

(check-expect (render (make-game LOI3 LOM2.5 T1))                                          ; two invaders, two missiles, one tank
              (overlay
               (render-invaders LOI3)
               (render-missiles LOM2.5)
               (render-tank T1)))

(check-expect (render G5)                                                                  ; two invaders, fours missiles, one tank
              (overlay
               (render-invaders LOI5)
               (render-missiles LOM4)
               (render-tank T1)))
                              
 
;(define (render ws) ws) ;stub 

(define (render ws)
  (overlay
   (render-invaders (game-invaders ws))
   (render-missiles (game-missiles ws))
   (render-tank     (game-tank     ws))))


;; WS -> Boolean
;; Ends the game when an invader has landed on the bottom of MTS
(check-expect (game-over? G0) false)       ; invader not landed
(check-expect (game-over? G1) false)
(check-expect (game-over? G3) true)        ; invader landed - game over

;(define (game-over? ws) false) ;stub

(define (game-over? ws)
  (invader-landed? (game-invaders ws)))
            
              
                 
 
;; WS KeyEvent -> WS
;; Move the tank and shoot a missile
(check-expect (handle-key (make-game empty empty T0) "right")                          ; move tank left and right
              (make-game empty empty (make-tank 150 1)))                              

(check-expect (handle-key G1 "left") (make-game empty empty (make-tank 50 -1)))

(check-expect (handle-key (make-game empty empty T2) "right")
              (make-game empty empty T1))

(check-expect (handle-key (make-game empty LOM2 (make-tank 50 1)) " ")
              (make-game empty (cons (make-missile 50 MISSILE-HEIGHT) LOM2) T1))       ; shoot a missile from Tank-x position
  
(check-expect (handle-key (make-game empty LOM2 (make-tank 199 1)) " ")
              (make-game empty
                         (cons (make-missile 199 MISSILE-HEIGHT) LOM2)
                         (make-tank 199 1)))
   
(check-expect (handle-key (make-game empty M2 T1) "up")                                ; do nothing on up click
              (make-game empty M2 T1))

      
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
 
;; LOI LOM -> LOI 
;; makes a LOI of invaders that have not been hit

(check-expect (safe-invaders empty empty) empty)

(check-expect (safe-invaders (list I1) (list M1))                               ; one invader, one missile - miss
              (list I1))

(check-expect (safe-invaders (list I1) (list (make-missile 150 100)))           ; one invader, one missile - one hit
              empty)


(check-expect (safe-invaders (list I4) (list M7))                               ; one invader, one missile - one hit
              empty)


(check-expect (safe-invaders LOI2 (list M7 (make-missile 150 100)))             ; one invader, two missiles - one hit
              (list I1))

(check-expect (safe-invaders LOI3 (list M7 (make-missile 350 100) M4))          ; two invaders, three missiles - miss
              (list I2 I1))
 
(check-random (safe-invaders (list I2 I3 I1) (list (make-missile 150 100) M4))  ; three invaders, two missiles - one hit
              (list I2 I3))

(check-expect (safe-invaders (list I4 I1 I3) (list M7 (make-missile 150 100)))  ; three invaders, two missiles - two hit
              (list I3))

(check-expect (safe-invaders (list I3 I4 I1) (list M7 (make-missile 150 100)))
              (list I3)) 


;(define (safe-invaders loi lom) empty)

(define (safe-invaders loi lom)
  (cond [(or (empty? loi)
             (empty? lom)) loi]
        [else
         (if (not (invader-hit? (first loi) (first lom)))
             (cons (first loi) (safe-invaders (rest loi) lom))
             (safe-invaders (rest loi) (rest lom)))]))

 
 
;; Invader Missile -> Boolean
;; Make the checks to see if invader has been hit
(check-expect (invader-hit? I1 M1) false)
(check-expect (invader-hit? I4 M7) true)

;(define (invader-hit? i m) false) ;stub

(define (invader-hit? i m) 
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))
  

 
;; LOI -> LOI
;; create new invaders if there is no invader in the list
;; or if a random number (between 0 and 99) is less than INVADE-RATE
 
(check-random (new-invaders empty)                              ; make a new invader with random x coordinate and random direction
              (list (make-invader                     
                     (+ INVADER-SPEED (random 300))
                     2
                     (cond [(< 0.5 (random 2))
                            (- INVADER-SPEED)]
                           [else INVADER-SPEED]))))
   
(check-random (new-invaders
               (list (make-invader 100 5 INVADER-SPEED)
                     (make-invader 200 200 INVADER-SPEED)))
              (list 
               (make-invader 102 7 INVADER-SPEED)
               (make-invader 202 202 INVADER-SPEED)))
         
;(define (new-invader loi) empty) ;stub
 
(define (new-invaders loi)
  (cond [(empty? loi)
         (next-invaders (new-invader loi))]
        [(< (random 100) INVADE-RATE)
         (next-invaders (new-invader loi))]
        [else (next-invaders loi)]))

   
  
;; LOI -> Invader
;; create a new invader with random x coordinate and random direction
(check-random (new-invader LOI1)
              (list
               (make-invader (random 300)
                             0
                             (cond [(< 0.5 (random 2))
                                    (- INVADER-SPEED)]
                                   [else INVADER-SPEED]))))

(check-random (new-invader LOI2)
              (list
               (make-invader (random 300)
                             0
                             (cond [(< 0.5 (random 2))
                                    (- INVADER-SPEED)]
                                   [else INVADER-SPEED])) I1))

(check-random (new-invader (list I4 I2 I1))
              (list (make-invader (random 300)
                                  0
                                  (cond [(< 0.5 (random 2))
                                         (- INVADER-SPEED)]
                                        [else INVADER-SPEED]))
                    I4 I2 I1))
   
;(define (new-invader loi) empty) ;stub

(define (new-invader loi)
  (cons (make-invader
         (random 300)
         0
         (cond [(< 0.5 (random 2))
                (- INVADER-SPEED)]
               [else INVADER-SPEED])) loi))



;; LOI -> LOI
;; produce the next list of invader, advancing by invader-x and invader-y speed
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
(check-expect (next-invader I1)                                                               ; moves through MTS at 45 degrees
              (make-invader (+ 150 INVADER-SPEED) (+ 100 INVADER-SPEED) INVADER-SPEED))
(check-expect (next-invader I2)
              (make-invader (+ 150 INVADER-SPEED) (+ HEIGHT INVADER-SPEED) INVADER-SPEED))

(check-expect (next-invader (make-invader 300 200 INVADER-SPEED))                             ; reaches right edge
              (make-invader (- 300 INVADER-SPEED) (+ 200 INVADER-SPEED) (- INVADER-SPEED)))
(check-expect (next-invader (make-invader 0 240   INVADER-SPEED))
              (make-invader (+ 0 INVADER-SPEED) (+ 240 INVADER-SPEED) (- INVADER-SPEED)))     ; reaches left edge
 
;(define (next-invader i) empty) ;stub
 
(define (next-invader i)
  (cond [(>= (+ (invader-x i) INVADER-SPEED) WIDTH)
         (make-invader (+ (invader-x i) (- (invader-v i)))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- (invader-v i)))]
        [(<= (invader-x i) 0)
         (make-invader (abs (+ (invader-x i) (invader-v i)))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- (invader-v i)))]
        (else
         (make-invader (+ (invader-x i) (invader-v i))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-v i)))))



;; LOI -> Image
;; render the list of invaders on the screen
(check-random (render-invaders LOI1) OUTLINE) 
(check-random (render-invaders LOI2) (place-image INVADER 150 100 OUTLINE))
(check-random (render-invaders LOI3) (place-image INVADER 150 HEIGHT (place-image INVADER 150 100 OUTLINE)))

(check-random (render-invaders (list (make-invader 200 200 2)
                                     (make-invader 150 (- HEIGHT 10) 2)
                                     (make-invader 150 HEIGHT 2)
                                     (make-invader 150 100 2)))
              (place-image INVADER 200 200
                           (place-image INVADER 150 (- HEIGHT 10)
                                        (place-image INVADER 150 HEIGHT
                                                     (place-image INVADER 150 100 OUTLINE)))))
(check-random (render-invaders LOI4)
              (place-image INVADER 150 (- HEIGHT 10)
                           (place-image INVADER 150 HEIGHT
                                        (place-image INVADER 150 100 OUTLINE))))
  
;(define (render loi) empty-image) ;stub 

(define (render-invaders loi)
  (cond [(empty? loi) OUTLINE]
        [else
         (render-invader (first loi)
                         (render-invaders (rest loi)))]))



;; Invader Image -> Image
;; render the invader on the screen
(check-expect (render-invader I1 OUTLINE) (place-image INVADER 150 100 OUTLINE))
(check-expect (render-invader I4 OUTLINE) (place-image INVADER 200 200 OUTLINE))

;(define (render-invader i) empty-image) ;stub

(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))



;; Invader -> Boolean
;; Produces true if the invader has reached the bottom of the MTS
(check-expect (invader-landed? LOI1) false)
(check-expect (invader-landed? LOI2) false)
(check-expect (invader-landed? LOI3) true)
              
;(define (invader-landed loi) false) ;stub

(define (invader-landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (= HEIGHT (invader-y (first loi)))
             true
             (invader-landed? (rest loi)))]))
          
   
   
;; * MISSILE FUNCTIONS

;; LOM LOI -> LOM
;; Makes a list of missiles that have not hit a target
(check-expect (missed-missiles empty empty) empty)

(check-expect (missed-missiles (list M1) (list I1)) (list M1))              ; one missile, one invader  - miss

(check-expect (missed-missiles (list (make-missile 150 100)) (list I1))     ; one missile, one invader  - one hit             
              empty)

(check-expect (missed-missiles (list M7) (list I4)) empty)                  ; one missile, one invader  - one hit

(check-expect (missed-missiles (list M7 (make-missile 150 100)) LOI2)       ; two missiles, one invader - one hit
              (list M7))

(check-expect (missed-missiles                                              ; two missiles, two invaders - two hit
               (list M8 M7)
               (list (make-invader 200 250 2) I4))                           
              empty)

(check-expect (missed-missiles                                              ; two missiles, three invaders - two hit
               (list M4 (make-missile 150 100))
               (list I4 I1 (make-invader 41 41 2)))        
              empty)
  
(check-expect (missed-missiles                                              ; three missiles, three invaders - two hit
               (list M5 M8 M7)
               (list (make-invader 100 100 2) I4 (make-invader 100 20 2)))  
              (list M8))
              


;(define (missed-missiles lom loi) lom) ;stub

(define (missed-missiles lom loi)
  (cond [(or (empty? lom)
             (empty? loi)) lom]
        [else
         (if (empty? (missile-hit-any? (first lom) loi))
             (missed-missiles (rest lom) loi)
             (cons (first lom) (missed-missiles (rest lom) loi)))]))


  
;; Missile LOI -> Missile
;; Returns a Missile if it missed all Invader
(check-expect (missile-hit-any? M1 LOI2) M1)
(check-expect (missile-hit-any? M7 (list I4 I1)) empty)
(check-expect (missile-hit-any? M7 (list I2 I3 I4)) empty)
(check-expect (missile-hit-any? M4 (list I4 I2 I3)) M4)

;(define (missile-hit-any? m loi) false) ;stub

(define (missile-hit-any? m loi)
  (cond [(empty? loi) m]
        [else
         (if (not (missile-hit? m (first loi)))
             (missile-hit-any? m (rest loi))
             empty
             )]))


 
;; Missile Invader -> Boolean
;; Returns true if a Missile is within HIT-RANGE of a Invader
(check-expect (missile-hit? M1 I1) false)
(check-expect (missile-hit? M7 I4) true)

;(define (missile-hit? m i) false) ;stub

(define (missile-hit? m i)
  (and (<= (abs (- (missile-x m) (invader-x i))) HIT-RANGE)
       (<= (abs (- (missile-y m) (invader-y i))) HIT-RANGE)))


 
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

(define (next-missiles lom)
  (onscreen-only (tick-missiles lom)))



;; LOM -> LOM
;; Produce LOM containing only Missiles that are onscreen
(check-expect (onscreen-only empty) empty)
(check-expect (onscreen-only (list (make-missile 100 100) (make-missile 100 400)))
              (list (make-missile 100 100) (make-missile 100 400)))
  
;(define (onscreen-only lom) empty) ;stub

(define (onscreen-only lom)
  (cond [(empty? lom) empty]
        [else
         (if (onscreen? (first lom))
             (cons (first lom) (onscreen-only (rest lom)))
             (onscreen-only (rest lom)))]))



;; Missile -> Boolean
;; Produce true if missile has not left the top of the screen
(check-expect (onscreen? M1) true)
(check-expect (onscreen? (make-missile 100          490))   true)
(check-expect (onscreen? (make-missile 100 (- HEIGHT 1)))   true)
(check-expect (onscreen? (make-missile 100 (- HEIGHT 200))) true)
(check-expect (onscreen? (make-missile 100 (- HEIGHT 10)))  true)

;(define (onscreen? m) false) ;stub

(define (onscreen? m)
  (>= (missile-y m) 0))
 


;; LOM -> LOM
;; produce ticked List Of Missiles
(check-expect (tick-missiles empty) empty)
(check-expect (tick-missiles LOM2) (list (make-missile 150 (- 300 MISSILE-SPEED))))
(check-expect (tick-missiles LOM3) (list (make-missile 100 (- 100 MISSILE-SPEED))
                                         (make-missile 41  (- 41  MISSILE-SPEED))
                                         (make-missile 51  (- 205 MISSILE-SPEED))))

(define (tick-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (tick-missile (first lom)) 
               (tick-missiles (rest lom)))]))



;; Missile -> Missile
;; Move the missile by MISSILE-SPEED in the Y Direction
(check-expect (tick-missile M1) (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (tick-missile M4) (make-missile 41  (- 41  MISSILE-SPEED)))
(check-expect (tick-missile M6) (make-missile 51  (- 205 MISSILE-SPEED)))

;(define (tick-missile m) m) ;stub

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
                
;(define (render-missile m img) m) ;stub

(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img)) 
   
  

;; LOM -> LOM
;; Produce a new missile on spacebar
(check-expect (fire-missile LOM1 100     " ")     (cons (make-missile 100 MISSILE-HEIGHT) empty))
(check-expect (fire-missile LOM2 200     " ")     (cons (make-missile 200 MISSILE-HEIGHT) LOM2))
(check-expect (fire-missile LOM2.5 100   "right")                                         LOM2.5)
   
;(define (fire-missile tank-x ke ) empty) ;stub

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

(check-expect (next-tank (make-tank (- WIDTH 2) 1)) (make-tank WIDTH 1))                  ; reaches right edge
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
(check-expect (render-tank T0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) MTS))            ; Middle
(check-expect (render-tank T1) (place-image TANK         50  (- HEIGHT TANK-HEIGHT/2) MTS))            ; x = 50, moving right
(check-expect (render-tank T2) (place-image TANK         50  (- HEIGHT TANK-HEIGHT/2) MTS))            ; x = 50, moving left

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

