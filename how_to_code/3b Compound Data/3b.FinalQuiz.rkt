;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3b.FinalQuiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Creates a star on MTS wherever mouse is clicked, star rotates and grows every second
;; New mouse click creates a new star and deletes the previous one

;; =================
;; Constants:

(define WIDTH 400)
(define HEIGHT WIDTH)
(define STAR-CLR "purple")

(define ROTATION-SPEED 1)
(define GROWTH-SPEED 1)

(define SIZE 1)
(define ROTATION 0)

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

(define-struct newStar (s r x y))
;; Star is (make-newStar Natural Natural Integar Integar)
;; Interp. a star with size (star-s), rotated (remainder (star-r) 360) degrees
;;         at position x y in screen pixels

(define S1 (make-newStar 10 100 30 200 ))
(define S2 (make-newStar 20 (remainder 500 360) 50 20))

#;
(define (fn-for-new-star s)
  (...(newStar-s s)
      (newStar-r s)
      (newStar-x s)
      (newStar-y s)))

;; Template rules used:
;; - compound: 4 fields

;; =================
;; Functions:

;; Star -> Star
;; start the world with cmd-r
;; 
(define (main s)
  (big-bang s                          ; Star
            (on-tick   tock)           ; Star -> Star
            (to-draw   render)         ; Star -> Image
            (on-mouse  restart)))      ; Star Integer Integer MouseEvent -> Star
        

;; Star -> Star
;; produce the next star growing by GROWTH-SPEED, rotating by ROTATION-SPEED 
(check-expect (tock (make-newStar 10 100 50 100)) (make-newStar (+ 10 GROWTH-SPEED) (+ 100 ROTATION-SPEED) 50 100))
(check-expect (tock (make-newStar 30 (remainder 553 360) 300 120)) (make-newStar (+ 30 GROWTH-SPEED) (remainder (+ 553 ROTATION-SPEED) 360) 300 120))


; (define (tock s) s) ;stub

; < Took template from newStar > 

(define (tock s)
  (make-newStar (+ (newStar-s s) GROWTH-SPEED)
                (remainder (+ (newStar-r s) ROTATION-SPEED) 360)
                (newStar-x s)
                (newStar-y s)))


;; Star -> Image
;; render the star on MTS at the x and y coordinates of mouse click
(check-expect (render (make-newStar 20 240 40 20))   (place-image (rotate 240                 (star 20 "solid" STAR-CLR)) 40  20  MTS))
(check-expect (render (make-newStar 30 500 200 300)) (place-image (rotate (remainder 500 360) (star 30 "solid" STAR-CLR)) 200 300 MTS))

;(define (render s) s) ;stub

; < Took template from newStar >

(define (render s)
  (place-image (rotate (newStar-r s)
                       (star (newStar-s s) "solid" STAR-CLR))
               (newStar-x s)
               (newStar-y s)
               MTS))



;; Star Integer Integer MouseEvent -> Star
;; on-mouse click, create a new Star at x and y position of mouse 
(check-expect (restart (make-newStar 20 30 500 100) 100 200 "button-down") (make-newStar 0 0 100 200))
(check-expect (restart (make-newStar 30 10 500 (remainder 400 360)) 400 200 "button-down") (make-newStar 0 0 400 200))

(check-expect (restart (make-newStar 40 20 200 300) 50 10 "button-up") (make-newStar 40 20 200 300))

; (define (restart s x y mo) 0) ;stub

; < Took Template from MouseEvent >

(define (restart s x y mo)
  (cond [(mouse=? mo "button-down") (make-newStar 0 0 x y)]
        [else s]))


(main (make-newStar 0 0 200 200))


