;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Final Quiz|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Creates a star on MTS wherever mouse is clicked, star rotates and grows every second
;; New mouse click creates a new star and deletes the previous one

;; =================
;; Constants:

(define WIDTH 400)
(define HEIGHT WIDTH)
(define STAR-CLR "purple")

(define ROTATION-SPEED 4)
(define SIZE 1)

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

(define-struct newStar (s r))
;; Star is (make-newStar Natural Natural)
;; Interp. a star with size (star-s), rotated (remainder (star-r) 360) degrees

(define S1 (make-newStar 10 100))
(define S2 (make-newStar 20 (remainder 500 360)))

#;
(define (fn-for-new-star s)
  (...(newStar-s s)
      (newStar-r s)))

;; Template rules used:
;; - compound: 2 fields

;; =================
;; Functions:

;; Star -> Star
;; start the world with (main (make-star 0 0))
;; 
(define (main s)
  (big-bang s                    ; Star
            (on-tick   tock)     ; Star -> Star
            (to-draw   render)   ; Star -> Image
            (on-mouse  restart)))    ; Star Integer Integer MouseEvent -> Star
        

;; Star -> Star
;; produce the next ...
;; !!!
(define (tock s) ...)


;; Star -> Image
;; render ... 
;; !!!
(define (render s) ...)

;; Star Integer Integer MouseEvent -> Star
;; on-mouse ... 
;; !!!
(define (restart s) ...) 