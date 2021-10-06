;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HtDF-Itemization) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; CountDown is one of:
;;  - false
;;  - Natural[1,10]
;;  - "complete"

;; interp.
;; false         means countdown has not started
;; Natural[1,10] means countdown is running and how many seconds left
;; "complete"    means countdown is over

(define CD1 false)
(define CD2 10)  ; just started
(define CD3 1)   ; almost over
(define CD4 "complete")

#;
(define (fn-for-countdown c)
  (cond [(false? c) (...)]
        [(and (number? c) (<= 1 c) (<= c 10)) (... c)]
        [else (...)]))


;; Template rules used:
;; One of: 3 cases
;;     Atomic distinct: false
;;     Atomic non-distinct: Natural [1,10]
;;     Atomic distinct: "complete"

;; Functions:
;; Countdown -> Image
;; Produce nice image of current state of countdown

#;
(define (countdown-to-image c) (square 0 "solid" "red"))

(check-expect (countdown-to-image false) (square 0 "solid" "red"))
(check-expect (countdown-to-image 5) (text (number->string 5 ) 24 "black"))
(check-expect (countdown-to-image 0) (text "Happy New Year!!!" 24 "red"))

;; Used template from countdown
(define (countdown-to-image c)
  (cond [(false? c) (square 0 "solid" "red")]
        [(and (number? c) (<= 1 c) (<= c 10))
         (text (number->string c) 24 "black")]
        [else (text "Happy New Year!!!" 24 "red")]))


