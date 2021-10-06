;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname how-to-design-data-intervals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; SeatNum is Integar[1,32]
;;            Number[1,32] includes decimals
;;            Natural[1,32) not including 32

;; interp. seat numbers in a row, 1 and 32 are aisle seats

(define SN1 1)  ; aisle
(define SN2 12) ; middle
(define SN3 32) ; aisle

#;
(define (fn-for-seat-num sn)
  (...sn)
  )

;; Template rules used:
;; -atomic, Non-distinct: Integar[1,32]

;; Functions:

;; SeatNum -> Boolean

;; produce true if the given seaat num is on the aisle

(check-expect (aisle? 1) true)
(check-expect (aisle? 19) false)
(check-expect (aisle? 32) true)

;; (define (aisle? sn) false)  ; stub

;; use template from SeatNum

(define (aisle? sn)
  (or (= sn 1 )
      (= sn 32)))