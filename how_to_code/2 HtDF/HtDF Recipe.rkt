;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |HtDF Recipe|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; RECIPE FOR HtDF

;; Consumes -> Produces (Signature)
;; Number -> Number

;; 1 line description of what the function produces (Purpose)
;; produce 2 times the given number

;; Dummy result of test with correct types (Stub)
;(define (double n) 0)

;; Help us understand what fuctions must do and serve as unit tests (Examples)
;; 1. At least 2
;; 2. Different argument values
;; 3. Code coverage
;; 4. Points of variation in behavior
;; 5. 2 long/ 2 deep
(check-expect (double 3) 6)
(check-expect (double 4.2) 8.4)

;; Function Name and Parameter, "..." means do something (Template or Inventory)
;(define (double n)
;  (...n))


;; (Code the function body)
(define (double n)
  (* n 2))

;; Test and Debug