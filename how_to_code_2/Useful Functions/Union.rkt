;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Union) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


;; Returns the Union of the given sets
;; Union is set of all elements that appear in either set, no repetitions

(define (union l1 l2)
  (cond [(empty? l1) l2]
        [else
         (cons (first l1)
               (union (rest l1)
                      (remove (first l1) l2)))]))

 
