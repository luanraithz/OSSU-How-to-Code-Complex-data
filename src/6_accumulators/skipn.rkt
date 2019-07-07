;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; skipn-starter.rkt

; 
; PROBLEM:
; 
; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by including the first element of lox, then 
; skipping the next n elements, including an element, skipping the next n 
; and so on.
; 
;  (skipn (list "a" "b" "c" "d" "e" "f") 2) should produce (list "a" "d")


(check-expect (skipn empty 2) empty)
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 0) (list "a" "b" "c" "d" "e" "f"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 2) (list "a" "d"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f" "g") 2) (list "a" "d" "g"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f" "g") 3) (list "a" "e"))

(define (skipn l f)
  ;; acc: Natural; 0-based amount that is missing to skip
  ;; (skipn (list "a" "b" "c" "d" 2) 2) ;; outer call
  ;; (skipn (list "a" "b" "c" "d" 2) 2 0) ;; first to inner function
  ;; (skipn (list     "b" "c" "d" 2) 2 2)
  ;; (skipn (list         "c" "d" 2 ) 2  1)
  ;; (skipn (list         "c" "d" 2 ) 2 0)
  (local [
          (define (skipn l acc)
            (cond 
              [(empty? l) empty]
              [(zero? acc) (cons (first l) (skipn (rest l) f))]
              [else (skipn (rest l) (- acc 1))]
            )
            )
          ]
    (skipn l 0)))

