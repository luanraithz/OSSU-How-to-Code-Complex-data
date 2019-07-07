;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; PROBLEM:

; Design a function that consumes a list of elements and produces the list
; consisting of only the 1st, 3rd, 5th and so on elements of its input. 
;
;   (skip1 (list "a" "b" "c" "d")) should produce (list "a" "c")
; 

;; (listof X) -> (listof X)
;; produce list consisting of only the 1st, 3rd, 5th ... elements

;; Without accumulator
(check-expect (skip1 empty) empty)
(check-expect (skip1 (list "a" "b" "c" "d")) (list "a" "c"))
(check-expect (skip1 (list 1 2 3 4 5)) (list 1 3 5))
(check-expect (skip1 (list 1 2 3 4 5 6)) (list 1 3 5))

(define (skip1 l) 
  (cond [(empty? l) empty ]
        [(empty? (rest l)) (list (first l))]
        [else
          (cons
            (first l)
            (skip1 (rest (rest l)))
            )]
        ))


;; With accumulator
(check-expect (skip2 empty) empty)
(check-expect (skip2 (list "a" "b" "c" "d")) (list "a" "c"))
(check-expect (skip2 (list 1 2 3 4 5 6)) (list 1 3 5)) 

;(define (skip2 lox) empty);stub


(define (skip2 lox0)
  ;; acc: Natural; 1-based position of (first lox) in lox0
  ;; (skip2 (list "a" "b" "c") 1)
  ;; (skip2 (list     "b" "c") 2)
  ;; (skip2 (list         "c") 3)
  (local [
          (define 
            (skip2 lox acc) 
            (cond 
              [(empty? lox) empty]
              [else 
                (if (odd? acc) 
                  (cons (first lox) (skip2 (rest lox) (add1 acc)))
                  (skip2 (rest lox) (add1 acc))
                  )
                ]
              )
            )
          ]
     (skip2 lox0 1))
)


