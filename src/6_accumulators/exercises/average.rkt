;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; average-starter.rkt

; PROBLEM:
;
; Design a function called average that consumes (listof Number) and produces the
; average of the numbers in the list.
;

; (listof Number) -> Number
; produces the average of items in a given list
(check-expect (average (list 1 3)) 2)
(check-expect (average (list 1 3 5)) 3)
(check-expect (average empty) 0)
(check-expect (average (list 0 0 0)) 0)
(check-expect (average (list 0)) 0)


(define (average l)
  (local [(define (av l c t)
            (cond [(empty? l) (if (zero? c) 0 (/ t c))]
                  [else (av (rest l) (add1 c) (+ t (first l)))]
             ))
          ]
    (av l 0 0)))



