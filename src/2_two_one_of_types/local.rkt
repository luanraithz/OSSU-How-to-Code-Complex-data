;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Local variables

;; (local [ <definition> ...] <expression> )

(local [(define a 1)
        (define b 2)]
  (+ a b))

(define p "inci ")

(local [(define p "accio ")
        (define (fetch n) (string-append p n))]
  (string-append (fetch "portkey" )))

;; Prints "accio portkey" as the local `p` overrides the upper one

