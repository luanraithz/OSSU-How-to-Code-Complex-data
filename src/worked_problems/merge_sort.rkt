;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require racket/list)


;; (listof Number) (listof Number) -> (listof Number)
;; produces a merged version of two lists ( expects the lists to be ordered )

(check-expect (merge empty (list 2 3)) (list 2 3))
(check-expect (merge (list 2 3) empty) (list 2 3))
(check-expect (merge (list 1 3 4) (list 2 3 )) (list 1 2 3 3 4 ))
(check-expect (merge (list 1) (list 2 3 )) (list 1 2 3 ))

(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(<= (first l1) (first l2)) (cons (first l1) (merge (rest l1)  l2))]
    [else (cons (first l2) (merge l1 (rest l2)))]
))


;; (listof X) -> (listof X)
;; breaks a list in to two

(check-expect (break empty) (list empty empty))
(check-expect (break (list 1 2 3)) (list (list 1) (list 2 3)))

(define (break lst)
  (list (take lst (quotient (length lst ) 2))
        (drop lst (quotient (length lst ) 2))))


;; (listof Number) -> (listof Number)
;; produces a sorted version of a given list

(check-expect (merge-sort (list 1 5 2)) (list 1 2 5))
(check-expect (merge-sort (list 10 1)) (list 1 10))
(check-expect (merge-sort empty) empty)
(check-expect (merge-sort (list 9 2 3 8 7 4 1)) (list 1 2 3 4 7 8 9))


(define (merge-sort l) (cond
                         [(<= (length l) 1) l]
                         [else
                            (local
                                [(define splitted (break l))]
                                (merge (merge-sort (first splitted)) (merge-sort (first (rest splitted)))))]
                        ))
