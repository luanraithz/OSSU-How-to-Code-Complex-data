;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; Problem:
; 
; Design the function merge. It consumes two lists of numbers, which it assumes are 
; each sorted in ascending order. It produces a single list of all the numbers, 
; also sorted in ascending order. 
; 
; Your solution should explicitly show the cross product of type comments table, 
; filled in with the values in each case. Your final function should have a cond 
; with 3 cases. You can do this simplification using the cross product table by 
; recognizing that there are subtly equal answers. 
; 
; Hint: Think carefully about the values of both lists. You might see a way to 
; change a cell content so that 2 cells have the same value.
; 

(define L1 (list 1 4 5 9 11 15))
(define L2 (list 2 3 6 10 12 16))
(define L3 (list 0 3 7 13 14 19))


(check-expect (merge L1 L2) (list 1 2 3 4 5 6 9 10 11 12 15 16))
(check-expect (merge L1 L3) (list 0 1 3 4 5 7 9 11 13 14 15 19))
(check-expect (merge L2 L3) (list 0 2 3 3 6 7 10 12 13 14 16 19))
(check-expect (merge empty L3) L3)
(check-expect (merge L1 empty) L1)

;; ListOfNumber ListOfNumber -> ListOfNumber
;; produces a merged version of 2 ordered lists of numbers
(define (merge list1 list2)
    (cond
        [(empty? list1) list2]
        [(empty? list2) list1]
        [(< (first list1) (first list2)) (cons (first list1) (merge (rest list1) list2 ))]
        [else (cons (first list2) (merge list1 (rest list2)))]
    )
)
