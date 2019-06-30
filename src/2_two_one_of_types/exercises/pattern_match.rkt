;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; pattern-match-starter.rkt

; Problem:
; 
; It is often useful to be able to tell whether the first part of a sequence of 
; characters matches a given pattern. In this problem you will design a (somewhat 
; limited) way of doing this.
; 
; Assume the following type comments and examples:


;; =================
;; Data Definitions:

;; 1String is String
;; interp. these are strings only 1 character long
(define 1SA "x")
(define 1SB "2")

;; Pattern is one of:
;;  - empty
;;  - (cons "A" Pattern)
;;  - (cons "N" Pattern)
;; interp.
;;   A pattern describing certain ListOf1String. 
;;  "A" means the corresponding letter must be alphabetic.
;;  "N" means it must be numeric.  For example:
;;      (list "A" "N" "A" "N" "A" "N")
;;   describes Canadian postal codes like:
;;      (list "V" "6" "T" "1" "Z" "4")
(define PATTERN1 (list "A" "N" "A" "N" "A" "N"))

;; ListOf1String is one of:
;;  - empty
;;  - (cons 1String ListOf1String)
;; interp. a list of strings each 1 long
(define LOS1 (list "V" "6" "T" "1" "Z" "4"))

; 
; Now design a function that consumes Pattern and ListOf1String and produces true 
; if the pattern matches the ListOf1String. For example,
; 
; (pattern-match? (list "A" "N" "A" "N" "A" "N")
;                 (list "V" "6" "T" "1" "Z" "4"))
; 
; should produce true. If the ListOf1String is longer than the pattern, but the 
; first part matches the whole pattern produce true. If the ListOf1String is 
; shorter than the Pattern you should produce false.       
; 
; Treat this as the design of a function operating on 2 complex data. After your 
; signature and purpose, you should write out a cross product of type comments 
; table. You should reduce the number of cases in your cond to 4 using the table, 
; and you should also simplify the cond questions using the table.
; 
; You should use the following helper functions in your solution:

;; ==========
;; Functions:

;; 1String -> Boolean
;; produce true if 1s is alphabetic/numeric
(check-expect (alphabetic? " ") false)
(check-expect (alphabetic? "1") false)
(check-expect (alphabetic? "a") true)
(check-expect (numeric? " ") false)
(check-expect (numeric? "1") true)
(check-expect (numeric? "a") false)

(define (alphabetic? 1s) (char-alphabetic? (string-ref 1s 0)))
(define (numeric?    1s) (char-numeric?    (string-ref 1s 0)))


(check-expect (match-char? "N" "1") true)
(check-expect (match-char? "N" "a") false)
(check-expect (match-char? "A" "1") false)
(check-expect (match-char? "A" "a") true)

;; Pattern String -> Boolean
;; Produces true if the string matches the given pattern
(define (match-char? pattern char)
    (cond
        [(alphabetic? char) (string=? "A" pattern)]
        [(numeric? char) (string=? "N" pattern)]
    ))

;; ListOfPattern ListOf1String -> Boolean
;; Produces true if a given pattern matches the given list of string

(define PATTERN2 (list "A" "A" "N" "A" "N" "A" "N"))
(define LOS2 (list "V" "B" "6" "T" "1" "Z" "4"))

(check-expect (pattern-match=? empty LOS1) true)
(check-expect (pattern-match=? PATTERN1 empty) false)
(check-expect (pattern-match=? PATTERN1 LOS1) true)
(check-expect (pattern-match=? PATTERN2 LOS2) true)
(check-expect (pattern-match=? PATTERN2 LOS1) false)
(check-expect (pattern-match=? PATTERN1 LOS2) false)

(define (pattern-match=? pattern listOfString)
    (cond 
        [(empty? pattern) true]
        [(empty? listOfString) false]
        [else
            (and
                (match-char? (first pattern) (first listOfString))
                (pattern-match=? (rest pattern) (rest listOfString))
            )
        ]
    )
)





