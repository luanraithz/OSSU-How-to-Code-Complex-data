;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/list)

;; fs-starter.rkt (type comments and examples)

;; Data definitions:

(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;         If data is 0, then subs is considered to be list of sub elements.
;;         If data is not 0, then subs is ignored.

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of file system Elements

(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define F3 (make-elt "F3" 3 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))


;; Functions (template):

#; (define (fn-for-element e)
  (...
     (elt-name e) ; String
     (elt-data e) ; Integer
     (fn-for-loe (elt-subs e)) ; ListOfElement
   )
  )

#; (define (fn-for-loe loe)
  (cond [(empty? loe) (...)]
        [else (...
                (fn-for-element (first loe)) ; Element
                (fn-for-loe (rest loe))) ; ListOfElement
        ]))


;; Functions:

;;  Design a function that consumes Element and produces the sum of all the file data in the tree.
;; Count file data

(check-expect (sum-data--element D6) 6)
(check-expect (sum-data--element D5) 3)
(check-expect (sum-data--element D4) 3)
(check-expect (sum-data--element F1) 1)
(check-expect (sum-data--element F2) 2)
(check-expect (sum-data--element F3) 3)

;; ListOfElement -> Integer
(define (sum-data--loe loe)
  (cond [(empty? loe) 0]
        [else (+
                (sum-data--element (first loe)) ; Element
                (sum-data--loe (rest loe))) ; ListOfElement
        ]))


;; Element -> Integer
(define (sum-data--element e)
  (cond [(zero? (elt-data e)) (sum-data--loe (elt-subs e))]
        [else (elt-data e)]
  ))

(check-expect (get-names--element D6) (list "D6" "D4" "F1" "F2" "D5" "F3") )

(check-expect (get-names--element D4) (list "D4" "F1" "F2"))

(define (get-names--element e)
  (cons
     (elt-name e) ; String
     (get-names--loe (elt-subs e)) ; ListOfElement
   )
  )

(define (get-names--loe loe)
  (if (empty? loe) empty 
        (flatten
          (cons
            (get-names--element (first loe))
            (get-names--loe (rest loe)))
           )
         )
 )


;   Design a function that consumes String and Element and looks for a data element with the given name.
; If it finds that element it produces the data, otherwise it produces false.


;   Design a function that consumes Element and produces a list of the names of
; all the elements in the tree. 

(check-expect (look-for-name--element "F1" D6) 1)
(check-expect (look-for-name--element "F1" F1) 1)
(check-expect (look-for-name--element "F7" F1) false)
(check-expect (look-for-name--element "F2" D4) 2)
(check-expect (look-for-name--element "F10" D6) false)
(check-expect (look-for-name--element "F2" D6) 2)
(check-expect (look-for-name--element "F3" D5) 3)
(check-expect (look-for-name--element "D4" D4) 0)


;; Element -> false | Number
(define (look-for-name--element name element)
  (if (equal? (elt-name element) name)
        (elt-data element)
        (look-for-name--loe name (elt-subs element))
  )
)

;; ListOfElement -> false | Number
(define (look-for-name--loe name loe)
  (cond [(empty? loe) false]
        [(number? (look-for-name--element name (first loe))) (look-for-name--element name (first loe))]
        [else (look-for-name--loe name (rest loe))]
        ))

