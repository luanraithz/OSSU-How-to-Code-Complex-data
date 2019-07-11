;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;  This is the same problem which is inside `exercises` folder, but in a tail recursive way
;  I DIDN'T GET THE EXERCISE FULLY, too much abstraction for a kid

;; Data definitions:

(define-struct wiz (name house kids))
;; Wizard is (make-wiz String String (listof Wizard))
;; interp. A wizard, with name, house and list of children.

(define Wa (make-wiz "A" "S" empty))
(define Wb (make-wiz "B" "G" empty))
(define Wc (make-wiz "C" "R" empty))
(define Wd (make-wiz "D" "H" empty))
(define We (make-wiz "E" "R" empty))
(define Wf (make-wiz "F" "R" (list Wb)))
(define Wg (make-wiz "G" "S" (list Wa)))
(define Wh (make-wiz "H" "S" (list Wc Wd)))
(define Wi (make-wiz "I" "H" empty))
(define Wj (make-wiz "J" "R" (list We Wf Wg)))
(define Wk (make-wiz "K" "G" (list Wh Wi Wj)))


#; ;template, arb-arity-tree, encapsulated w/ local
(define (fn-for-wiz w)          
  (local [(define (fn-for-wiz w)
            (... (wiz-name w)
                 (wiz-house w)
                 (fn-for-low (wiz-kids w))))
          (define (fn-for-low low)
            (cond [(empty? low) (...)]
                  [else
                   (... (fn-for-wiz (first low))
                        (fn-for-low (rest low)))]))]
    (fn-for-wiz w)))


;; Functions:

; 
; PROBLEM:
; 
; Design a function that consumes a wizard and produces the names of every 
; wizard in the tree that was placed in the same house as their immediate
; parent. 
; 


;; Wizard -> (listof String)
;; Produce the names of every descendant in the same house as their parent.
(check-expect (same-house-as-parent Wa) empty)
(check-expect (same-house-as-parent Wh) empty)
(check-expect (same-house-as-parent Wg) (list "A"))
(check-expect (same-house-as-parent Wk) (list "A" "F" "E" ))

; template from Wizard plus lost context accumulator
(define (same-house-as-parent w)
  ;; rsf - Result so far
  ;; todo - list of the remaining
  (local [
          (define-struct wle (w parent))
          (define (fn-for-wiz todo w parent-house rsf)
                (fn-for-low (append
                              (map (λ (k)
                                      (make-wle k (wiz-house w))
                                      ) (wiz-kids w))
                              todo
                              )
                        (if (string=? (wiz-house w) parent-house)
                            (cons (wiz-name w) rsf)
                            rsf)
                        )
                )
          (define (fn-for-low todo rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-wiz
                     (rest todo)
                     (wle-w      (first todo))
                     (wle-parent (first todo))
                     rsf
                     )
                   ]
                  )
            )]
    (fn-for-wiz empty w "" empty)))



; 
; PROBLEM:
; 
; Design a new function definition for same-house-as-parent that is tail 
; recursive. You will need a worklist accumulator.
; 
; 


; 
; PROBLEM:
;
; Design a function that consumes a wizard and produces the number of wizards 
; in that tree (including the root). Your function should be tail recursive.
; 


