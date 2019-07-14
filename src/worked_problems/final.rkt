;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require racket/list)

;  PROBLEM 1:
;  
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people. 
;  
;  Design a data definition for Chirper, including a template that is tail recursive and avoids 
;  cycles. 
;  
;  Then design a function called most-followers which determines which user in a Chirper Network is 
;  followed by the most people.

(define-struct chirper-user (name note verified following))
;; interp. name as user name, note as a description, verified, and a list of people that this user is following

(define C1
  (shared ((-Maria- (make-chirper-user "Maria" "Maria's description" true (list -Maria- -Ruan-)))
           (-Ruan- (make-chirper-user "Ruan" "Ruan's description" false (list -Maria-)))
           (-Mary- (make-chirper-user "Mary" "Mary's description" true (list -Mac- -Linus-)))
           (-John- (make-chirper-user "John" "John's description" false (list -Ruan- -Mac-)))
           (-Mac- (make-chirper-user "Mac" "Mac's description" true empty))
           (-Linus- (make-chirper-user "Linus" "Linus's description" true (list -Maria-)))
           )
    (list -Maria- -Ruan- -Mary- -John- -Mac- -Linus-)
    ))

(define C2
  (shared ((-Maria- (make-chirper-user "Maria" "Maria's description" true (list -Ruan-)))
           (-Ruan- (make-chirper-user "Ruan" "Ruan's description" false (list -Mac- -Maria-)))
           (-Mary- (make-chirper-user "Mary" "Mary's description" true (list -Mac- -Linus-)))
           (-John- (make-chirper-user "John" "John's description" false (list -Ruan- -Mac-)))
           (-Mac- (make-chirper-user "Mac" "Mac's description" true empty))
           (-Linus- (make-chirper-user "Linus" "Linus's description" true (list -Maria-)))
           )
    (list -Maria- -Ruan- -Mary- -John- -Mac- -Linus-)
    ))


(check-expect (max-followers empty) "")
(check-expect (max-followers C1) "Maria")
(check-expect (max-followers C2) "Mac")



;; THIS CODE IS REALLY BAD, A REFACTOR WOULD BE NICE

;; I first thought of writing this structure as a graph, but
;; it does make sense that more than one user has no followers
;; and still following someone, so we wouldn't be able to reach him


;; This part of the code isn't following the recipe either, again, a refactor would be nice

;; AKA a todo to my self that I will never remember

(define (max-followers l)
  ;; acc is the result so far of the given array
  (local [
          (define (max-followers acc)
            (local [(define (max-f acc c)
                (cond [(empty? acc) c]
                      [(< (accumulator-count c) (accumulator-count (first acc))) (max-f (rest acc) (first acc))]
                      [else (max-f (rest acc) c)]
                  ))]
                (max-f acc (make-accumulator "" -1))
              )
            )
        (define (increase-by-one acc c)
          (cond [(empty? acc) empty]
              [else
                (if
                  (eq? (chirper-user-name c) (accumulator-chirper (first acc)))
                  (cons (make-accumulator (chirper-user-name c) (add1 (accumulator-count (first acc)))) (rest acc))
                  (cons (first acc) (increase-by-one (rest acc) c))
                  )
                    ])
        )
        (define (add-to-acc acc c)
          (local [
                  (define (already-in-acc? c acc) (not (false? (findf (lambda (a) (eq? (chirper-user-name c) (accumulator-chirper a))) acc))))
                  (define (add acc1 l)
                    (cond [(empty? l) acc1]
                          [(already-in-acc? (first l) acc1) (add (increase-by-one acc1 (first l)) (rest l))]
                          [else (add (cons (make-accumulator (chirper-user-name (first l)) 1) acc1) (rest l))])
                    )
                ]
          (add acc (chirper-user-following c))
          )
        )

        (define-struct accumulator (chirper count))
        ;; interp chiper as the key of, of a map with the count of each user so far
          (define (fn-for-chirper l acc)
            (cond
              [(empty? l) acc]
              [else (fn-for-chirper (rest l) (add-to-acc acc (first l)))]
              )
            )
          ]
    (accumulator-chirper (max-followers (fn-for-chirper l empty)))
    )
  )



;  PROBLEM 2:
;  
;  In UBC's version of How to Code, there are often more than 800 students taking 
;  the course in any given semester, meaning there are often over 40 Teaching Assistants. 
;  
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write 
;  a program to do it for us! 
;  
;  Below are some data definitions for a simplified version of a TA schedule. There are some 
;  number of slots that must be filled, each represented by a natural number. Each TA is 
;  available for some of these slots, and has a maximum number of shifts they can work. 
;  
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their 
;  maximum shifts. If no such schedules exist, produce false. 
; 
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))

(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 3)) (list (make-assignment SOBA 3)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4)) 
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)

(define (schedule-tas tas slots)
  (local [
          (define (schedule t slts acc)
            (cond 
              [(empty? slts) acc]
              [(empty? t) false]
              [else
                (local
                  [(define av (findf (lambda (t) (can-attend t acc (first slts))) t))]
                      (cond
                        [(false? av) false]
                        [else (schedule t (rest slts) (cons (make-assignment av (first slts)) acc))]
                        )
                      )
                ]
            )
            )
          ]
    (schedule tas slots empty))
)


;; Requiring the native solution wasn't working
(define (findf pr l)
  (cond [(empty? l) false]
        [(pr (first l )) (first l)]
        [else (findf pr (rest l))]
        )
  )

;; TA (listof Assigment) slot -> Boolean
;; produces true if the given TA can attend to a given slot

(check-expect (can-attend SOBA empty 1) true)
(check-expect (can-attend SOBA empty 2) false)
(check-expect (can-attend SOBA (list (make-assignment SOBA 1)) 1) false)

(define (can-attend ta assignments slot)
  (cond
    [(= (assignment-count ta assignments) (ta-max ta)) false]
    [(not (member slot (ta-avail ta))) false]
    [else (can-attend-to-slot ta assignments slot)]
    )
  )



(check-expect (assignment-count SOBA (list (make-assignment SOBA 1))) 1)
(define (assignment-count ta assignments)
  (length (filter (lambda (a) (eq? (assignment-ta a) ta)) assignments))
  )

;; TA (listof Assignment) slot -> Boolean
;; produces true if the given ta didn't attend to any previous requirement of this slot
(define (can-attend-to-slot ta assignments slot)
  (zero? (length (filter (lambda (a) (and (eq? (assignment-ta a) ta) (= slot (assignment-slot a)))) assignments))
  ))

