;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; max-exits-from-starter.rkt
;; Data Definitions: 

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))
(define H2 
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-)) 

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))

(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

(define H5
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C- -E- -D-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

;; template: structural recursion, encapsulate w/ local, tail-recursive w/ worklist, 
;;           context-preserving accumulator what rooms have we already visited

(define (fn-for-house r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)))) ; (... (room-name r))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty))) 


; PROBLEM:
;
; Using the following data definition, design a function that produces the room with the most exits 
; (in the case of a tie you can produce any of the rooms in the tie).
;

;; Room -> Room
;; produces the rooms with the highest number of exits

(check-expect (max-exits-from H1) H1)
(check-expect (room-name (max-exits-from H2)) "B")
(check-expect (room-name (max-exits-from H5)) "B")

#;
(define (max-exits-from r) r)

(define (has-more-or-equal-exits? r1 r2)  (>= (length (room-exits r1)) (length (room-exits r2))))

(define (max-exits-from r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; c is the current room with the most exits
  (local [(define (fn-for-room r todo visited c)
            (cond 
                [(member (room-name r) visited)  (fn-for-lor todo visited c)]
                [else (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited ) (if (has-more-or-equal-exits? r c) r c ))]
                  ))
          (define (fn-for-lor todo visited c)
            (cond [(empty? todo) c]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited c)]))]
    (fn-for-room r0 empty empty (make-room "" empty))))


