;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)


; I SOLVED THIS PROBLEM IN A VERY BAD WAY
; BUT, IT WORKS AND ANYONE CAN SEE WHAT THE HELL
; IS GOING ON
;
; PROBLEM :

; Each circle is surrounded by circles that are two-fifths smaller. 
; 
; You can build these images using the convenient beside and above functions
; if you make your actual recursive function be one that just produces the
; top leaf shape. You can then rotate that to produce the other three shapes.
; 
; You don't have to use this structure if you are prepared to use more
; complex place-image functions and do some arithmetic. But the approach
; where you use the helper is simpler.
; 
; Include a termination argument for your design.
;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIVIAL-SIZE 5)

(check-expect (circle-fractal TRIVIAL-SIZE) (circle TRIVIAL-SIZE "solid" "blue"))

(define (circles-to-bottom s)
  (if (<= s TRIVIAL-SIZE) (circle s "solid" "blue")
      (local [
              (define next-step (* s STEP))
              (define b (circles-to-bottom next-step))
              (define r (circles-to-right next-step))
              (define l (circles-to-left next-step))
             ]
        (above (beside l (circle s "solid" "blue") r) b)
        )
   )
)

(define (circles-to-left s)
  (if (<= s TRIVIAL-SIZE) (circle s "solid" "blue")
      (local [
              (define next-step (* s STEP))
              (define t (circles-to-top next-step))
              (define b (circles-to-bottom next-step))
              (define l (circles-to-left next-step))
             ]
        (above t (beside l (circle s "solid" "blue")) b)
        )
   )
)

(define (circles-to-top s)
  (if (<= s TRIVIAL-SIZE) (circle s "solid" "blue")
      (local [
              (define next-step (* s STEP))
              (define r (circles-to-right next-step))
              (define t (circles-to-top next-step))
              (define l (circles-to-left next-step))
             ]
        (above t (beside l (circle s "solid" "blue") r))
        )
   )
)


(define (circles-to-right s)
  (if (<= s TRIVIAL-SIZE) (circle s "solid" "blue")
      (local [
              (define b (circles-to-bottom (* s STEP)))
              (define r (circles-to-right (* s STEP)))
              (define c (circles-to-top (* s STEP)))
             ]
        (beside (above c (circle s "solid" "blue") b) r)
        )
   )
)
 
(define (circle-fractal d)
  (if (<= d TRIVIAL-SIZE) (circle d "solid" "blue")
      (local [(define next-size (* d STEP))]
        (beside
         (circles-to-left next-size)
         (above
          (circles-to-top next-size)
          (circle d "solid" "blue")
          (circles-to-bottom next-size)
          )
         (circles-to-right next-size)
         ))
  )
)

(circle-fractal 100)



 



