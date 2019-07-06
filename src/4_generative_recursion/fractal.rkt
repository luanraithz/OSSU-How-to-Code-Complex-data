;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

(require 2htdp/image)

;; fractals-starter.rkt

; PROBLEM:

; Design a function that consumes a number and produces a Sierpinski
; triangle of that size. Your function should use generative recursion.

;One way to draw a Sierpinski triangle is to:

  
; Note that in the 2nd picture above the inner triangles are drawn in 
; black and slightly smaller just to make them clear. In the real
; Sierpinski triangle they should be in the same color and of side
; length s/2. Also note that the center upside down triangle is not
; an explicit triangle, it is simply formed from the other triangles.


; generative recursion template
#;
(define (genrec-fn d)
  (if (trivial? d)
      (trivial-answer d)
      (... d (genrec-fn (next-problem d)))))


(define CUTOFF 1)

;; Number -> Image
;; produce a Sierpinski Triangle of the given size
(check-expect (stri CUTOFF) (triangle CUTOFF "outline" "red"))
(check-expect (stri (* CUTOFF 2))
              (overlay
                  (triangle (* CUTOFF 2) "outline" "red")
                      (local [(define sub (triangle CUTOFF "outline" "red"))]
                        (above sub (beside sub sub))
                        )
              )
             )

(define (stri s)
  (cond
    [(<= s CUTOFF) (triangle s "outline" "red")] ;; Trivial case no recursion
    [else
     (overlay
      (triangle s "outline" "red")
      (local [(define sub (stri (/ s 2)))]
        (above sub (beside sub sub))
      )
     )
    ]
  )
)


; 
; PROBLEM:
; 
; Design a function to produce a Sierpinski carpet of size s.
; 

(check-expect (scar CUTOFF) (square CUTOFF "outline" "red"))
(check-expect (scar 3)
    (overlay
     (square 3 "outline" "red")
     (local
       [
        (define sub (square 1 "outline" "red"))
        (define blk (square 1 "solid" "white"))
       ]
       (beside
        (above sub sub sub)
        (above sub blk sub)
        (above sub sub sub)
     ))))



(define (scar d)
  (if (<= d CUTOFF) (square d "outline" "red")
      (overlay
       (square d "outline" "red")
        (local
         [
          (define sub (scar (/ d 3)))
          (define blk (square (/ d 3) "solid" "white"))
         ]
           (beside
            (above sub sub sub)
            (above sub blk sub)
            (above sub sub sub)
     )))
  )
)


