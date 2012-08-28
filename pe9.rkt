#lang scheme

(define (pe9 n)
  (define (isPythagorean? a b c)
    (if (zero? (- (+ (* a a) (* b b)) (* c c))) #t #f)) 
  (define (sub a b c)
    (define (next a b c)
      (cond ((< (+ b 1) c)(sub a (+ b 1) (- c 1)))
            ((< a b)(sub (+ a 1) (+ a 1) (- n (* (+ a 1) 2))))
            (else #f)))
    (cond ((isPythagorean? a b c)(* a b c))
          (else (next a b c))))
  (sub 1 1 (- n 2)))

(pe9 1000)

