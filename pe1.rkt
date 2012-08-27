#lang scheme
(define (pe1 x)
  (define (sub x sum)
    (cond ((zero? x) sum)
          ((or (zero? (modulo x 3)) (zero? (modulo x 5)))(sub (- x 1) (+ sum x)))
          (else (sub (- x 1) sum))))
  (sub (- x 1) 0))

(pe1 1000)

