#lang scheme
;Combination Problem
;C(40,20)
;Route is anyway "20 times right and 20 times down"
;rrdddrdr...drddr

(define (fact n)
  (define (sub n f)
    (cond ((<= n 1) f)
          (else (sub (- n 1) (* f n)))))
  (sub n 1))

(define (C n m)
  (/ (fact n) (* (fact m) (fact (- n m)))))

(define (pe15 n)
  (C (* 2 n) n))

(pe15 20)

