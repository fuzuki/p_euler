#lang scheme

(define (makelist n)
  (define (sub n l)
    (cond ((zero? n) l)
          (else (sub (- n 1) (cons n l)))))
  (sub n '()))

(define (sum l)
  (define (sub l s)
    (cond ((null? l) s)
          (else (sub (cdr l) (+ s (car l))))))
  (sub l 0))

(define (pe6 n)
  (- (expt (sum (makelist n)) 2) (sum (map (lambda (x)(expt x 2)) (makelist n)))))

(pe6 100)