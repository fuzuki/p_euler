#lang scheme
(define (fib m)
  (define (sub b1 b2 l)
    (let ((b3 (+ b1 b2)))
     (cond ((< m b3) l)
          (else (sub b2 b3 (cons b3 l))))))
  (sub 1 2 '(2 1)))

(define (sum l)
  (define (sub l s)
    (cond ((null? l) s)
          (else (sub (cdr l) (+ s (car l))))))
  (sub l 0))

(define (pe2 x)
  (sum (filter even? (fib x))))

(pe2 4000000)
