#lang scheme
(define (pfactor x)
  (let ((max (sqrt x)))
    (define (sub x n l)
      (cond ((even? x)(sub (/ x 2) n (cons 2 l)))
            ((> n x) l)
            ((> n max) (cons x l))
            ((zero? (modulo x n))(sub (/ x n) n (cons n l)))
            (else (sub x (+ n 2) l))))
    (sub x 3 '())))

(define (max-num l)
  (define (sub l m)
    (cond ((null? l) m)
          ((> (car l) m)(sub (cdr l)(car l)))
          (else (sub (cdr l) m))))
  (sub (cdr l)(car l)))

(define (pe3 x)
  (max-num (pfactor x)))

(pe3 600851475143)
