#lang scheme

(define (dvi n)
  (define (sub x l)
    (cond ((> x (/ n 2)) l)
          ((zero? (modulo n x))(sub (+ x 1) (cons x l)))
          (else (sub (+ x 1) l))))
  (sub 2 '(1)))

(define (sum l)
  (define (sub l s)
    (cond ((null? l) s)
          (else (sub (cdr l) (+ (car l) s)))))
  (sub l 0))

(define (amicable? n)
  (let ((pair (sum (dvi n))))
    (cond ((= n pair) #f)
          ((= n (sum (dvi pair))) #t)
          (else #f))))

(define (pe21 n)
  (define (sub x l)
    (cond ((> x n) l)
          ((amicable? x) (sub (+ x 1) (cons x l)))
          (else (sub (+ x 1) l))))
  (sum (sub 1 '())))

(pe21 10000)
