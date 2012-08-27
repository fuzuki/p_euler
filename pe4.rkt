#lang scheme

(define (num2list n)
  (define (sub n l)
    (cond ((zero? n) l)
          (else (sub (floor (/ n 10)) (cons (modulo n 10) l)))))
  (sub n '()))

(define (isParin n)
  (equal? (num2list n) (reverse (num2list n))))

(define (max-num l)
  (define (sub l m)
    (cond ((null? l) m)
          ((> (car l) m)(sub (cdr l)(car l)))
          (else (sub (cdr l) m))))
  (sub (cdr l)(car l)))


(define (pe4 digit)
  (let ((orig (- (expt 10 digit) 1)))
    (define (sub m n l)
      (cond ((> (- orig (/ orig 10)) n) l)
            ((isParin (* n m)) (sub (- m 1) n (cons (* n m) l)))
            ((> m n)(sub (- m 1) n l))
            (else (sub orig (- n 1) l))))
    (max-num (sub orig orig '()))))

(pe4 3)

