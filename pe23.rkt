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

(define (abundant? n)
    (cond ((> (sum (dvi n)) n) #t)
          (else #f)))

(define (listdel l1 l2)
  (define (del c l)
    (cond ((null? l) '())
          ((equal? c (car l)) (cdr l))
          (else (cons (car l) (del c (cdr l))))))
  (cond ((null? l2) l1)
        (else (listdel (del (car l2) l1) (cdr l2)))))

(define (pe23 n)
  (define (ladd l1 l2)
    (define (s c l)
      (cond ((null? l) '())
            (else (cons (+ (car l) c) (s c (cdr l))))))
    (cond ((null? l2) '())
          (else (append (s (car l2) l1) (ladd l1 (cdr l2))))))
  (define (makelist n)
    (cond ((zero? n) '())
          ((abundant? n) (cons n (makelist (- n 1))))
          (else (makelist (- n 1)))))
  (let ((al (makelist n)))
    (listdel al (ladd al al))))

(pe23 28123)


