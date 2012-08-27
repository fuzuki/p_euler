#lang scheme

(define (makelist n)
  (define (sub n l)
    (cond ((zero? n) l)
          (else (sub (- n 1) (cons n l)))))
  (sub n '()))

;Sieve of Eratosthenes
(define (primelist n)
  (define (sub in out)
    (cond ((null? in) out)
          (else (sub (filter (lambda (x)(not (zero? (modulo x (car in))))) in)
                     (cons (car in) out)))))
  (sub (cdr (makelist n)) '()))

(define (prod l)
  (define (sub l p)
    (cond ((null? l) p)
          (else (sub (cdr l) (* (car l) p)))))
  (sub l 1))

(define (pe5 n)
  (define (sub x)
    (define (subsub c)
      (cond ((< n (expt x c))(expt x (- c 1)))
            (else (subsub (+ c 1)))))
    (subsub 0))
  (prod (map sub (primelist n))))

(pe5 20)
