#lang scheme
;repeat length

(define (in? c l)
  (cond ((null? l) #f)
        ((equal? c (car l)) #t)
        (else (in? c (cdr l)))))
;n/m
(define (repeatlength n m)
  (define (len c l n);caution null?
    (cond ((equal? c (car l)) n)
          (else (len c (cdr l) (+ n 1)))))
  (define (dmod n m l)
    (let ((mod (modulo n m)))
      (cond ((zero? mod) 0)
            ((in? mod l)(len mod l 1))
            (else (dmod (* mod 10) m (cons mod l))))))
  (dmod n m '()))

(define (fracpart n m)
  (define (len c l n);caution null?
    (cond ((equal? c (car l)) n)
          (else (len c (cdr l) (+ n 1)))))
  (define (makefrac ord n f out)
    (cond ((zero? n) out)
          (else (makefrac (* ord 10) (- n 1) (cdr f) (+ (* (car f) ord) out)))))
  (define (makefracstr n f out)
    (cond ((zero? n) (list->string out))
          (else (makefracstr (- n 1) (cdr f)
                             (cons (integer->char (+ (char->integer #\0) (car f))) out)))))
  (define (dmod n m l f)
    (let ((mod (modulo n m))
          (frac (floor (/ n m))))
      (cond ((zero? mod) '())
            ((in? mod l) (makefracstr (len mod l 1) (cons frac f) '()))
            (else (dmod (* mod 10) m (cons mod l) (cons frac f))))))
  (dmod n m '() '()))


(define (pe26 n)
  (define (sub c max maxc)
    (let ((rlen (repeatlength 1 c)))
      (cond ((> c n) maxc)
            ((> rlen max)(sub (+ c 1) rlen c))
            (else (sub (+ c 1) max maxc)))))
  ;(fracpart 1 (sub 1 0 0)))
  (sub 1 0 0))

(pe26 (- 1000 1))

