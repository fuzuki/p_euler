#lang scheme

(define (pe28 n)
  (define (sumcorner x len)
    (+ x (- x len) (- x (* len 2)) (- x (* len 3))))
  (define (sub x len sum)
    (cond ((= x 1) (+ 1 sum))
          (else (sub (- x (* len 4)) (- len 2) (+ sum (sumcorner x len))))))
  (sub (* n n) (- n 1) 0))

(pe28 1001)
         
