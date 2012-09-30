#lang scheme
; Miller-Rabin primality test

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (miller-rabin-test n)
  (define (test-iter a b)
    (if (odd? b)
        (or (= (expmod a b n) (- n 1)) (= (expmod a b n) 1))
        (or (test-iter a (/ b 2))
            (= (expmod a (/ b 2) n) (- n 1)))))
  (test-iter (+ 1 (random (- n 1))) (- n 1)))

(define (good-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (good-prime? n (- times 1)))
        (else #f)))

(define (sum_digit n)
  (define (hoge x s)
    (cond ((zero? x) s)
          (else (hoge (floor (/ x 10)) (+ s (modulo x 10))))))
  (hoge n 0))

(define (isharshad? n)
  (cond ((zero? (modulo n (sum_digit n))) #t)
        (else #f)))

(define (tran? n)
  (cond ((zero? n) #t)
        ((isharshad? n)(tran? (floor (/ n 10))))
        (else #f)))

(define (isgood? n)
  (let ((tran (floor (/ n 10))))
    (define (strong? x)
      (let ((p (/ x (sum_digit x))))
      (cond ((= 1 p) #f)
            ((good-prime? p 10) #t)
            (else #f))))
    (cond ((not (tran? tran)) #f)
          ((not (good-prime? n 10)) #f)
          ((not (strong? tran)) #f)
          (else #t))))

(define (pe387 n)
  (define (hoge x s)
    (cond ((> x n) s)
          ((isgood? x) (hoge (+ x 2) (+ s x)))
          (else (hoge (+ x 2) s))))
  (hoge 23 0))

(pe387 10000)
(pe387 (expt 10 14))


