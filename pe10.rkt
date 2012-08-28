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
  (cond ((= times 0) true)
        ((miller-rabin-test n) (good-prime? n (- times 1)))
        (else false)))

(define (pe10 n)
  (define (sub p s)
    (cond ((> p n) s)
          ((zero? (modulo p 3)) (sub (+ p 2) s))
          ((zero? (modulo p 5)) (sub (+ p 2) s))
          ((zero? (modulo p 7)) (sub (+ p 2) s))
          ((good-prime? p 3)(sub (+ p 2) (+ p s)))
          (else (sub (+ p 2) s))))
  (sub 9 (+ 2 3 5 7)))

(pe10 2000000)
