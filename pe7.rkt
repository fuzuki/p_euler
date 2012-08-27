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

(define (pe7 n)
  (define (sub n p)
    (cond ((good-prime? p 20)(cond ((zero? (- n 1)) p)
                                   (else (sub (- n 1) (+ p 2)))))
          (else (sub n (+ p 2)))))
  (cond ((= n 1) 2)
        (else (sub (- n 1) 3))))

(pe7 10001)