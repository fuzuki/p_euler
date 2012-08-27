#lang scheme

(define (num2list n)
  (define (sub n l)
    (cond ((zero? n) l)
          (else (sub (floor (/ n 10)) (cons (modulo n 10) l)))))
  (sub n '()))

(define (isParin n)
  (equal? (num2list n) (reverse (num2list n))))

(define (pe4 digit)
  (let ((orig (- (expt 10 digit) 1)))
    (define (sub m n max)
      (cond ((> max (* orig n)) max)
            ((isParin (* n m))(cond ((> max (* n m)) (sub (- m 1) n max))
                                    (else (sub (- m 1) n (* n m)))))
            ((> m n)(sub (- m 1) n max))
            (else (sub orig (- n 1) max))))
    (sub orig orig 0)))

(pe4 3)

