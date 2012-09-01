#lang scheme
;too complicated and slow
;rewrite later

(define (factorize n)
  (let ((max (sqrt n)))
     (define (div2 n l)
       (cond ((= n 1) l)
             ((even? n)(div2 (/ n 2) (cons 2 l)))
             (else (sub n 3 l))))
     (define (sub n d l)
       (cond ((> d max) (cons n l))
             ((= n 1) l)
             ((zero? (modulo n d))(sub (/ n d) d (cons d l)))
             (else(sub n (+ d 2) l))))
     (div2 n '())))

(define (cnt l)
  (cond ((null? l) 0)
        (else (+ 1 (cnt (cdr l))))))

(define (group l)
  (define (sub x l out)
    (cond ((null? l) out)
          ((= x (car l)) (sub x (cdr l) (cons (cons x (car out)) (cdr out))))
          (else (sub (car l) (cdr l) (cons (cons (car l) '()) out)))))
  (sub (car l) (cdr l) (cons (cons (car l) '()) '())))

(define (ex l)
  (define (sub l out)
    (cond ((null? l) (cons 1 out))
          (else (sub (cdr l)(map (lambda (x) (* (car l) x)) (cons 1 out))))))
  (sub l '()))

(define (cross l m)
  (define (mul c l)
    (map (lambda (x) (* x c)) l))
  (define (sub l m o)
    (cond ((null? l) o)
          (else (sub (cdr l) m (append (mul (car l) m) o)))))
  (sub l m '()))

(define (divisor n)
  (define (sub l o)
    (cond ((null? l) o)
          (else (sub (cdr l) (cross (car l) o)))))
  (sub (map ex (group (factorize n))) '(1)))

(define (makelist n)
  (define (sub n l)
    (cond ((= n 1) l)
          (else (sub (- n 1) (cons (divisor n) l)))))
  (sub n '()))

(define (sum l)
  (cond ((null? l) 0)
        (else (+ (car l) (sum (cdr l))))))
(define (pow l)
  (cond ((null? l) 1)
        (else (* (car l) (sum (cdr l))))))

(define (abundant? l)
  (cond ((> (sum (cdr l)) (car l)) #t)
        (else #f)))

(define (makeaddlist l)
  (define (ad c l)
    (map (lambda (x)(+ x c)) l))
  (define (sub l1 l2 out)
    (cond ((null? l1) out)
          (else (sub (cdr l1) l2 (append (ad (car l1) l2) out)))))
  (sub l l '()))

(define (multifilter l)
  (cond ((null? l) '())
        (else (cons (car l) (multifilter (filter (lambda (x)(cond ((zero? (modulo x (car l)))#f)
                                                     (else #t))) l))))))
(define (makeintlist n)
  (define (sub n l)
    (cond ((zero? n) l)
          (else (sub (- n 1) (cons n l)))))
  (sub n '()))


(define (in? c l)
  (cond ((null? l) #f)
        ((equal? c (car l)) #t)
        (else (in? c (cdr l)))))

(define (pe23check tgt ad)
  (cond ((null? tgt) '())
        ((in? (car tgt) ad)(pe23check (cdr tgt) ad))
        (else (cons (car tgt) (pe23check (cdr tgt) ad)))))



(define (pe23 n li)
  (let ((alist (map (lambda (l)(car l)) (filter abundant? (makelist n)))))
    (define (ok? c l1 l2)
      (define (inchk c il)
        (cond ((null? il) #f)
              ((> (car il) c) #f)
              ((zero? (- (car il) c)) #t);found!
              (else (inchk c (cdr il)))))
      (cond ((null? l1) #t)
            ((> (car l1) c) #t)
            ((inchk (- c (car l1)) l2) #f);added
            (else (ok? c (cdr l1) l2))))
    (sum (filter (lambda (x)(ok? x alist alist)) li))))

;         (map (lambda (l)(car l)) (filter abundant? (makelist 28123))))))

(define (pe23b n)
  (let ((alist (map (lambda (l)(car l)) (filter abundant? (makelist n)))))
    (define (ok? c l1 l2 out)
      (define (inchk c il)
        (cond ((null? il) #f)
              ((> (car il) c) #f)
              ((zero? (- (car il) c)) #t);found!
              (else (inchk c (cdr il)))))
      (cond ((null? l1) #t)
            ((> (car l1) c) #t)
            ((inchk (- c (car l1)) l2) (cons (car l1)out));added
            (else (ok? c (cdr l1) l2 out))))
    (map (lambda (x)(ok? x alist alist (cons x '()))) alist)))

        
;(cnt (filter abundant? (makelist 28123)))
;(makelist 28123)

(define alist (multifilter (map (lambda (l)(car l)) (filter abundant? (makelist 200)))))
;(cnt alist)

(define (mfil l1 l2)
  (cond ((null? l1) l2)
        (else (mfil (cdr l1) (filter (lambda (x) (not (zero? (modulo x (car l1))))) l2)))))
;(cnt (mfil alist (makeintlist 28123)))
;(cnt (filter (lambda (x) (> (cnt x) 2)) (makelist 28123)))
;(map car (filter abundant? (makelist 20161)))
;(cross alist alist)
;(define ad (filter (lambda (x)(< x 28124)) (makeaddlist alist)))
;(cnt ad)
;(cnt (pe23check alist ad))
;
(pe23 28123 (append alist (mfil alist (makeintlist 28123))))
;(cnt (map (lambda (l)(car l)) (filter abundant? (makelist 28123))))
