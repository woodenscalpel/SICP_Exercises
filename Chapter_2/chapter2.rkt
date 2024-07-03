#lang sicp

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (add-rat x y)
(make-rat (+ (* (numer x) (denom y))
(* (numer y) (denom x)))
(* (denom x) (denom y))))
(define (sub-rat x y)
(make-rat (- (* (numer x) (denom y))
(* (numer y) (denom x)))
(* (denom x) (denom y))))
(define (mul-rat x y)
(make-rat (* (numer x) (numer y))
(* (denom x) (denom y))))
(define (div-rat x y)
(make-rat (* (numer x) (denom y))
(* (denom x) (numer y))))
(define (equal-rat? x y)
(= (* (numer x) (denom y))
(* (numer y) (denom x))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (negative? x)
  (< x 0))
(define (positive? x)
  (> x 0))


(define (make-rat n d)
  (let ((ng (/ n (gcd n d)))
        (dg (/ d (gcd n d))))
    (if (and (negative? dg) (positive? ng))
        (cons (* -1 ng) (abs dg))
        (cons ng dg))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
(newline)
(display (numer x))
(display "/")
(display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)


(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point x) ( cdr x))

(define (make-segment x y) (cons x y))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (print-point p)
(newline)
(display "(")
(display (x-point p))
(display ",")
(display (y-point p))
(display ")"))

(define (midpoint-segment x)
  (make-point (average (x-point (start-segment x)) (x-point (end-segment x)))
              (average (y-point (start-segment x)) (y-point (end-segment x)))))

(define p1 (make-point 1 2))
(define p2 (make-point 1 5))
(define p3 (make-point 3 2))
(define s1 (make-segment p1 p2))
(define s2 (make-segment p1 p3))
(print-point (midpoint-segment s1))


;(define (make-rectangle left-seg top-seg)(cons left-seg top-seg))
;(define (side-rectangle x)(car x))
;(define (top-rectangle x) (cdr x))

(define (make-rectangle p1 p2 p3)(cons p1 (cons p2 p3)))
(define (side-rectangle x)(make-segment (car x) (cadr x)))
(define (top-rectangle x)(make-segment (car x) (cddr x)))



;(define r1 (make-rectangle s1 s2))
(define r1 (make-rectangle p1 p2 p3))

(define (length segment)
  (sqrt (+
         (square (- (x-point (end-segment segment)) (x-point (start-segment segment))))
         (square (- (y-point (end-segment segment)) (y-point (start-segment segment)))))))

(define (perimeter rect)
  (+ (* 2 (length (side-rectangle rect))) (* 2 (length(top-rectangle rect)))))
  
(define (area rect)
  (* (length (side-rectangle rect)) (length (top-rectangle rect))))

(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z)
  (z (lambda (p q) q)))

(define (cons2.5 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car2.5 x)
  (define (iter n acc)
    (if (not (= (remainder n 2) 0))
        acc
        (iter (/ n 2) (+ 1 acc))))
  (iter x 0))

(define (cdr2.5 x)
  (define (iter n acc)
    (if (not (= (remainder n 3) 0))
        acc
        (iter (/ n 3) (+ 1 acc))))
  (iter x 0))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (mul-interval x y)
(let ((p1 (* (lower-bound x) (lower-bound y)))
      (p2 (* (lower-bound x) (upper-bound y)))
      (p3 (* (upper-bound x) (lower-bound y)))
      (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< (* (lower-bound y)(upper-bound y))0)
      (error "Division Interval Crosses 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))


(define (make-interval a b) (cons a b))
(define (lower-bound y) (car y))
(define (upper-bound y) (cdr y))

(define (make-center-width c w)
(make-interval (- c w) (+ c w)))
(define (center i)
(/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
(/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (+ c (/ p 200)) (- c (/ p 200))))
(define (percent i)
  (* (/ (- (upper-bound i) (lower-bound i)) (center i) ) 100))

(define i1 (make-interval 101 102))
(define i2 (make-interval 88 89))

(define (par1 r1 r2)
(div-interval (mul-interval r1 r2)
(add-interval r1 r2)))
(define (par2 r1 r2)
(let ((one (make-interval 1 1)))
(div-interval
one (add-interval (div-interval one r1)
                  (div-interval one r2)))))


(define (list-ref items n)
(if (= n 0)
(car items)
(list-ref (cdr items) (- n 1))))

(define (list-length items)
(if (null? items)
0
(+ 1 (list-length (cdr items)))))

(define (append list1 list2)
(if (null? list1)
list2
(cons (car list1) (append (cdr list1) list2))))

(define (last-pair list1)
  (if (null? (cdr list1))
      (car list1)
      (last-pair (cdr list1))))

;Ex 2.18
(define (reverse list1)
  (if (null? (cdr list1))
      list1
      (append (reverse (cdr list1)) (list (car list1)) )))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
      
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination coin-list) (car coin-list))
(define (except-first-denomination coin-list) (cdr coin-list))
(define (no-more? coin-list) (null? coin-list))

(define (even? x) (= 0 (remainder x 2)))
(define (odd? x) (= 1 (remainder x 2)))

(define (same-parity-l test-val args)
  (cond ((null? args) (list test-val))
        ((or (and (even? test-val) (even? (car args))) (and (odd? test-val) (odd? (car args))))
          (append (list (car args)) (same-parity-l test-val (cdr args))))
        (else (same-parity-l test-val (cdr args)))))

(define (same-parity test-val . args)
  (same-parity-l test-val args))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (square-list items)
(if (null? items)
nil
(cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
(map square items))

(define (square-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))
                    ))))
  (iter items nil))


;Ex 2.23

(define (for-each function data)
  (map function data)
  true ) ; returns true

;test
(for-each (lambda (x)
            (newline)
            (display x))
          (list 1 2 3))

;Ex 2.24
(list 1 (list 2 (list 3 4)))
;tree drawing irl

;Ex 2.25
(cdaddr (list 1 3 (list 5 7) 9)) ;cadaddr
(caar (list (list 7)))
(cadadr (list 1 (list 2 (list 3( list 4(list 5 (list 6 7))))))) ;cddadadadadr

;Ex 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))

;Ex 2.27
(define (deep-reverse lst)
  (if (list? lst)
      (reverse (map deep-reverse lst))
      lst))

;test
(define test-list (list 1 (list 2 3) 4 (list 5 6)))
(list? test-list)
(deep-reverse test-list)

