(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.0000001)
(define (fixed-point-old f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (sqrtold x)
(fixed-point (average-damp (lambda (y) (/ x y)))
1.0))

(define dx 0.00001)

(define (deriv g)
(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
(lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
(fixed-point (newton-transform g) guess))

(define (sqrt1 x)
(newtons-method
(lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
(fixed-point (transform g) guess))

(define (sqrt x)
(fixed-point-of-transform
(lambda (y) (/ x y)) average-damp 1.0))

(define (cubic a b c) (lambda (x) (+ (cube x)(* a (square x))(* b x) c)))

(define (double f) (lambda (x) (f (f x))))

(define (compose f g) (lambda (x) (f (g x) )))

(define (repeated f n)
  (define (iter facc i)
    (cond ((= i 0) (lambda (x) (facc x)) )
	  (else (iter (lambda (x) (f (facc x))) (- i 1)))))
  (iter f (- n 1)))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smooth f n)
  (lambda (x) ((repeated (smooth f) n) x)))


(define (fast-expt-iter2 base counter acc)
  (cond ((= counter 0) acc)
	((= counter 1) (* base acc))
	((even? counter) (fast-expt-iter2 (square base)
				   (/ counter 2)
				   acc))
	(else (fast-expt-iter2 (square base)
			      (/ (- counter 1) 2)
			      (* base acc)))))


(define (even? n)
  (= (remainder n 2) 0))

(define (exp b n)
  (fast-expt-iter2 b n 1.0))

(define (nrt x n )
(fixed-point-of-transform
(lambda (y) (/ x (exp y (- n 1)))) (repeated average-damp (floor (log-n n 2))) 1.0))

(define (log-n x n)
  (/ (log x) (log n)))

(define (iterative-improve good-enuf? improve init)
  (define (iter good-enuf? improve guess)
    (if (good-enuf? guess (improve guess))
	guess
	(iter good-enuf? improve (improve guess))))
  (iter good-enuf? improve init))



(define (close-enough? v1 v2)
  (< (abs (- v1 v2))
    tolerance))

(define (fixed-point f first-guess)
  (iterative-improve close-enough? f first-guess))

(define (sqrt2 x)
  (iterative-improve close-enough? (lambda (new) (average new (/ x new))) 1.0))
