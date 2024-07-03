(define (square x)
  (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p)(square q))
		   (+ (* 2 q p) (square q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      ((display "not prime")
       0)
		    ))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  1)

(define (next-n-primes n x)
  ((display n)
   (if (= n 0)
      (display "done")
      (if(timed-prime-test x)
	 (next-n-primes (- n 1) (+ x 1))
	 (next-n-primes n (+ x 1))))))



(define (sumold term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (identity x) x)
(define (sum-integers a b)
(sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
(define (add-dx x)
(+ x dx))
(* (sum f (+ a (/ dx 2.0)) add-dx b)
dx))

(define (int-simp f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x)
    (+ x (* 2 h)))
  (define odd-terms (* 4.0 (sum f (+ a h) add-2h b)))
  (define even-terms (- (* 2.0 (sum f a add-2h b)) (+ (f a) (f b)) ))
  (* (/ h 3.0) (+ odd-terms even-terms))
  )


(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate-recursive combiner null-value term (next a) next b))
  ))

(define (product term a next b)(accumulate * 1.0 term a next b))

(define (inc2 x) (+ 2 x))
(define (pi-approx steps)
  (* (+ 1 (* 2 steps)) 8.0 (/ (product square 4 inc2 (* 2 steps))(product square 3 inc2 (+ 1 (* 2 steps))))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
  (if (> a b)
      result 
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (pi-func x) (/ (square x)(square (- x 1))))

(define (pi-approx2 steps)
  (* (/ 8.0 (* 2.0 steps)) (product pi-func 4 inc2 (* 2 steps)) ))



(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
  (if (> a b)
      result 
      (if (filter a)
	  (iter (next a) (combiner result (term a)))
	  (iter (next a) result))))
  (iter a null-value))

(define (sum-of-primes a b) (filtered-accumulate prime? + 0 square a inc b))

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
	    (- 1 y)))

(define (f2 x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
  (+ (* x (square a))
    (* y b)
       (* a b))))

(define (search f neg-point pos-point)
  (let ( (midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (average a b) (/ (+ a b) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))


(define (cont-frac n d k)
  (define (iter n d acc current-k)
    (if (> current-k k)
	acc
	(iter n d (+ d (/ n acc)) (+ 1 current-k))))
  (iter n d 0 0))

(define (cont-frac-lambda n d k)
  (define (iter n d acc i)
    (cond ((<= i 0) acc)
	  ((= i k) (iter n
			 d
			 (/ (n k)(d k))
			 (- i 1)))
	  (else
	   (iter n
		 d
		 (/ (n  i) (+ (d i ) acc))
		 (- i 1)))))
  (iter n d 0 k))

(define (d1.38 i)
  (cond ((= (modulo (- i 2) 3) 0) (* 2(+ 1 (/ (- i 2) 3))))
	(else 1)
	 ))

(define (e n)
  (+ 2 (cont-frac-lambda (lambda (i) 1.0) (lambda (i) (d1.38 i)) n)))

(define (d1.39 i) (- (* 2 i) 1))

(define (tan-cf x k)
  (/ (cont-frac-lambda (lambda (i) (* -1 (square x))) (lambda (i) (d1.39 i)) k) (* -1 x)))
