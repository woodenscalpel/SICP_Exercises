(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter prod count max-count)
  (if (> count max-count)
      prod
      (fact-iter (* count prod) (+ count 1) max-count)))

(define (ex1.11 n)
  (if (< n 3) n
      (+ (ex1.11 (- n 1)) (* 2 (ex1.11 (- n 2))) (* 3 (ex1.11 (- n 3))))))


(define (pascal row col)
  (cond ((> col row) 0)
	((or (= col 1)(= col row)) 1)
	(else (+ (pascal (- row 1)(- col 1)) (pascal (- row 1) col)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-expt-iter b counter evenacc oddacc)
  (cond ((= counter 1) (* evenacc oddacc ))
	((= counter 2) (* (square evenacc) oddacc ))
	((even? counter) (fast-expt-iter b
				   (/ counter 2)
				   (square evenacc)
				   oddacc))
	(else (fast-expt-iter b
			      (/ (- counter 1) 2)
			      (square evenacc)
			      (* oddacc evenacc)))))

(list (fast-expt-iter 2 1 2 1)
(fast-expt-iter 2 2 2 1)
(fast-expt-iter 2 3 2 1)
(fast-expt-iter 2 4 2 1)
(fast-expt-iter 2 5 2 1)
(fast-expt-iter 2 6 2 1))

(define (fast-expt-iter2 base counter acc)
  (cond ((= counter 0) acc)
	((= counter 1) (* base acc))
	((even? counter) (fast-expt-iter2 (square base)
				   (/ counter 2)
				   acc))
	(else (fast-expt-iter2 (square base)
			      (/ (- counter 1) 2)
			      (* base acc)))))

(list (fast-expt-iter2 2 1 1)
(fast-expt-iter2 2 2  1)
(fast-expt-iter2 2 3  1)
(fast-expt-iter2 2 4  1)
(fast-expt-iter2 2 5  1)
(fast-expt-iter2 2 6  1))

(define (double x) (+ x x))
(define (half x) (/ x 2))

(define (fast-mult-iter a b acc)
  (cond ((= b 0) 0)
	((= b 1) (+ a acc))
	((even? b) (fast-mult-iter (double a) (half b) acc))
	(else (fast-mult-iter (double a) (half (- b 1)) (+ a acc)))))
