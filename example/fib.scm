; Fibonacci numbers

(define (fib n)
	(if (<= n 1)
		1 
		(+ (fib (- n 1))
		      (fib (- n 2)))))
