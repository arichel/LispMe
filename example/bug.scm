


(letrec ((fn1 (lambda (x) (fn2 x)))
         (fn2 (lambda (x) (* x x))))
  (fn1 100))


((lambda (fn1)
   ((lambda (fn2) fn2)  (lambda (x) (* x x))))
 (lambda (x) (fn2 x)))
   


(define (fn x) (+ x x))

