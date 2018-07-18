; Towers of Hanoi
(define (hanoi n a b c)
  (if (eq? n 0) '()
    (append
      (hanoi (- n 1) a c b)
      (list (cons a b))
      (hanoi (- n 1) c b a))))
