;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LispMe initialization file to be loaded on each start-up
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *none* #n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Type predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (boolean? x)
  (boolean? x))

(define (null? x)
  (null? x))

(define (none? x)
  (none? x))

(define (pair? x)
  (pair? x))

(define (number? x)
  (number? x))

(define (complex? x)
  (complex? x))

(define (real? x)
  (real? x))

(define (integer? x)
  (integer? x))

(define (char? x)
  (char? x))

(define (string? x) 
  (string? x))

(define (symbol? x) 
  (symbol? x))

(define (procedure? x)
  (procedure? x))

(define (continuation? x)
  (continuation? x))

(define (promise? x)
  (promise? x))

(define (input-port? x)
  (input-port? x))

(define (output-port? x)
  (output-port? x))

(define (port? p)
  (or (input-port? p) (output-port? p)))

(define (eof-object? x)
  (eof-object? x))

(define (vector? o)
  (vector? o))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Booleans and comparisons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (not x)
  (not x))

(define (eq? x y)
  (eq? x y))

(define (eqv? x y)
  (eqv? x y))

(define (equal? a b)
  (define (%vec-equal? a b)
    (letrec ((help (lambda (k)
      (if (eq? k -1)
        #t
        (and (equal? (vector-ref a k) (vector-ref b k))
             (help (- k 1)))))))
    (let ((l (vector-length a)))
      (and (eq? l (vector-length b))
           (help (- l 1))))))
  (cond ((eqv? a b) #t)
        ((and (string? a) (string? b))
          (string=? a b))
        ((and (pair? a) (pair? b))
          (and (equal? (car a) (car b))
               (equal? (cdr a) (cdr b))))
        ((and (vector? a) (vector? b))
          (%vec-equal? a b))
        (else #f)))

(define (= x y)
  (eqv? x y))

(define (< x y)
  (< x y))

(define (<= x y)
  (<= x y))

(define (> x y)
  (> x y))

(define (>= x y)
  (>= x y))

(define (max n . l)
  (if (null? l) n
    (let ((m (apply max l)))
      (if (<= n m) m n))))

(define (min n . l)
  (if (null? l) n
    (let ((m (apply min l)))
      (if (<= n m) n m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (+ . r)
  (if (null? r)
    0
   (+ (car r) (apply + (cdr r)))))

(define (- x . r)
  (if (null? r)
    (- x)
    (- x (car r))))

(define (* . r)
  (if (null? r)
    1
    (* (car r) (apply * (cdr r)))))

(define (/ x . r)
  (if 
    (null? r)
    (/ x)
    (/ x (car r))))

(define(quotient x y)
  (quotient x y))

(define (remainder x y)
  (remainder x y))

(define (random n)
  (random n))

(define (sin x)
  (sin x))

(define (cos x)
  (cos x))

(define (tan x)
  (tan x))

(define (sinh x)
  (sinh x))

(define (cosh x)
  (cosh x))

(define (tanh x)
  (tanh x))

(define (asin x)
  (asin x))

(define (acos x)
  (acos x))

(define (atan x . r)
  (if (null? r)
    (atan x)
    (atan x (car r))))

(define (asinh x)
  (asinh x))

(define (acosh x)
  (acosh x))

(define (atanh x)
  (atanh x))

(define (exp x)
  (exp x))

(define (log x)
  (log x))

(define (sqrt x)
  (sqrt x))

(define (floor x)
  (floor x))

(define (ceiling x)
  (ceiling x))

(define (truncate x)
  (truncate x))

(define (round x)
  (round x))

(define (integer x)
  (integer x))

(define (make-rectangular x y)
  (make-rectangular x y))

(define (make-polar x y)
  (make-polar x y))

(define (real-part x)
  (real-part x))

(define (imag-part x)
  (imag-part x))

(define (magnitude x)
  (magnitude x))

(define (angle x)
  (angle x))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))

(define (exact? x)
  #f)

(define (inexact? x)
  #t)

(define (expt x y) 
  (exp (* y (log x))))

(define (log10 x)
  (/ (log x) (log 10)))

(define (modulo a b)
  (let ((r (remainder a b)))
    (if (>= (* r b) 0)
      r
      (+ r b))))

(define (negative? x)
  (< x 0))

(define (positive? x)
  (> x 0))

(define (zero? x)
  (eqv? x 0))

(define (abs n)
  (if (real? n)
    (if (>= n 0) n (- n))
    (magnitude n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bit operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bit-and . r)
  (if (null? r)
    -1
   (bit-and (car r) (apply bit-and (cdr r)))))

(define (bit-or . r)
  (if (null? r)
    0
   (bit-or (car r) (apply bit-or (cdr r)))))

(define (bit-xor . r)
  (if (null? r)
    0 
   (bit-xor (car r) (apply bit-xor (cdr r)))))

(define (bit-not x)
  (bit-not x))

(define (bit-shift x y)
  (bit-shift x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (char->integer c)
  (char->integer c))

(define (integer->char n)
  (integer->char n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Pairs and lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (car x)
  (car x))

(define (cdr x)
  (cdr x))

(define (caar x)
  (caar x))

(define (cadr x)
  (cadr x))

(define (cdar x)
  (cdar x))

(define (cddr x)
  (cddr x))

(define (caaar x)
  (caaar x))

(define (caadr x)
  (caadr x))

(define (cadar x)
  (cadar x))

(define (caddr x)
  (caddr x))

(define (cdaar x)
  (cdaar x))

(define (cdadr x)
  (cdadr x))

(define (cddar x)
  (cddar x))

(define (cdddr x)
  (cdddr x))

(define (cons x y)
  (cons x y))

(define (append . r)
  (if (null? r)
    '()
    (append (car r) (apply append (cdr r)))))

(define (list . x)
  x)

(define (%assoc test)
  (lambda (x ls)
    (letrec ((ahelp (lambda (l)
      (cond ((null? l) #f)
            ((test x (caar l)) (car l))
            (else (ahelp (cdr l)))))))
    (ahelp ls))))

(define (assoc x ls)
  ((%assoc equal?) x ls))

(define (assq x ls)
  ((%assoc eq?) x ls))

(define (assv x ls)
  ((%assoc eqv?) x ls))

(define (%member test)
  (lambda (x ls)
    (letrec ((mhelp (lambda (l)
      (cond ((null? l) #f)
            ((test x (car l)) l)
            (else (mhelp (cdr l)))))))
    (mhelp ls))))

(define (member x ls)
  ((%member equal?) x ls))

(define (memq x ls)
  ((%member eq?) x ls))

(define (memv x ls)
  ((%member eqv?) x ls))

(define (length l)
  (letrec ((iter (lambda (l n)
    (if (null? l)
      n
      (iter (cdr l) (+ 1 n))))))
  (iter l 0)))

(define (list-ref ls n)
  (if (eq? n 0)
    (car ls)
    (list-ref (cdr ls) (- n 1))))

(define (reverse l) 
  (letrec ((rev (lambda (a b)
    (if (null? a)
       b
      (rev (cdr a) (cons (car a) b))))))
  (rev l '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-string n c)
  (make-string n c))

(define (string->list s)
  (string->list s))

(define (list->string s)
  (list->string s))

(define (string-length s)
  (string-length s))

(define (string-ref s k)
  (string-ref s k))

(define (string-set! s k c)
  (string-set! s k c))

(define (string-append . r)
  (if (null? r)
    ""
    (string-append (car r) (apply string-append (cdr r)))))

(define (string=? s1 s2)
  (string=? s1 s2))

(define (substring s n k)
  (substring s n k))

(define (object->string n)
  (object->string n))

(define (string->object n)
  (string->object n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-vector n o)
  (make-vector n o))

(define (vector-length v)
  (vector-length v))

(define (vector-ref v n)
  (vector-ref v n))

(define (vector-set! v n o)
  (vector-set! v n o))

(define (vector . ol)
  (list->vector ol))

(define (vector->list v)
  (vector->list v))

(define (list->vector l)
  (list->vector l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I/O and Ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display x . r)
  (if (null? r)
    (display x)
    (display x (car r))))

(define (write x . r)
  (if (null? r)
    (write x)
    (write x (car r))))

(define (newline . p)
  (if (null? p) (display "#0a")
                (display "#0a" (car p))))

(define (open-input-file f)
  (open-input-file f))

(define (open-output-file f)
  (open-output-file f))

(define (open-append-file f)
  (open-append-file f))

(define (read p)
  (read p))

(define (read-char p)
  (read-char p))

(define (peek-char p)
  (peek-char p))

(define (read-line p)
  (read-line p))

(define (delete-file x) 
  (delete-file x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Control structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (call/cc x)
  (call/cc x))

(define (force x)
  (force x))

(define (apply f a)
  (apply f a))

(define (app f  v . a)
  (apply f (cons v a)))

(define (for-each f l)
  (if (null? l)
    #n
    (begin (f (car l))
           (for-each f (cdr l)))))

(define (map f l)
  (if (null? l) '()
    (cons (f (car l))
          (map f (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-macro (let* expr)
  (if (null? (cadr expr))
    (cons 'begin (cddr expr))
    `(let (,(caadr expr))
       (let* ,(cdadr expr) ,@(cddr expr)))))

(define-macro (when expr)
  (let ((pred (cadr expr))
        (form (cddr expr)))
  `(if ,pred (begin ,@form) #n)))

(define-macro (unless expr)
  (let ((pred (cadr expr))
        (form (cddr expr)))
  `(if ,(not pred) (begin ,@form) #n)))

(define-macro (do expr)
  (let ((bindings     (cadr  expr))
        (test-and-res (caddr expr))
        (body         (cdddr expr)))

    (let ((vars   (map car  bindings))
          (inits  (map cadr bindings))
          (steps  (map (lambda (clause)
                         (if (null? (cddr clause))
                             (car   clause)   
                             (caddr clause)))
                       bindings))
          (test   (car test-and-res))
          (result (cdr test-and-res))
          (loop   (gensym)))
     
      `(letrec ((,loop (lambda ,vars
                         (if ,test
                             ,(if (null? result)
                                  #t `(begin ,@result))
                             (begin 
                               ,@body
                               (,loop ,@steps))))))
         (,loop ,@inits)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (disasm p)
  (disasm p))


