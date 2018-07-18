;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                   ;;
;;              Initialization file for PicoScheme 0.1                               ;;
;;                                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *nil* (if #f #f))                                  ; define nil as return type

(define call/cc call-with-current-continuation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following macros are due to Peter Norvig, see http://www.norvig.com
;;
(define-macro quasiquote (x)
  (define (constant? exp)
    (if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))
    
  (define (combine-skeletons left right exp)
    (cond
     ((and (constant? left) (constant? right)) 
      (if (and (eqv? (eval left)  (car exp))
               (eqv? (eval right) (cdr exp)))
          (list 'quote exp)
          (list 'quote (cons (eval left) (eval right)))))
     ((null? right) (list 'list left))
     ((and (pair? right) (eq? (car right) 'list))
      (cons 'list (cons left (cdr right))))
     (else (list 'cons left right))))
    
  (define (expand-quasiquote exp nesting)
    (cond
     ((vector? exp)     (list 'apply 'vector (expand-quasiquote
                                              (vector->list exp) nesting)))
     ((not (pair? exp)) (if (constant? exp) exp (list 'quote exp)))
     ((and (eq? (car exp) 'unquote) (= (length exp) 2))
      (if (= nesting 0)
          (second exp)
          (combine-skeletons ''unquote 
                             (expand-quasiquote (cdr exp) (- nesting 1))
                             exp)))
     ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
      (combine-skeletons ''quasiquote 
                         (expand-quasiquote (cdr exp) (+ nesting 1))
                         exp))
     ((and (pair? (car exp))
           (eq? (caar exp) 'unquote-splicing)
           (= (length (car exp)) 2))
      (if (= nesting 0)
          (list 'append (second (first exp))
                (expand-quasiquote (cdr exp) nesting))
          (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
                             (expand-quasiquote (cdr exp) nesting)
                             exp)))
     (else (combine-skeletons (expand-quasiquote (car exp) nesting)
                              (expand-quasiquote (cdr exp) nesting)
                              exp))))
  (expand-quasiquote x 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-macro let (bindings . body) 
  (define (varval v)
    (string->symbol (string-append (symbol->string v) "=")))

  (define (named-let name bindings body)
    ((lambda (new-bindings)
       `(let ,(cons `(,name #f) new-bindings)
          (set! ,name (lambda ,(map first bindings) . ,body))
          (,name . ,(map car  new-bindings))))
     (map (lambda (b) `(,(varval (car b)) ,(cadr b))) bindings)))

  (if (symbol? bindings)
      (named-let bindings (first body) (rest body))
      `((lambda ,(map first bindings) . ,body) . ,(map second bindings))) )

(define-macro let* (bindings . body)
  (if (null? bindings) `((lambda () . ,body))
      `(let (,(first bindings))
         (let* ,(rest bindings) . ,body))))

(define-macro letrec (bindings . body)
  (let ((vars (map first  bindings))
        (vals (map second bindings)))
    `(let ,(map (lambda (var) `(,var #f)) vars)
       ,@(map (lambda (var val) `(set! ,var ,val)) vars vals)
       . ,body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-macro do (bindings test-and-result . body)
  (let ((variables (map first bindings))
        (inits     (map second bindings))
        (steps     (map (lambda (clause)
                          (if (null? (cddr clause))
                              (first clause)   
                              (third clause)))
                        bindings))
        (test   (first test-and-result))
        (result (rest  test-and-result))
        (loop   (gensym)))
    `(letrec ((,loop (lambda ,variables
                       (if ,test
                           ,(if (null? result) #t `(begin . ,result))
                           (begin 
                             ,@body
                             (,loop . ,steps))))))
       (,loop . ,inits)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-macro when   (pred . form) `(if ,pred       (begin . ,form)))
(define-macro unless (pred . form) `(if (not ,pred) (begin . ,form)))

(define-macro assert (pred? . args)
   (let ((cntr 0))
     (cons begin (map (lambda(arg)
                        (if (null? (cdr args))
                            `(unless (,pred? ,arg) ; single argument version:
                               (error (format "assertion ~a for argument ~a failed"
                                              ,(object->string type?) ,(object->string arg))))
                            `(unless (,pred? ,arg)
                               ,(set! cntr (1+ cntr))
                               (error (format "assertion ~a for ~ath argument ~a failed"
                                              ,(object->string pred?) ,cntr ,(object->string arg))))))
                      args))))

(define-macro case   (expr . cases)
  (let ((item (gensym)))
    (define (do-case case)
      (cond ((not (pair? case)) (error "bad syntax in case" case))
            ((eq? (first case) 'else) case)
            (else `((member ,item ',(first case)) . ,(rest case)))))
    `(let ((,item ,expr))
       (cond . ,(map do-case cases)))))

(define-macro if-null? (expr form1 . form2)
  (if (null? form2)
      `(if (null? ,expr) ,form1)
      `(if (null? ,expr) ,form1 ,(car form2))))

(define-macro if-zero? (expr form1 . form2)
  (if (null? form2)
      `(if (zero? ,expr) ,form1)
      `(if (zero? ,expr) ,form1 ,(car form2))))

(define-macro if-eqv? (expr1 expr2 form1 . form2)
  (if (null? form2)
      `(if (eqv? ,expr1 ,expr2) ,form1)
      `(if (eqv? ,expr1 ,expr2) ,form1 ,(car form2))))

(define-macro if-equal? (expr1 expr2 form1 . form2)
  (if (null? form2)
      `(if (equal? ,expr1 ,expr2) ,form1)
      `(if (equal? ,expr1 ,expr2) ,form1 ,(car form2))))
;;
;; Postfix, prefix increment, decrement operators:
(define-macro ++1 (x)  `(begin (set! ,x (1+ ,x)) ,x))
(define-macro --1 (x)  `(begin (set! ,x (1- ,x)) ,x))

(define-macro 1++ (x)  `(let ((y ,x))(set! ,x (1+ ,x)) y))
(define-macro 1-- (x)  `(let ((y ,x))(set! ,x (1- ,x)) y))

(define-macro +=  (x y)`(begin (set! ,x (+ ,x ,y)) ,x))
(define-macro -=  (x y)`(begin (set! ,x (- ,x ,y)) ,x))
;;
;; Read, peek with optional assignment:
(define-macro read! (port . x)
  (cond ((null?    x)      `(read ,port))
        ((symbol? (car x)) `(begin (set! ,(car x) (read ,port)) ,(car x)))
        (else (error "read!" "not a symbol" (car x)))))

(define-macro read-char! (port . x)
  (cond ((null?    x)      `(read-char ,port))
        ((symbol? (car x)) `(begin (set! ,(car x) (read-char ,port)) ,(car x)))
        (else (error "read-char!" "not a symbol" (car x)))))

(define-macro peek-char! (port . x)
  (cond ((null?    x)      `(peek-char ,port))
        ((symbol? (car x)) `(begin (set! ,(car x) (peek-char ,port)) ,(car x)))
        (else (error "peek-char" "not a symbol" (car x)))))
;;
;; Macro to write simple escape procedures
;;
(define-macro with-return proc
  `(call-with-current-continuation
    (lambda (return) ,@proc)))
;;
;; Like list-ref but return optional alternative if list length is smaller then index
;;
(define-macro list-ref? (arglist index . alternative)
  (if (null? alternative)
      `(if (< ,index (length ,arglist))
              (list-ref ,arglist ,index)
              (error 'list-ref-if " - to few arguments in list: " ,arglist))
      `(if (< ,index (length ,arglist))
           (list-ref ,arglist ,index)
           ,(car alternative))))
;;
;; call a procedure with a timer wrapped around, return (result . time) pair
;;
(define-macro with-timer proc `(call-with-timer (lambda () . ,proc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Per R5RS, up to four deep compositions should be defined
;;
(define (caaar  x) (car (caar x)))
(define (caadr  x) (car (cadr x)))
(define (cadar  x) (car (cdar x)))
(define (caddr  x) (car (cddr x)))
(define (cdaar  x) (cdr (caar x)))
(define (cdadr  x) (cdr (cadr x)))
(define (cddar  x) (cdr (cdar x)))
(define (cdddr  x) (cdr (cddr x)))

(define (caaaar x) (caar (caar x)))
(define (caaadr x) (caar (cadr x)))
(define (caadar x) (caar (cdar x)))
(define (caaddr x) (caar (cddr x)))
(define (cadaar x) (cadr (caar x)))
(define (cadadr x) (cadr (cadr x)))
(define (caddar x) (cadr (cdar x)))
(define (cadddr x) (cadr (cddr x)))
(define (cdaaar x) (cdar (caar x)))
(define (cdaadr x) (cdar (cadr x)))
(define (cdadar x) (cdar (cdar x)))
(define (cdaddr x) (cdar (cddr x)))
(define (cddaar x) (cddr (caar x)))
(define (cddadr x) (cddr (cadr x)))
(define (cdddar x) (cddr (cdar x)))
(define (cddddr x) (cddr (cddr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define-struct - macro from 'Teach Yourself Scheme in Fixnum Days' by Dorai Sitram
;;                 to define a structure data type and procedures for modifying
;;                 and accessing its fields.
;;
;; example:   (define-struct person height weight (age 0))
;;
;;            => make-person, person.height, person.weight, person.age
;;               set!person.height, ...
;;
(define-macro define-struct (name . prop)
  (let ((s-s (symbol->string name))
        (n (length prop)))

    (let* ((n+1 (1+ n))
           (vv (make-vector n+1)))

      (let loop ((i 1) (prop prop))
        (if (<= i n)
            (let ((f (car prop)))
              (vector-set! vv i (if (pair? f) (cadr f) '(if #f #f)))
              (loop (1+ i) (cdr prop)))))

      (let ((prop (map (lambda (f) (if (pair? f) (car f) f)) prop)))
        `(begin
           (define ,(string->symbol (string-append "make-" s-s))
             (lambda fvfv
               (let ((st (make-vector ,n+1)) (prop ',prop))
                 (vector-set! st 0 ',name)
                 ,@(let loop ((i 1) (r '()))
                     (if (>= i n+1) r
                         (loop (1+ i) (cons `(vector-set! st ,i ,(vector-ref vv i)) r))))
                 (let loop ((fvfv fvfv))
                   (if (not (null? fvfv))
                       (begin
                         (vector-set! st (+ (list-position (car fvfv) prop) 1) (cadr fvfv))
                         (loop (cddr fvfv)))))
                   st)))

           ,@(let loop ((i 1) (procs '()))
               (if (>= i n+1) procs
                   (loop (1+ i)
                         (let ((f (symbol->string (list-ref prop (1- i)))))
                           (cons
                            `(define ,(string->symbol (string-append s-s "." f))
                               (lambda (x) (vector-ref x ,i)))
                            (cons
                             `(define ,(string->symbol (string-append "set!" s-s "." f))
                                (lambda (x v) 
                                  (vector-set! x ,i v)))
                             procs))))))

           (define ,(string->symbol (string-append s-s "?"))
             (lambda (x)
               (and (vector? x) (eqv? (vector-ref x 0) ',name)))) ',name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic format string support, author Scott G. Miller
;;
;; Syntax: (format format-string [obj ...]) -> string
;;
;; Escape sequence:
;;  ~a - The corresponding value is inserted into the string as if printed with display.
;;  ~s - The corresponding value is inserted into the string as if printed with write.
;;  ~% - A newline is inserted.
;;  ~~ - A tilde '~' is inserted.
;;
;; Example: (format "This is a list: ~s~%" '(one "two" 3))
;;
(define (format format-string . objects)
    (let ((buffer (open-output-string)))
      (let loop ((format-list (string->list format-string))
                 (objects objects))
        (cond ((null? format-list) (get-output-string buffer))
              ((char=? (car format-list) #\~)
               (if (null? (cdr format-list))
                   (error 'format " - incomplete escape sequence")
                   (case (cadr format-list)
                     ((#\a)
                      (if (null? objects)
                          (error 'format " - no value for escape sequence")
                          (begin
                            (display (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
                     ((#\s)
                      (if (null? objects)
                          (error 'format " - no value for escape sequence")
                          (begin
                            (write (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
                     ((#\%)
                      (newline buffer)
                      (loop (cddr format-list) objects))
                     ((#\~)
                      (write-char #\~ buffer)
                      (loop (cddr format-list) objects))
                     (else
                      (error 'format " - unrecognized escape sequence")))))
              (else (write-char (car format-list) buffer)
                    (loop (cdr format-list) objects))))))

(define-macro print (fmt . vals) `(display (format ,fmt ,@vals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; matrix, vector utilities
;;
;; make-matrix creates a matrix
(define (make-matrix nrow ncol val)
  (do ((mat (make-vector nrow))
       (i 0 (1+ i)))
      ((= i nrow) mat)
    (vector-set! mat i (make-vector ncol val))))
;;
;; matrix? checks to see if its argument is a matrix.
(define (matrix? x)
  (and (vector? x) (> (vector-length x) 0) (vector? (vector-ref x 0))))
;;
;; matrix-rows returns the number of rows in a matrix
(define (matrix-nrow mat) (vector-length mat))
;;
;; matrix-columns returns the number of columns in a matrix
(define (matrix-ncol mat) (vector-length (vector-ref mat 0)))
;;
;; matrix-ref returns th jth element of the ith row
(define (matrix-ref  mat icol irow) (vector-ref (vector-ref mat icol) irow))
;;
;; matrix-set! changes the jth element of the ith row
(define (matrix-set! mat icol irow val)(vector-set! (vector-ref mat icol) irow val))
;;
;; matrix-row return a copy of ith matrix row
(define (matrix-row mat irow)(vector-copy (vector-ref mat irow)))
;;
;; matrix-copy 
(define (matrix-copy mat)
  (let ((nrow (matrix-nrow mat)))
    (do ((res (make-vector nrow))
         (i 0 (1+ i)))
        ((= i nrow) res)
      (vector-set! res i (vector-copy (vector-ref mat i))) )))
;;
;; matrix-col return a copy of jth matrix column
(define (matrix-col mat icol)
  (let ((nrow (matrix-nrow mat)))
    (do ((i 0 (1+ i))
         (res (make-vector (vector-length mat))))
        ((= i nrow) res)
      (vector-set! res i (matrix-ref mat i icol)) )))
;;
;; axpy - scalar * vector1 + vector2 addition
(define (axpy alpha vec1 vec2)
  (let ((ndim (vector-length vec1)))
    (if (not (= ndim (vector-length vec2)))
        (error 'axpy "- incompatible vector dimensions: " ndim)
        (do ((i 0 (1+ i)) (res (make-vector ndim)))
            ((= i ndim) res)
          (vector-set! res i (+ (* alpha (vector-ref vec1 i))
                                (vector-ref vec2 i)))))))
;;
;; mul is the generic matrix/scalar multiplication procedure
(define (mul x y)
  ;;
  ;; vec-sca-mul multiplies a vector by a scalar
  (define (vec-sca-mul vec val)
    (let ((dim (vector-length vec)))
      (do ((res (make-vector dim))
           (i 0 (1+ i)))
          ((= i dim) res)
        (vector-set! res i (* val (vector-ref vec i))) )))
  ;; 
  ;; vec-vec-mul scalar multiplication of two vectors
  (define (vec-vec-mul vec1 vec2)
    (let ((dim (vector-length vec1)))
      (if (not (= dim (vector-length vec2)))
          (match-error vec1 vec2)
          (do ((i   0 (1+ i))
               (res 0 (+ res (* (vector-ref vec1 i)
                                (vector-ref vec2 i)))))
              ((= i dim) res)) )))
  ;;
  ;; mat-sca-mul multiplies a matrix by a scalar
  (define (mat-sca-mul mat val)
    (let* ((nrow (matrix-nrow mat))
           (ncol (matrix-ncol mat))
           (res  (make-matrix nrow ncol 0)))
      (do ((i 0 (1+ i)))((= i nrow) res)
        (do ((j 0 (1+ j)))((= j ncol))
          (matrix-set! res i j (* val (matrix-ref mat i j))) )) ))
  ;;
  ;; mat-vec-mul multiplies a matrix by a vector:
  (define (mat-vec-mul mat vec)
    (let ((nrow (matrix-nrow mat))
          (ncol (matrix-ncol mat)))
      (if (not (= ncol (vector-length vec)))
          (match-error mat vec)
          (do ((res (make-vector nrow))
               (i 0 (1+ i)))
              ((= i nrow) r)
            (do ((j 0 (1+ j))
                 (sum 0 (+ sum (* (matrix-ref mat i j)(vector-ref vec j)))))
                ((= j ncol) (vector-set! res i sum)) )))))
  ;;
  ;; vec-mat-mul multiplies a vector by a matrix:
  (define (vec-mat-mul vec mat)
    (let ((nrow (matrix-nrow mat))
          (ncol (matrix-ncol mat)))
      (if (not (= nrow (vector-length vec)))
          (match-error vec mat)
          (do ((res (make-vector ncol))
               (i 0 (1+ i)))
              ((= i ncol) res)
            (do ((j 0 (1+ j))
                 (sum 0 (+ sum (* (matrix-ref mat j i)(vector-ref vec j)))))
                ((= j nrow) (vector-set! res i sum)) )))))
  ;;
  ;; mat-mat-mul multiplies one matrix by another
  (define (mat-mat-mul mat1 mat2)
    (let* ((nrow1 (matrix-nrow mat1))
           (nrow2 (matrix-nrow mat2))
           (ncol2 (matrix-ncol mat2))
           (res   (make-matrix nrow1 ncol2 0)))
      (if (not (= (matrix-ncol mat1) nrow2))
          (match-error mat1 mat2)
          (do ((i 0 (1+ i))) ((= i nrow1) res)
            (do ((j 0 (1+ j))) ((= j ncol2))
              (do ((k 0 (1+ k))
                   (sum 0 (+ sum (* (matrix-ref mat1 i k) (matrix-ref mat2 k j)))))
                  ((= k nrow2) (matrix-set! res i j sum)) )))) ))
  ;;
  ;; type-error is called to complain when mul receives an invalid
  ;; type of argument.
  (define (type-error what)(error 'mul "- not a number, vector or matrix:" what))
  ;;
  ;; match-error is called to complain when mul receives a pair of
  ;; incompativle arguments.
  (define (match-error what1 what2)
    (error 'mul "- incompatible operands:" what1 what2))
  ;;
  ;; body of mul, dispatch based on input types.
  (cond ((number? x) (cond ((number? y) (* x y))
                           ((matrix? y) (mat-sca-mul y x))
                           ((vector? y) (vec-sca-mul y x))
                           (else (type-error y)) ))
        ((matrix? x) (cond ((number? y) (mat-sca-mul x y))
                           ((matrix? y) (mat-mat-mul x y))
                           ((vector? y) (mat-vec-mul x y))
                           (else (type-error y))))
        ((vector? x) (cond ((number? y) (vec-sca-mul x y))
                           ((matrix? y) (vec-mat-mul x y))
                           ((vector? y) (vec-vec-mul x y))
                           (else (type-error y))))
        (else (match-error x y))) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; vector tools
;;
;;
;; vref - vector reference and optional assignment operator:
(define-macro vref (vec pos . val)
  (if (null? val)
      `(vector-ref ,vec ,pos)
      `(begin (vector-set! ,vec ,pos ,@val) ,@val)))
;;
;; vector-type - return the atomic vector type
;;
(define-macro vector-type (vec)
  `(cond [(vector-char?    ,vec) 'char]
         [(vector-double?  ,vec) 'flonum]
         [(vector-integer? ,vec) 'fixnum]
         [else                   'any]))
;;
;; vector-new - build a new vector of given type, size and optional inital value
;;
(define-macro vector-new (type size . val)
  (if (null? val)
      (case type
        [(char)   `(make-vector-char    ,size)]
        [(fixnum) `(make-vector-integer ,size)]
        [(flonum) `(make-vector-double  ,size)]
        [else     `(make-vector         ,size)])
      (case type
        [(char)   `(make-vector-char    ,size ,@val)]
        [(fixnum) `(make-vector-integer ,size ,@val)]
        [(flonum) `(make-vector-double  ,size ,@val)]
        [else     `(make-vector         ,size ,@val)])))
;;
;; call proc for vector range: (proc val) | (proc vec pos) | (proc vec pos end)
(define-macro vector-for (proc vec . range)
  (let ([pos (gensym)][end (gensym)])
    (cond [(null? range)
           `(let ([,pos 0] [,end (vector-length ,vec)])
              (while (< ,pos ,end)
                (,proc (vector-ref ,vec ,pos))
                (set! ,pos (1+ ,pos))))]
          [(null? (cdr range))
           `(let ([,pos (max 0 ,(car range))]
                  [,end (vector-length ,vec)])
              (while (< ,pos ,end)
                (,proc ,vec ,pos)
                (set! ,pos (1+ ,pos))))]
          [else
           `(let ([,pos (max 0 ,(car  range))]
                  [,end (min   ,(cadr range) (vector-length ,vec))])
              (while (< ,pos ,end)
                (,proc ,vec ,pos ,end)
                (set! ,pos (1+ ,pos))))])))
;;
;; inplace assignment operators
;;
(define-macro make-vector-op (op)
  `(lambda (vec pos val)
     (let ([val (,op (vector-ref vec pos) val)])
       (vector-set! vec pos val)
       val)))

(define vector+= (make-vector-op +))
(define vector-= (make-vector-op -))
(define vector/= (make-vector-op /))
(define vector*= (make-vector-op *))

(define (vector1++ vec pos)
  (let ([prv (vector-ref vec pos)])
    (vector-set! vec pos (1+ prv))
    prv))
;;
;; vector-norm - calculate the euclidian norm of a numeric vector or optional
;;               a subrange: (pos, end)
;;
(define (vector-norm vec . range)
  (let ([norm 0.])
    (apply vector-foreach
           (lambda (val)
             (set! norm (+ norm (* val val))))
           vec range)
    (sqrt norm)))
;;
;; vector-scalar - scalar product of two vector or subrange: pos, end
;;                 efficient version of: (vector-reduce + (vector-map * vec1 vec2))
;;
(define (vector-scalar vec1 vec2 . range)
  (let ([pos (list-ref? range 0 0)][res 0])
    (vector-foreach
     (lambda (val)
       (set! res (+ res (* val (vector-ref vec2 (1++ pos))))))
     vec1 pos (list-ref? range 1 (vector-length vec1)))
    res))

(define (vector-scale scalar vec . argl)
  (let ([res (list-ref? argl 0 (vector-new (vector-type vec) (vector-length vec)))]
        [pos (list-ref? argl 1 0)])
    (vector-foreach (lambda (val) (vector-set! res (1++ pos) (* scalar val)))
                    vec 0 (- (list-ref? argl 2 (vector-length res)) pos))
    res))

(define (vector-axpy! scalar vec1 vec2 . range)
  (let ([pos (list-ref? range 0 0)])
    (vector-foreach (lambda (val)
                      (vector-set! vec2 pos (+ (* scalar val) (vector-ref vec2 pos)))
                      (set! pos (1+ pos)))
                    vec1 pos (list-ref? range 1 (vector-length vec1)))))

(define (vector-reduce proc vec . range)
  (let ([pos (list-ref?  range 0 0)]
        [res (vector-ref vec   0)])
    (vector-foreach
     (lambda (val) (set! res (proc res val)))
     vec (1+ pos) (list-ref? range 1 (vector-length vec)))
    res))

(define (vector-map proc vec1 vec2 . argl)
  (let ([vec (list-ref? argl  0 (vector-new (vector-type vec1) (vector-length vec1)))]
        [pos (list-ref? argl 1 0)]
        [i    0])
    (vector-foreach
     (lambda (val)
       (vector-set! vec (1++ pos) (proc val (vector-ref vec2 (1++ i)))))
     vec1)
    vec))

(define (vector-sub vec1 vec2 . argl)(apply vector-map - vec1 vec2 argl))

(define (vector-add vec1 vec2 . argl)(apply vector-map + vec1 vec2 argl))

(define (vector-cross vec1 vec2 . argl)
  (let ([vec (list-ref? argl 0 (vector-new (vector-type vec1) 3))]
        [pos (list-ref? argl 1 0)])
    (vector-set! vec pos       [- (* (vector-ref vec1 1) (vector-ref vec2 2))
                                  (* (vector-ref vec1 2) (vector-ref vec2 1))])
    (vector-set! vec (+ pos 1) [- (* (vector-ref vec1 2) (vector-ref vec2 0))
                                  (* (vector-ref vec1 0) (vector-ref vec2 2))])
    (vector-set! vec (+ pos 2) [- (* (vector-ref vec1 0) (vector-ref vec2 1))
                                  (* (vector-ref vec1 1) (vector-ref vec2 0))])
    vec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filter object from list 
;;
(define (filter pred? obj list)
  (if (list? list) 
      (let filter ((new ())(list list))
        (cond ((null? list) (reverse! new))
              ((pred? (car list) obj) (filter new (cdr list)))
              (else (filter (cons (car list) new) (cdr list)))))
      (error! 'filter "not a list argument:" list)))
;;
;; select key-value pairs from association list and filter unknown keys
(define (select-from alist keys)
  (if (list? keys)
      (let ((res (filter eq? #f (map (lambda (key)(assoc key alist)) keys))))
        (if (= (length keys) (length res)) (map second res) #f))
      (let ((res (assoc keys alist)))
        (if res (second res) res))))
;;
;; Simple while loop with non-local exit: (while-do (< i nmax) (display i) (if (= i 33) 'break))
(define-macro while-do (pred . body)
  (letrec ((replace (lambda (symb new expr)
                      (cond ((null?   expr) ())
                            ((symbol? expr) (if (eq? expr symb) new expr))
                            ((pair?   expr) (cons (replace symb new (car expr))
                                                  (replace symb new (cdr expr))))
                            (else expr)))))
    (let ((body (replace 'break '(break (if #f #f)) body))(loop (gensym)))
      `(call-with-current-continuation
        (lambda (break)
          ((lambda (,loop)(when ,pred ,@body (,loop ,loop)))
           (lambda (,loop)(when ,pred ,@body (,loop ,loop)))) )))))
;;
;; return a function which cycles between arguments at each call:
;;   ex.: (define flip-flop (make-cycle 1 2)) => 1, 2, 1, 2, ....
(define (make-cycle . args)
  (if (null? args)
      (lambda () args)
      (let ((nxt args)(arg ()))
        (lambda ()
          (set! arg (car nxt))
          (set! nxt (if (null? (cdr nxt)) args (cdr nxt)))
          arg))))
;;
;; get year substring from date-time-year string
;;
(define-macro year ()
  (let* ((str  (current-time))(len (string-length str))
         (year (substring str (- len 4) len)))
    year))
;;
;; get time substring
;;
(define-macro time ()
  (let* ((len (- (string-length (current-time)) (1+ (string-length (year))))))
    `(substring (current-time) 0 ,len)))
;;
;; export-c generates this file:
;;   (export->c '(".pschemerc" "picoslib.init") "pscmInitrc.h")
;;
(define (export->c file-list output-file)
  (call-with-output-file output-file
    (lambda (out-port)
      (display "#ifndef PSCM_INITRC\n"            out-port)
      (display "#define PSCM_INITRC\n"            out-port)
      (display "static const char Initrc[] = {\n" out-port)
      (for-each (lambda (file)    ; for each input file do:
                  (call-with-input-file file
                    (lambda (port)
                      (let ([line (readline port)])
                        (while (not (eof-object? line))
                          (write (string-append line "\n") out-port)
                          (newline out-port)
                          (set! line (readline port)))))))
                file-list)
      (display "};\n"     out-port)
      (display "#endif\n" out-port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
