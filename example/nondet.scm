;; Non-deterministic primitives
(define (amb-fail) 
  (error "alternatives exhausted"))

;; choose any of the alternatives given,
;; use (amb) to indicate failure
;; execution will find a valid combination of alternatives
;; or raise an error if none exists    
(define-macro (amb alts)
  `(let ((+prev amb-fail))
    (call/cc (lambda (sk)
      ,@(map (lambda (alt)
	`(call/cc (lambda (fk)
	  (set! amb-fail (lambda ()
	    (set! amb-fail +prev)
	    (fk 'fail)))
	  (sk ,alt))))
	(cdr alts))
    (+prev)))))

;; collect all possible solutions into a list/bag
;; this is done by repeatedly failing after a solution
;; has been found
;; the last (reverse ...) is to list the the solutions
;; in the order found
(define-macro (bag-of ex)
  `(let ((+prev amb-fail) (+res '()))
    (if (call/cc (lambda (k)
	(set! amb-fail
		(lambda () (k #f)))
	(let ((+v ,(cadr ex)))
	  (set! +res (cons +v +res))
	  (k #t))))
      (amb-fail) #n)
    (set! amb-fail +prev)
    (reverse +res)))
