; Color Europe

(define (assert p)
  (if (not p) (amb) #n))

(define (choose-color)
  (amb 'red 'green 'yellow 'blue))

(define (color-europe)
  (let ((p (choose-color))
	   (e (choose-color))
	   (f (choose-color))
	   (b (choose-color))
	   (h (choose-color))
	   (g (choose-color))
	   (l (choose-color))
	   (i (choose-color))
	   (s (choose-color))
	   (a (choose-color)))
  (let ((portugal 
	  (list 'portugal p (list e)))
	(spain
	  (list 'spain e (list f p)))
	(france
	  (list 'france f (list e i s b g l)))
	(belgium
	  (list 'belgium b (list f h l g)))
	(holland
	  (list 'holland h (list b g)))
	(germany
	  (list 'germany g 
		(list f a s h b l)))
	(luxembourg
	  (list 'luxembourg l (list f b g)))
	(italy
	  (list 'italy i (list f a s)))
	(switzerland
	  (list 'switzerland s (list f i a g)))
	(austria 
	  (list 'austria a (list i s g))))
  (let ((countries (list
	portugal spain france belgium
	holland germany luxembourg
	italy switzerland austria)))
    (for-each (lambda (c)
      (assert (not (memq (cadr c)
				 (caddr c)))))
      countries)
    (map (lambda (l) (cons (car l) (cadr l)))
      countries)))))  
