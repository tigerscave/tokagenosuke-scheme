(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember_star
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat))
      (cond
        ((eq? (car lat) a) (rember_star a (cdr lat)))
        (else (cons (car lat) (rember_star a (cdr lat))))))
    (else (cons (rember_star a (car lat)) (rember_star a (cdr lat)))))))

(print (rember_star 'a '(ae (a ab ab) c d a a a a a c)))
