(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(define rember_star
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
      (cond
        ((eq? (car l) a) (rember_star a (cdr l)))
        (else (cons (car l) (rember_star a (cdr l))))))
      (else (cons (rember_star a (car l)) (rember_star a (cdr l)))))))

(print (rember_star 'tea '((coffee) cup ((tea) cup) (and (hick)) cup)))
