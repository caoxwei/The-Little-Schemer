#lang racket

(define col
  (lambda (x y)
      (null? y)))


(define last-func
     (lambda(x y) (length x)))


(define when-match
      (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen))))


(define when-differ
          (lambda (newlat seen)
                          (col (cons (car lat) newlat)
                               seen)))
       
  




(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote ()) (quote ())))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
        (multirember&co a (cdr lat)
                        (lambda (newlat seen)
                          (col (cons (car lat) newlat)
                               seen)))))))