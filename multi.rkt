#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;(multiinsertLR 'lang 'car 'bus '(eric james lebron car kobe bus))
(define multiinsertLR
     (lambda (new oldL oldR lat)
        (cond
         ((null? lat ) (quote ()))
         ((eq? (car lat) oldL)
          (cons new
            (cons oldL
             (multiinsertLR new oldL oldR
               (cdr lat)))))
         ((eq? (car lat ) oldR)
          (cons oldR
           (cons new
            (multiinsertLR new oldL oldR
             (cdr lat)))))
         (else
          (cons (car lat)
           (multiinsertLR new oldL oldR
            (cdr lat)))))))



;test   (remberall 'cup '(coffe cup tea cup and hick cup))
(define multirember
     (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        (else (cond
                ((eq? (car lat) a) (multirember a (cdr lat)))
                 (else (cons (car lat)
                       (multirember a (cdr lat)))))))))

;test   (multiinsertR 'fried 'fish  '(chips and fish or fish and fried))
(define multiinsertR
      (lambda ( new old lat)
           (cond  ((null? lat) '())
                  ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
                  (else (cons  (car lat) (multiinsertR new old (cdr lat)))))))

;test   (multiinsertL 'fried 'fish  '(chips and fish or fish and fried))
(define multiinsertL
      (lambda ( new old lat)
           (cond  ((null? lat) '())
                  ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
                  (else (cons  (car lat) (multiinsertL new old (cdr lat)))))))


;test   (multireplace 'fried 'fish  '(chips and fish or fish and fried))
(define multireplace
      (lambda ( new old lat)
           (cond  ((null? lat) '())
                  ((eq? old (car lat)) (cons new (multireplace new old (cdr lat))))
                  (else (cons  (car lat) (multireplace new old (cdr lat)))))))


;testcode
;(multiinsertLR&co 'foo 'bar 'baz '(foo bar baz qux quux) list)
;(multiinsertLR&co 'foo 'bar 'baz '(goo bar baz qux quux) list)
;(multiinsertLR&co 'foo 'bar 'baz '(baz goo bar baz qux bar quux baz) list)
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co
       new
       oldL
       oldR
       (cdr lat)
       (lambda (newlat L R) 
         (col (cons new
                    (cons oldL newlat))
              (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co
       new
       oldL
       oldR
       (cdr lat)
       (lambda (newlat L R)
         (col (cons oldR
                    (cons new newlat))
              L (add1 R)))))
     (else
      (multiinsertLR&co
       new
       oldL
       oldR
       (cdr lat)
       (lambda (newlat L R)
         (col (cons (car lat)
                    newlat) L R)))))))



(define evens-results
 (lambda (newl product sum)
   (cons sum (cons product newl))))


;(evens-only*&co  '((9 1 2 8) 3 10 ((9 9) 7 6) 2)  evens-results)
(define evens-only*&co
 (lambda (l col)
   (cond
    ((null? l)
     (col '() 1 0))
    ((atom? (car l))
     (cond
      ((even? (car l))
       (evens-only*&co (cdr l)
                    (lambda (newl product sum)
                      (col (cons (car l) newl)
                           (* (car l) product)
                           sum))))
      (else
       (evens-only*&co (cdr l)
                    (lambda (newl product sum)
                      (col newl product (+ (car l) sum)))))))
    (else
     (evens-only*&co (car l)
                  (lambda (newl product sum)
                    (evens-only*&co (cdr l)
                                    (lambda (dnewl dproduct dsum)
                                      (col (cons newl dnewl)
                                           (* product dproduct)
                                           (+ sum dsum))))))))))


(define pick
    (lambda (n lat)
        (cond
          ((zero? (sub1 n)) (car lat))
          (else (pick (sub1 n) (cdr lat))))))
(define keep-looking
    (lambda (a sorn lat)
       (cond
         ((number? sorn)
          (keep-looking a (pick sorn lat) lat))
         (else (eq? sorn a)))))


(define looking
   (lambda (a lat)
     (keep-looking a (pick 1 lat) lat)))
;testcode
; (looking 'caviar '(6 2 4 caviar 5 7 3))
;(looking 'caviar '(6 2 grits caviar 5 7 3))

(define eternity
    (lambda (x)
      (eternity x)))

;build pair
(define build
    (lambda (s1 s2)
      (cond
        (else (cons s1 (cons s2 '()))))))
;testcode
;(shift '((a b) (c d)))
;(shift '((a b) c))
(define shift
     (lambda (pair)
        (build (first (first pair))
             (build (second (first pair))
                    (second pair)))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x ) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define align
    (lambda (pora)
      (cond
        ((atom? pora) pora)
        ((a-pair? (first pora))
         (align (shift pora)))
        (else (build (first pora)
                     (align (second pora)))))))
;(weight* '(a (b c)))
;(weight* '((a b) c))
(define length*
   (lambda (pora)
     (cond
       ((atom? pora) 1)
       (else
        (+ (length* (first pora))
           (length* (second pora)))))))


(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (* (weight* (first pora)) 2)
          (weight* (second pora)))))))
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))


;(shuffle '(a (b c)))
;(shuffle '((a b) (c d)))
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
         (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(define one?
  (lambda(n)
    (cond
      (else (= n 1)))))

(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))
                       
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))
                 



  
