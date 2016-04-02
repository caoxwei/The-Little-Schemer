#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


;这个递归式子有什么错误，不解？
;注意，car和cdr接受一个非空的list
;同时对于scheme中嵌套的if语句，不知道怎么使用,可以用嵌套的cond来模拟
;(define (mylat? x)
;         (if (not (atom? (car x)))
;             false
;             (mylat? (cdr x))))


;(define (mylat? x)
 ;        (if(null? x)
  ;          (#t)
   ;         (if (not (atom? (car x))
    ;                (#f)
     ;               (mylat? (cdr x))))))


;define a function
(define (mylat? l)
      (cond ((null? l) #t)
            ((atom? (car l)) (mylat? (cdr l)))
                (else #f)))

;define a function with lambda
(define lat?
    (lambda (l)
       (cond
         ((null? l) #t)   ;注意空列表也是一个lat
         ((atom? (car l)) (lat? (cdr l)))
         (else #f))))


;判断一个原子是否属于一个lat
(define (membera? a lat)
    ( cond ((null? lat) #f)
          ((eq? a (car lat)) #t)
           (else (membera? a (cdr lat)))))

;原书程序

(define (memberb? a lat)
    ( cond ((null? lat) #f)
           (else (or (eq? a (car lat))
                     (memberb? a (cdr lat))))))

(define atom 'atom)
(define testlat '(aka atom tata maxi))
;rember,从一个lat中移除一个atom(只移除一次),参数为一个atom和一个lat
;(rember atom lat)
;(define (rember atom lat)
  ;      (cond ((null? lat) '())
   ;           ((not (eq? atom (car lat))) (cons (car lat) lll)  (cons lll (rember atom (cdr lat))) ) 
    ;          (else  (cdr lat))))
;(rember atom testlat) 
;函数式编程如何单步调试，查看变量？

;书上的定义

(define rember-wrong
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        (else (cond
                ((eq? (car lat) a) (cdr lat))
                 (else (rember-wrong a (cdr lat))))))))

;递归确实是比较难以理解
(define rember-right
     (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        (else (cond
                ((eq? (car lat) a) (cdr lat))
                 (else (cons (car lat)
                       (rember-right a (cdr lat)))))))))


(define rember-right2
     (lambda (a lat)
        (cond ((null? lat) (quote ()))
                ((eq? (car lat) a) (cdr lat))
                 (else (cons (car lat)
                       (rember-right2 a (cdr lat)))))))

(define first 
     (lambda (l)
        (cond ((null? l) '())
              (else (cons (car (car l)) (first (cdr l)))))))
  
;testfirst   (first '((paper water eric) (people love junk)(doing sports)))

(define replace
      (lambda ( new old lat)
           (cond  ((null? lat) '())
                  ((eq? old (car lat)) (cons new (cdr lat)))
                  (else (cons  (car lat) (replace new old (cdr lat)))))))
           
;testreplace  (replace  'haha  'caowei '(cao wei caowei is a dummy))



(define insertR
      (lambda ( new old lat)
           (cond  ((null? lat) '())
                  ((eq? old (car lat)) (cons old (cons new (cdr lat))))
                  (else (cons  (car lat) (insertR new old (cdr lat)))))))


(define insertL
      (lambda ( new old lat)
           (cond  ((null? lat) '())
                  ((eq? old (car lat)) (cons new lat))
                  (else (cons  (car lat) (insertL new old (cdr lat)))))))


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
              