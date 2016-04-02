#lang racket



(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;用普通思维思考，
;测试代码
　　　　　;;(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

;(define rember*
 ;    (lambda (a lst)
  ;       (cond ((null? lst) '())
   ;            ((and (atom? (car lst)) (eq? a (car lst)) )  (rember* a (cdr lst)))
    ;           (else (cons (rember* a (car lst)) (rember* a (cdr lst)) )))))

(define rember*
      (lambda (a lst)
           (cond ((null? lst) '())
                 ((atom? (car lst))
                    (cond ((eq? (car lst) a) (rember* a (cdr lst)))
                          (else (cons (car lst)(rember* a (cdr lst))))))
　　              (else (cons (rember* a (car lst)) (rember* a (cdr lst)))))))  ;分而治之


;test (insertR* 'Eric 'Caow '((Caow is my name) Caow is a dummy (Caow please let it go) ((Caow) ((Caow good luck)) Caow is a dush)))
(define insertR*
        (lambda (new old lst)
          (cond ((null? lst) '())
                ((atom? (car lst))
                      (cond ((eq? old (car lst)) (cons old (cons new (insertR* new old (cdr lst)))))
                            (else (cons (car lst) (insertR* new old (cdr lst))))))
                (else (cons (insertR* new old (car lst)) (insertR* new old (cdr lst)))))))


;test    (occur* 'Caow '((Caow is my name) Caow is a dummy (Caow please let it go) ((Caow) ((Caow good luck)) Caow is a dush)))
(define occur*
      (lambda (atom lst)
           (cond ((null? lst) 0)
                  ((atom? (car lst))
                      (cond ((eq? atom (car lst)) (add1 (occur* atom (cdr lst))))
                            (else  (occur* atom (cdr lst)))))
                  (else  (+ (occur* atom (car lst)) (occur* atom (cdr lst)))))))

;(replace* 'Eric 'Caow '((Caow is my name) Caow is a dummy (Caow please let it go) ((Caow) ((Caow good luck)) Caow is a dush)))
(define replace*
        (lambda (new old lst)
          (cond ((null? lst) '())
                ((atom? (car lst))
                      (cond ((eq? old (car lst)) (cons new (replace* new old (cdr lst))))
                            (else (cons (car lst) (replace* new old (cdr lst))))))
                (else (cons (replace* new old (car lst)) (replace* new old (cdr lst)))))))

;test  (insertL* 'Eric 'Caow '((Caow is my name) Caow is a dummy (Caow please let it go) ((Caow) ((Caow good luck)) Caow is a dush)))
(define insertL*
        (lambda (new old lst)
          (cond ((null? lst) '())
                ((atom? (car lst))
                      (cond ((eq? old (car lst)) (cons new (cons old (insertL* new old (cdr lst)))))
                            (else (cons (car lst) (insertL* new old (cdr lst))))))
                (else (cons (insertL* new old (car lst)) (insertL* new old (cdr lst)))))))


  ;test      (member*  'Caow '((Caow is my name) Caow is a dummy (Caow please let it go) ((Caow) ((Caow good luck)) Caow is a dush)))
(define member*
        (lambda (a lst)
          (cond ((null? lst) #f)
                ((atom? (car lst))
                      (cond ((eq? a (car lst)) #t)
                            (else  (member* a (cdr lst)))))
                (else (or (member* a (car lst))  (member* a (cdr lst)))))))

;test  (leftmost '(((lala)) haha (lala))),这次不用再问null?了
(define leftmost
     (lambda (lst)
          (cond 
                ((atom? (car lst)) (car lst))
                (else (leftmost (car lst))))))

;(eqlist? '(3 wenxi (xicai (３)) pan caowei) '(3 wenxi (xicai (３)) pan caowei))
;(eqlist? '(3 wenxi (xicai (３)) pan caowei) '(3 wenxi (xicai (３)) pan ))
(define eqlist?　　;判断两个list是否相同
      (lambda (lsta lstb)
           (cond ((and (null? lsta) (null? lstb)) #t)
                 ((or (null? lsta) (null? lstb)) #f)
                 (else  (cond  ((and (atom? (car lsta)) (atom? (car lstb)))
                                  (cond ((eq? (car lsta) (car lstb)) (eqlist? (cdr lsta) (cdr lstb)))
                                        (else #f)))
                               ( (or (atom? (car lsta)) (atom? (car lstb))) #f) 
                                (else (and (eqlist? (car lsta) (car lstb )) (eqlist? (cdr lsta) (cdr lstb )))))))))
                      
 ;不用atom?进行定义, 函数功能是判断两个atom或者两个数字是否相等．
(define eqan?
      (lambda (a1 a2)
         (cond ((and (number? a1) (number? a2)) (= a1 a2))
               ((or (number? a1) (number? a2)) #f)
               (else(eq? a1 a2)))))

(define equal?   ;判断两个S-expression是否相同　
     (lambda(s1 s2)
         (cond
             ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
             ((or (atom? s1) (atom? s2)) #f)
             (else (eqlist? s1 s2)))))


(define *
    (lambda (n m)
         (cond
             ((zero? m) 0)
             (else (+ n (* n (sub1 m)))))))


(define ^
       (lambda(a n)
         (cond ((zero? n) 1)
               (else (* a (^ a (sub1 n)))))))

;test  (numbered? '((4^5)+28+32+(32^24)))

(define numbered?
     (lambda (aexp)
          (cond
             ((atom? aexp) (number? aexp))
             ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
             ((eq? (car (cdr aexp)) (quote *)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
             ((eq? (car (cdr aexp)) (quote ^)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))




(define simpler-numbered?
     (lambda (aexp)
          (cond
             ((atom? aexp) (number? aexp))
             ((eq? (car (cdr aexp)) (quote +)) (and (simpler-numbered? (car aexp)) (simpler-numbered? (car (cdr (cdr aexp))))))
             ((eq? (car (cdr aexp)) (quote *)) (and (simpler-numbered? (car aexp)) (simpler-numbered? (car (cdr (cdr aexp))))))
             ((eq? (car (cdr aexp)) (quote ^)) (and (simpler-numbered? (car aexp)) (simpler-numbered? (car (cdr (cdr aexp)))))))))

;test  (value '((3 + 4) + (4 * 3)))  
(define value
      (lambda(nexp)
          (cond
            ((atom? nexp) nexp)
            ((eq? (car (cdr nexp)) (quote +)) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
             ((eq? (car (cdr nexp)) (quote *)) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
             ((eq? (car (cdr nexp)) (quote ^)) (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))
             



(define 1st-sub-exp
    (lambda (aexp)
       (car (cdr aexp))))

(define 2nd-sub-exp
    (lambda (aexp)
       (car (cdr (cdr aexp)))))




; 不管用
;test   (value-prefix '(+ 3 (+ 4 3)))
(define value-prefix
      (lambda(nexp)
          (cond
            ((atom? nexp) nexp)
            ((eq? (car  nexp) (quote +)) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
             ((eq? (car  nexp) (quote *)) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
             ((eq? (car nexp) (quote ^)) (^ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))




;(member? 4 '(4 a la haha))
(define member?
      (lambda (a lat)
        (cond
          ((null? lat) #f)
          (else (or (equal? (car lat) a)
                    (member? a (cdr lat)))))))
;(set? '(a 4 3 2 9 a))
(define set?
    (lambda (lat)
       (cond
         ((null? lat) #t)
         (else
          (cond
            ((member? (car lat) (cdr lat)) #f)
            (else (set? (cdr lat))))))))
;;test   (makeset '(caowei xiang fang liu  cao eric xiang caowei))
(define makeset
    (lambda (lat)
        (cond
          ((null? lat) '())
          ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
          (else (cons (car lat) (makeset (cdr lat)))))))



(define multirember
     (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        (else (cond
                ((equal? (car lat) a) (multirember a (cdr lat)))
                 (else (cons (car lat)
                       (multirember a (cdr lat)))))))))






;;test   (makeset2 '(caowei xiang fang liu  cao eric xiang caowei))
;(makeset2 '(apple peach pear peach plum apple lemon peach))

;(makeset2 '(apple 3 pear 4 9 applem apple lemon peach))

(define makeset2
    (lambda (lat)
        (cond
          ((null? lat) '())
          (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))


;(two-in-row '(cao eric james james lua))
(define two-in-row
      (lambda (lat)
        (cond ((null? lat) #f)
              ((null? (cdr lat)) #f)
              (else (or (equal? (car lat) (car (cdr lat))) (two-in-row (cdr lat)))))))


(define is-first?
  (lambda (a lat)
         (cond
           ((null? lat) #f)
           (else (eq? (car lat) a)))))

(define two-in-a-row?
  (lambda(lat)
    (cond
      ((null? lat) #f)
      (else
          (or (is-first? (car lat) (cdr lat))
              (two-in-a-row? (cdr lat)))))))
  
;函数可以以不同的方式定义

;你中有我，我中有你
;if-first-b?定义用到了two-in-a-row-b？，而后者定义用到了前者
;用到了schmer中惰性求职
;is-first-b?中lat非空且由于or只有第一个表达式为0时，才对第二个求值

(define is-first-b?
  (lambda (a lat)
         (cond
           ((null? lat) #f)
           (else (or (eq? (car lat) a) (two-in-a-row-b? lat))))))



(define two-in-a-row-b?
  (lambda(lat)
    (cond
      ((null? lat) #f)
      (else
           (is-first-b? (car lat) (cdr lat) )))))




(define two-in-a-row-c?
  (lambda(preceding lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) preceding)
           (two-in-a-row-c?
             (car lat)
             (cdr lat) ))))))



;(final-2inarow '(cao eric james james lua))
(define final-2inarow
     (lambda (lat)
       (cond
         ((null? lat) #f)
         (else (two-in-a-row-c? (car lat)
                            (cdr lat))))))







(define sum-of-prefixes-b
  (lambda(sonssf tup)   ;显示地定义了第一个数，可定义为0
    (cond
      ((null? tup) '())
      (else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b
                   (+ sonssf (car tup))
                   (cdr tup)))))))

;(sum-of-prefixes '(1 2 3 4 5))
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))



(define one?
  (lambda(n)
    (cond
      (else (= n 1)))))

(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
        (else (pick (sub1 n) (cdr lat))))))



(define scramble-b
   (lambda(tup rev-pre)
     (cond
       ((null? tup) '())
       (else
        (cons (pick (car tup)
                    (cons (car tup) rev-pre))
               (scramble-b (cdr tup)
                    (cons (car tup) rev-pre)))))))

















 





