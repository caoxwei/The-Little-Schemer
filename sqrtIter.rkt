#lang racket
(define (average x y)
        (/ 2 (+ x y))) 

(define (myIf predicate thenClause elseClause)
       (cond (predicate thenClause)
             (else elseClause)))

(define (improve guess x)
             (average guess (/ x guess)))

(define (square x)
          (* x x))

(define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
       (myIf (good-enough? guess x)
           guess
           (sqrt-iter (improve guess x)
                      x)))


(define (improve-cube guess x)
            (/ (+ (/ x (* guess guess)) (* guess 2)) 3)    )  


(define (cube-good-enough? guess x)
        (< (abs (- (* guess guess guess) x)) 0.001))


(define (cube-iter guess x)
       (if(cube-good-enough? guess x)
           guess
           (cube-iter (improve-cube guess x)
                      x)))