#lang racket
(define (myIf predicate thenClause elseClause)
       (cond (predicate thenClause)
             (else elseClause)))
