#lang racket

(define (op-trans op)
  (match op
    ['+ +]
    ['* *]
    ['- -]
    ['/ /]))

(define (lookup var env)
  (cond
    [(empty? env) (error 'interp "unbound variable ~a" var)]
    [(symbol=? var (sub-name (first env))) (sub-loc (first env))]
    [else (lookup var (rest env))]))
	
(define (st-lookup loc st)
  (cond
    [(empty? st) (error 'interp "unbound variable ~a" loc)]
    [(= loc (sub-name (first st))) (sub-loc (first st))]
    [else (st-lookup loc (rest st))]))
	
(provide (all-defined-out))
