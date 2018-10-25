#lang racket

(require "struct.rkt")
(require "util.rkt")

(define (do-parse sx)
  (match sx
    [`(seq ,x ,y) (seq (do-parse x) (do-parse y))]
    [`(box ,x) (newbox (do-parse x))]
    [`(unbox ,x) (openbox (do-parse x))]
    [`(setbox ,x ,y) (setbox (do-parse x) (do-parse y))]
    [`(with ((,nm ,nmd)) ,bdy) (app (fun nm (do-parse bdy)) (do-parse nmd))]
    [`(+ ,x ,y) (bin '+ (do-parse x) (do-parse y))]
    [`(* ,x ,y) (bin '* (do-parse x) (do-parse y))]
    [`(- ,x ,y) (bin '- (do-parse x) (do-parse y))]
    [`(/ ,x ,y) (bin '/ (do-parse x) (do-parse y))]
    [`(fun (,x) ,bdy) (fun (do-parse x) (do-parse bdy))]
    [`(,f ,x) (app (do-parse f) (do-parse x))]
    [x x]))
	
(provide do-do-parse)