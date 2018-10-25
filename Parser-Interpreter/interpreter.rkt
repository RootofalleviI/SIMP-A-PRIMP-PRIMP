#lang racket

(require "util.rkt")
(require "struct.rkt")

(define (do-interp ast env st)

  ;;(printf "ast: ~a\nenv: ~a\nst: ~a\n\n" ast env st)
  
  (match ast
    [(fun v bdy) (result (closure v bdy env) st)]
	
    [(app fun-exp arg-exp)
     (match (do-interp fun-exp env st) 
       [(result (closure val bdy cl-env) st2)
        (match-let*
            ([(result v s) (do-interp arg-exp env st2)])
          (do-interp bdy
                  (cons (sub val (length s)) env)
                  (cons (sub (length s) v) s)))])]
				  
    [(bin op x y)
     (match-let*
         ([(result v1 s1) (do-interp x env st)]
          [(result v2 s2) (do-interp y env s1)])
       (result ((op-trans op) v1 v2) s2))]
	   
    [(seq x y)
     (let* {[r1 (do-interp x env st)]}
       (do-interp y env (result-newstore r1)))]

    [(newbox exp)
     (match-let*
         ([(result v sp) (do-interp exp env st)])
       (result (length sp) (cons (sub (length sp) v) sp)))]
    
    [(openbox exp)
     (match-let*
         ([(result v sp) (do-interp exp env st)])
       (do-interp (st-lookup v sp) env sp))]
    
    [(setbox bexp vexp)
     (match-let*
         ([(result loc s1) (do-interp vexp env st)]
          [(result val s2) (do-interp bexp env s1)])
       (result void (cons (sub val loc) s2)))]
	   
    [x
     (if (number? x)
         (result x st)
         (result (st-lookup (lookup x env) st) st))]))

(provide do-parse)