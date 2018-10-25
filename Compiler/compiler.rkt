#lang racket

(provide compile-simp)

(define (compile-simp prog)
  (compile prog '()))

;;==================================================================================================
;; Global helpers: iif and while label name generator. Preventing conflicts.
(define iif-ind 0)
(define while-ind 0)
(define (iif-generater name iif-ind)
  (string->symbol (string-append name (number->string iif-ind))))
(define (while-generater name while-ind)
  (string->symbol (string-append name (number->string while-ind))))
;;==================================================================================================


;;==================================================================================================
;; Level two: separate variable definitions and program statements.
(define (compile prog output)
  (begin
    (match prog
      [`(vars ,var-defns ,stmts ...)
       (set! output (append (convert-statements stmts '()) output)) 
       (set! output (append (initialize-variables var-defns '()) output))])
    (reverse output)))
;;==================================================================================================

;;==================================================================================================
;; Level three: handling variable definitions. Pure functional.
(define (initialize-variables var-defns return-lst)
  (define (initialize-variables-recursive lst-of-pairs return-lst)
    (cond
      [(empty? lst-of-pairs)
       (let* {[initialization-block '((data stack 0) (data SP stack) (halt))]}
         (append initialization-block return-lst))]
      [else
       (let* {[current-pair (first lst-of-pairs)]
              [var-name (first current-pair)]
              [var-val (second current-pair)]}
         (initialize-variables-recursive
          (rest lst-of-pairs)
          (cons `(data ,var-name ,var-val) return-lst)))]))
  (match var-defns
    [`[,lst-of-pairs ...]
     (initialize-variables-recursive lst-of-pairs return-lst)]))
;;==================================================================================================

;;==================================================================================================
;; Level three: converting statements. Imperative. Can be used recursively.
(define (convert-statements stmts return-lst)
  (begin
    (for [(stmt stmts)]
      (match stmt
        [`(set ,id ,aexp)
         (let* {[arithmetic-block (handle-aexp aexp '())]
                [set-block `((sub SP SP 1) (move ,id (-1 SP)))]}
           (set! return-lst (append set-block arithmetic-block return-lst)))]
        [`(print ,aexp)
         (if (string? aexp)
             (let* {[block `((print-string ,aexp))]}
               (set! return-lst (append block return-lst)))
             (let* {[arithmetic-block (handle-aexp aexp '())]
                    [set-block `((sub SP SP 1) (print-val (-1 SP)))]}
               (set! return-lst (append set-block arithmetic-block return-lst))))]
        [`(while ,bexp ,stmts ...)
         (begin
           (set! while-ind (+ 1 while-ind))
           (set! return-lst (append (handle-while bexp stmts '() while-ind) return-lst)))]
        [`(iif ,bexp ,stmt1 ,stmt2)
         (begin
           (set! iif-ind (+ 1 iif-ind))
           (set! return-lst (append (handle-iif bexp stmt1 stmt2 '() iif-ind) return-lst)))]
        [`(seq ,stmts ...)
         (set! return-lst (append (convert-statements stmts '()) return-lst))] 
        [`(skip)
         (let {[block empty]} (set! return-lst (append empty return-lst)))]))
    return-lst))
;;==================================================================================================

;;==================================================================================================
;; Level four: Arithmetic expression.
(define (handle-aexp aexp return-lst)
  (match aexp
    [(? number? aexp)
     (let {[block `((add SP SP 1) (move (0 SP) ,aexp))]}
       (set! return-lst (append block return-lst)))]
    [(? symbol? aexp)
     (let {[block `((add SP SP 1) (move (0 SP) ,aexp))]}
       (set! return-lst (append block return-lst)))]
    [`(,op ,a1 ,a2)
     (set! return-lst (append (handle-aexp a1 '()) return-lst))
     (set! return-lst (append (handle-aexp a2 '()) return-lst))
     (let {[block `((sub SP SP 1) (,(op-trans op) (-2 SP) (-2 SP) (-1 SP)))]}
       (set! return-lst (append block return-lst)))])
  return-lst)
;;==================================================================================================

;;==================================================================================================
;; Level four: Boolean expression.
(define (handle-bexp bexp return-lst)
  (define (true? bexp) (equal? 'true bexp))
  (define (false? bexp) (equal? 'false bexp))
  (match bexp
    [(? true? bexp)
     (let {[block `((add SP SP 1) (move (0 SP) #t))]}
       (set! return-lst (append block return-lst)))]
    [(? false? bexp)
     (let {[block `((add SP SP 1) (move (0 SP) #f))]}
       (set! return-lst (append block return-lst)))]
    [(? number? bexp)
     (let {[block `((add SP SP 1) (move (0 SP) ,bexp))]}
       (set! return-lst (append block return-lst)))]
    [(? symbol? bexp)
     (let {[block `((add SP SP 1) (move (0 SP) ,bexp))]}
       (set! return-lst (append block return-lst)))]       
    [`(not ,bexp)
     (set! return-lst (append (handle-bexp bexp '()) return-lst))
     (let {[block `((lnot (-1 SP) (-1 SP)))]}
       (set! return-lst (append block return-lst)))]
    [`(and ,bexp ...)
     (set! return-lst (append (handle-bexp (first bexp) '()) return-lst))
     (for [(b (rest bexp))]
       (set! return-lst (append (handle-bexp b '()) return-lst))
       (let {[block `((sub SP SP 1) (land (-2 SP) (-2 SP) (-1 SP)))]}
         (set! return-lst (append block return-lst))))]
    [`(or ,bexp ...)
     (set! return-lst (append (handle-bexp (first bexp) '()) return-lst))
     (for [(b (rest bexp))]
       (set! return-lst (append (handle-bexp b '()) return-lst))
       (let {[block `((sub SP SP 1) (lor (-2 SP) (-2 SP) (-1 SP)))]}
         (set! return-lst (append block return-lst))))]
    [`(,op ,b1 ,b2)
     (set! return-lst (append (handle-bexp b1 '()) return-lst))
     (set! return-lst (append (handle-bexp b2 '()) return-lst))
     (let {[block `((sub SP SP 1) (,(op-trans op) (-2 SP) (-2 SP) (-1 SP)))]}
       (set! return-lst (append block return-lst)))])
  return-lst)
;;==================================================================================================

;;==================================================================================================
;; Level four: iif statement
(define (handle-iif bexp stmt1 stmt2 return-lst iif-ind)
  (begin
    (let*
        {[bexp-block (handle-bexp bexp '())]
         [branch-block `((sub SP SP 1) (branch (-1 SP) ,(iif-generater "_TRUE" iif-ind)))]
         [jump-false-block `((jump ,(iif-generater "_FALSE" iif-ind)))]
         [true-block `((label ,(iif-generater "_TRUE" iif-ind)))]
         [stmt1-block (convert-statements (list stmt1) '())]
         [jump-done-block `((jump ,(iif-generater "_DONE" iif-ind)))]
         [false-block `((label ,(iif-generater "_FALSE" iif-ind)))]
         [stmt2-block (convert-statements (list stmt2) '())]
         [done-block `((label ,(iif-generater "_DONE" iif-ind)))]}
      (set! return-lst (append done-block stmt2-block false-block jump-done-block stmt1-block
                               true-block jump-false-block branch-block bexp-block return-lst)))
    return-lst))
;;==================================================================================================

;;==================================================================================================
;; Level four: while statement
(define (handle-while bexp stmts return-lst while-ind)
  (begin
    (let*
        {[start-block `((label ,(while-generater "_START" while-ind)))]
         [bexp-block (handle-bexp bexp '())]
         [branch-block `((sub SP SP 1) (branch (-1 SP) ,(while-generater "_LOOP" while-ind)))]
         [jump-finish-block `((jump ,(while-generater "_FINISH" while-ind)))]
         [loop-block `((label ,(while-generater "_LOOP" while-ind)))]
         [stmt-block (convert-statements stmts '())]
         [jump-start-block `((jump ,(while-generater "_START" while-ind)))]
         [finish-block `((label ,(while-generater "_FINISH" while-ind)))]}
      (set! return-lst (append finish-block jump-start-block stmt-block loop-block
                               jump-finish-block branch-block bexp-block start-block return-lst)))
    return-lst))
;;==================================================================================================

;;==================================================================================================      
;; Level four: operator helper
(define (op-trans op)
  (match op
    ['+ 'add]
    ['* 'mul]
    ['- 'sub]
    ['div 'div]
    ['mod 'mod]
    ['= 'equal]
    ['> 'gt]
    ['< 'lt]
    ['>= 'ge]
    ['<= 'le]
    [_ (error "op-trans match error" op)]))
;;==================================================================================================
