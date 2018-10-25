#lang racket

(provide primp-assemble h)

(define h (make-hash))

(define op-list
  '(add sub mul div mod gt ge lt le equal not-equal land lor))

(define (list-of-list? x)
  (and (list? x) (list? (first x))))

(define (unpack v)
  (if (and (list? v) (eq? (first v) 'lit)) (second v) v))

(define (preprocess lst i)
  (when (not (empty? lst))
    (match (car lst)
      [`(label ,n)
       (begin (if (hash-has-key? h n)
                  (error "duplicate" n)
                  (hash-set! h n (list 'target i 'footprint))) 
              (preprocess (cdr lst) i))]
      [`(const ,n ,v)
       (begin (if (hash-has-key? h n)
                  (error "duplicate" n)
                  (if (number? v)
                      (hash-set! h n (list 'const (unpack v) 'footprint))
                      (hash-set! h n (list 'const (unpack v) 'untainted))))
              (preprocess (cdr lst) i))]
      [`(data ,n ,v ...)
       (begin (if (hash-has-key? h n)
                  (error "duplicate" n)
                  (hash-set! h n (list 'data i 'footprint)))
              (cond
                [(list-of-list? v) ;; case 1
                 (preprocess (cdr lst) (+ (first (first v)) i))]
                [else ;; case 2
                 (preprocess (cdr lst) (+ (length v) i))]))]
      [_ (preprocess (cdr lst) (+ i 1))])))
                

(define (inspect h)
  (define (step key)
    (let {[triplet (hash-ref h key)]}
      (hash-set! h key (list (car triplet) (second triplet) 'footprint))))
  
  (define (inspect-key k h lst)
    (define v (hash-ref h k (λ() (error "undefined: inspect-key" k)))) ;; v is a pair (triplet now)
    (define val (second v))
    (define state (third v))
    (cond
      [(or (number? val) (boolean? val) (equal? state 'footprint))
       (begin (if (equal? state 'footprint)
                  (map step lst)
                  (map step (cons k lst)))
              (void))]
      
      [(symbol? val)
       (if (member val lst)
           (error "circular")
           (inspect-key val h (cons val lst)))]
      [else (error "incorrect: nothing matched in inspect" k)]
      ))
  (begin (hash-map h (λ (k v) (inspect-key k h '()))) (void))
  )
    
(define (clean-ht h) ;; applied after inspect
  (define (trace-elem k v) ;; eg. k = C1, v = (const C2 'footprint)
    (if (symbol? (second v))
        (let* {[triplet (hash-ref h (second v))]
               [final (trace-elem (second v) triplet)]}
          (hash-set! h k (const-wrap final))
          final)
        (cdr v)))

  (define (const-wrap v)
    (cons `const v))
  
  (hash-for-each h trace-elem))
  
(define (cons-array lst out)
  (if (empty? lst) out
      (cons-array (rest lst) (cons (replace-data (first lst)) out))))

(define (cons-repeat n v out)
  (if (zero? n) out
      (cons-repeat (sub1 n) v (cons (replace-data v) out))))

(define (assemble lst out)
  (if (not (empty? lst))
      (let {[stmt (car lst)]}
        ; (printf"~a\n" stmt)
        (match stmt
          [`(label ,n) (assemble (cdr lst) out)]
          [`(halt) (assemble (cdr lst) (cons 0 out))]
          [`(const ,n ,v) (assemble (cdr lst) out)]
          [`(data ,n ,v ...)
           (cond
             [(and (list? v) (list? (car v)) (= (length (car v)) 1)) (assemble (cdr lst) (cons (replace-opd v) out))] 
             [(list-of-list? v) ;; case 1
              (assemble (cdr lst) (cons-repeat (first (first v)) (unpack (second (first v))) out))]
             [else ;; case 2
              (assemble (cdr lst) (cons-array v out))])]
          [`(lit, v) (assemble (cdr lst) (cons (replace-data (unpack v)) out))]
          [`(jump ,t) (assemble (cdr lst) (cons `(jump ,(replace-tgt (unpack t))) out))]
          [`(branch ,o ,t) (assemble (cdr lst) (cons `(branch ,(replace-opd (unpack o)) ,(replace-tgt (unpack t))) out))]
          [`(move ,t1 ,t2) (assemble (cdr lst) (cons `(move ,(replace-tgt (unpack t1)) ,(replace-tgt (unpack t2))) out))]
          [`(print-val ,o) (assemble (cdr lst) (cons `(print-val ,(replace-opd (unpack o))) out))]
          [`(print-string ,str) (assemble (cdr lst) (cons `(print-string ,(replace-str (unpack str))) out))]
          [`(lnot ,d ,o) (assemble (cdr lst) (cons `(lnot ,(replace-dest (unpack d)) ,(replace-opd (unpack o))) out))]
          [`(,opt ,d ,o1 ,o2)
           (begin ((λ(x) (unless (member x op-list) (error "incorrect: not in op-list" x))) opt)
                  (assemble (cdr lst)
                            (cons `(,opt ,(replace-dest (unpack d))
                                         ,(replace-opd (unpack o1))
                                         ,(replace-opd (unpack o2))) out)))]
	  [`(jsr ,d ,o)
	   (assemble (cdr lst) (cons `(jsr ,(replace-dest (unpack d)) ,(replace-opd (unpack o))) out))]
          [x (assemble (cdr lst) (cons x out))])) out))

(define (replace-opd o)
  (let {[v (replace-opd-inner o)]}
    (if (and (list? v) (list? (car v)))
        (cons (caar v) (cdr v))
        v)))

(define (replace-opd-inner o)
  (cond
    [(or (number? o) (boolean? o)) o]
    [(list? o)
     (if (= (length o) 1)
         (map replace-opd o)
         (if (number? (second o)) ;; ASSUMING ((nat) (nat)) is valid offset
             (error "incorrect: replace-opd-inner, list" o)
             (map replace-opd o)))]
    [(symbol? o)
     (let* {[v (hash-ref h o (λ() (error "undefined:replace-opd-inner " o)))]
            [type (first v)]
            [val (second v)]}
       (cond
         
         [(boolean? val) val]
         [(number? val) (if (eq? type 'data) (cons val '()) val)]
         [else (replace-opd val)]))]
    [else (error "incorrect: general, replace-opd-inner" o)]))


(define (replace-data o)
  (let {[v (replace-data-inner o)]}
    (if (and (list? v) (list? (car v)))
        (cons (first (first v)) (cdr v))
        v)))

(define (replace-data-inner o)
  (cond
    [(or (number? o) (boolean? o)) o]
    [(list? o) o]
    [(symbol? o)
     (let* {[v (hash-ref h o (λ() (error "undefined:replace-data-inner  " o)))]
            [type (first v)]
            [val (second v)]}
       (replace-data val))]
    [else (error "incorrect: replace-data-inner, general" o)]))

(define (resolve-offset v)
  (if (and (list? v) (list? (car v)))
        (cons (first (first v)) (cdr v))
        v))

(define (replace-tgt t)
  (let {[v (replace-tgt-inner t)]}
    (resolve-offset v)))

(define (replace-tgt-inner t)
  (cond
    [(and (boolean? t) (number? t)) t]
    [(symbol? t)
     (let* {[v (hash-ref h t (λ() (error "undefined:replace-tgt " t)))]
            [type (first v)]
            [val (second v)]}
       (if (equal? type 'data)
           (list val)
           val))]
    [(list? t) (map replace-opd t)]
    [else t]))


(define (replace-dest o)
  (let {[v (replace-dest-inner o)]}
    (if (and (list? v) (list? (car v)))
        (cons (first (first v)) (cdr v))
        v)))

(define (replace-dest-inner d)
  (cond
    [(list? d)
     (if (= (length d) 1)
         (map replace-opd d)
         (if (number? (second d)) 
             (error "incorrect: replace-dest-inner, list" d)
             (map replace-opd d)) 
         )]
    [(symbol? d)
     (let* {[v (hash-ref h d (λ() (error "undefined:replace-dest-inner " d)))]
            [type (first v)]
            [val (second v)]}
       (if (eq? type 'data)
           (if (number? val) (cons val '()) (replace-dest val))
           (error "incorrect: replace-dest-inner, symbol" d)))]
    [else (error "incorrect: replace-dest-inner, general" d)]))

(define (replace-str str)
  (if (string? str) str (error "incorrect: replace-str")))

(define (primp-assemble lst)
  (begin
    (preprocess lst 0)
    (inspect h)
    (clean-ht h)
    (reverse (assemble lst '()))))

