#lang racket

(struct bin (op fst snd) #:transparent) 			
(struct fun (param body) #:transparent)				
(struct app (fn arg) #:transparent)					
(struct closure (var body envt) #:transparent)		 
(struct seq (fst snd) #:transparent)				
(struct newbox (exp)#:transparent)					
(struct openbox (exp)#:transparent) 				
(struct setbox (bexp vexp) #:transparent)			
(struct result (val newstore) #:transparent)		
(struct sub (name loc)#:transparent)				
(struct store (loc val)#:transparent)		

(provide (all-defined-out))