#lang racket

(provide (all-defined-out))

; +----------------------+
; | Answer Q4-a.1 below  |
; +----------------------+

; Signature: filter1$(pred$,seq,cont)
; Type: 
; Purpose: 
; Pre-conditions:
; Tests:
(define filter1$
  (lambda (pred$ seq cont)
    ...))

; +----------------------+
; | Answer Q4-a.3 below  |
; +----------------------+

; Signature: filter2$(pred,seq,cont)
; Type: 
; Purpose: 
; Pre-conditions:
; Tests:
(define filter2$
  (lambda (pred seq cont)
    (cond ((empty? seq) (cont empty))
          ((pred (car seq))
           (filter2$ pred (cdr seq) (lambda (filtered_cdr)
                                      (cont (cons (car seq) filtered_cdr)))))
          (else (filter2$ pred (cdr seq) (lambda (filtered_cdr)
                                           (cont filtered_cdr)))))))

; +----------------------+
; |    Answer Q4-b.1     |
; +----------------------+

;Signature: get-value$(assoc-list, key, success, fail)
;Type: [List(Pair(Symbol,T))*Symbol*[Pair(Symbol,T1)->T2] *
;        [Empty->T3]] -> T2 union T3)
;Purpose: Find the value of 'key'. If 'key' is found, then apply the 
;         continuation 'success' to the pair (key . val). Otherwise, 
;         apply the continuation 'fail'.
;Pre-conditions:
;Tests:
;Examples:(get-value$ '((a . 3) (b . 4)) 'b (lambda(x)x) (lambda()#f)) 
;          --> (b . 4) 
(define get-value$
  (lambda (assoc-list key success fail)
    ...))

; +----------------------+
; |    Answer Q4-b.2     |
; +----------------------+

;Signature: get-first-value(list-assoc-lists, key)
;Type: [List(Association-list)*Symbol -> T]
;Purpose: Retrieves the value of 'key' in the first association-list that includes 'key'. If no such value, the Scheme error function is applied.
;Pre-conditions:
;Tests:
;Examples: 	> (define l1 '((a . 1) (b . 2) (e . 3)))
;		> (define l2 '((e . 5) (f . 6)))
;		> (get-first-value (list l1 l2), 'e)
;               3
;		> (get-first-value (list l1 l2), 'g)
;               ‘()
(define get-first-value
  (lambda (assoc-list key)
    ...))


;Signature: collect-all-values(list-assoc-lists, key)
;Type: [List(Assoc-list)*Symbol -> T]
;Purpose: Returns a list of all values of 'key' in the given association lists. If no such value, returns the empty list.  
;Pre-conditions:
;Tests:
;Examples: 	> (define l1 '((a . 1) (b . 2) (e . 3)))
;		> (define l2 '((e . 5) (f . 6))) 
;		> (get-first-value (list l1 l2), 'e)
;               ‘(3 5)
;		> (get-first-value (list l1 l2), 'k)
;		‘()
(define collect-all-values
  (lambda (assoc-list key)
    ...))