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
    (cond [(empty? seq) (cont empty)]
          [(pred$ (car seq)
                  (lambda (pred-res)
                    (if pred-res (filter1$ pred$ (cdr seq)
                                           (lambda (filtered_cdr)
                                                  (cont (cons (car seq) filtered_cdr))))
                        (filter1$ pred$ (cdr seq)
                                           cont))
                    ))]
          )))





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
          (else (filter2$ pred (cdr seq) cont)))))

; +----------------------+
; |    Answer Q4-b.1     |
; +----------------------+

;Signature: get-value$(assoc-list, key, success, fail)
;Type: [List(Pair(Symbol,T))*Symbol*[Pair(Symbol,T1)->T2] *
;        [Empty->T3]] -> T2 union T3)
;Purpose: Find the value of 'key'. If 'key' is found, then apply the 
;         continuation 'success' to the pair (key . val). Otherwise, 
;         apply the continuation 'fail'.
;Pre-conditions: assoc-list is a list of pairs
;Tests:
;Examples:(get-value$ '((a . 3) (b . 4)) 'b (lambda(x)x) (lambda()#f)) 
;          --> (b . 4) 
(define get-value$
  (lambda (assoc-list key success fail)
    (cond [(empty? assoc-list) (fail)]
          [(equal? (caar assoc-list) key)
              (success (car assoc-list))]
          [else (get-value$ (cdr assoc-list) key success fail)])
    ))
        
        

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
    (let ([success (lambda (x) (cdr x))]
          [fail (lambda () '(key not in list))])
      (cond [(empty? assoc-list) ((lambda () '()))]
            [(equal? '(key not in list) (get-value$ (car assoc-list) key success fail))
                (get-first-value (cdr assoc-list) key)]
            [else (get-value$ (car assoc-list) key success fail)])
      )))


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
    (let ([success (lambda (x) (cdr x))]
          [fail (lambda () '(key not in list))])
    (cond [(empty? assoc-list) ((lambda () '()))]
            [(equal? '(key not in list) (get-value$ (car assoc-list) key success fail))
                (collect-all-values (cdr assoc-list) key)]
            [else (cons (get-value$ (car assoc-list) key success fail) (collect-all-values (cdr assoc-list) key))])
      )))
