#lang racket

(require "type-expression-adt.rkt" "stack.rkt" "utils.rkt")
(provide (all-defined-out))

;; ============================================================
;; TE parser

;; Purpose: Parse concrete syntax into TE abstract syntax
;; Type: [List union Symbol -> TE]
;; Example:
;; (te-parse 'T)                   => 'T
;; (te-parse '(T -> T))            => (-> T T)
;; (te-parse '(T * T -> T))        => (-> (* T T) T)
;; (te-parse '(T -> T -> T))       => (-> T (-> T T))
;; (te-parse '((T -> T) * T -> T)) => (-> (* (-> T T) T) T)
;; (te-parse '(T -> T * T -> T))   => (-> (* (-> T T) T) T)
;; (te-parse '(Empty -> T)         => (-> Empty-tuple-te T)
(define te-parse
  (lambda (e)
    (cond ((empty? e) (error 'te-parse "Bad TE concrete syntax: '()"))
          ((eq? e 'Number) e)
          ((eq? e 'Boolean) e)
          ((eq? e 'Empty) (make-empty-tuple-te))
          ((symbol? e) e)
          ((list? e) (te-parse-composite e))
          (else (error 'te-parse
                       "Bad TE concrete syntax - unexpected type: ~s" e)))))

;; Use shift-reduce algorithm to turn the infix notation
;; of the composite term expressions to abstract syntax.
;; -> / -> : Right associative (shift)
;; * / *   : Reduce tuple
;; * / ->  : Left associative (reduce)
;; -> / *  : Left associative (reduce)
(define te-parse-composite
  (lambda (l)
    (let ((param-stack (make-stack))
          (op-stack (make-stack)))
      (letrec
          (;; Shift-reduce engine
           (iter
            (lambda (l pstack ostack)
              ;; (display (format "Parse Params: ~s Operators: ~s Queue: ~s\n"
              ;;                 pstack ostack l))
              (cond
               ((empty? l) (check-stacks pstack ostack))
               ((eq? (car l) '->)
                (cond ((stack-empty? ostack)
                       (iter (cdr l) pstack (stack-push ostack '->)))
                      ((eq? (stack->top ostack) '->)
                       (iter (cdr l) pstack (stack-push ostack '->)))
                      ((eq? (stack->top ostack) '*)
                       (iter (cdr l) (reduce-tuple pstack)
                             (stack-push (stack-pop ostack) (car l))))))
               ((eq? (car l) '*)
                (cond ((stack-empty? ostack)
                       (iter (cdr l) pstack (stack-push ostack '*)))
                      ((eq? (stack->top ostack) '->)
                       (iter (cdr l) (reduce-proc pstack)
                             (stack-push (stack-pop ostack) (car l))))
                      ((eq? (stack->top ostack) '*)
                       (iter (cdr l) (reduce-tuple pstack)
                             (stack-push (stack-pop ostack) (car l))))))
               (else (let ((first (te-parse (car l))))
                       (iter (cdr l) (stack-push pstack first) ostack))))))
           ;; reduce-tuple: reduce the param stack by combining the top 2
           ;;               types into a tuple.
           ;; Configuration: pstack = [p1 (tuple l2) ...]
           ;;                return = [(tuple l2+p1) ...]
           ;; Configuration: pstack = [p1 p2 ...]
           ;;                return = [(tuple (list p2 p1)) ...]
           (reduce-tuple
            (lambda (pstack)
              (if (< (stack-length pstack) 2)
                  (error 'reduce-tuple "Bad proc expression: ~s" l)
                  (let ((p1 (first pstack))
                        (p2 (second pstack))
                        (pstack-2 (stack-pop (stack-pop pstack))))
                    (if (or (empty-tuple-te? p1)
                            (empty-tuple-te? p2))
                        (error 'reduce-tuple
                               "Cannot combine Empty in a tuple ~s" l)
                        (if (tuple-te? p2)
                            (stack-push
                             pstack-2
                             (make-tuple-te
                              (append (tuple-te->components p2) (list p1))))
                            (stack-push
                             pstack-2
                             (make-tuple-te (list p2 p1)))))))))
           ;; reduce-proc
           ;; Configuration: pstack = [p1 p2 ...]
           ;;                return = [(p2 -> p1) ...]
           (reduce-proc
            (lambda (pstack)
              (if (< (stack-length pstack) 2)
                  (error 'reduce-proc "Bad proc expression: ~s" l)
                  (let ((p1 (first pstack))
                        (p2 (second pstack))
                        (pstack-2 (stack-pop (stack-pop pstack))))
                    (stack-push pstack-2 (make-proc-te p2 p1))))))
           ;; reduce-ostack
           ;; Configuration: When the queue is empty, ostack not empty
           ;;                repeatedly reduce operators from the stack
           (reduce-ostack
            (lambda (pst ost)
              (cond ((stack-empty? ost) pst)
                    ((eq? (stack->top ost) '->)
                     (reduce-ostack (reduce-proc pst)
                                    (stack-pop ost)))
                    ((eq? (stack->top ost) '*)
                     (reduce-ostack (reduce-tuple pst)
                                    (stack-pop ost))))))
           ;; check-stacks
           ;; Complete parse when:
           ;; - ostack is empty and pstack contains 1 proc-te only
           ;; - or ostack is not empty, and reduce it till empty
           ;;   and obtain a single composite in pstack.
           (check-stacks
            (lambda (pstack ostack)
              (let ((pst (reduce-ostack pstack ostack)))
                ;; (display (format "End of parse: pstack ~s ostack ~s\n"
                ;;                 pstack ostack))
                ;; (display (format "              reduced ~s\n" pst))
                (if (and (stack-empty? (stack-pop pst))
                         (composite-te? (stack->top pst)))
                    (stack->top pst)
                    (error 'check-stacks "Bad proc-te: ~s" l))))))
        ;; Iterate over the incoming proc-te expression infix->prefix
        (iter l param-stack op-stack)))))


;; ============================================================
;; Serialize TE into a readable expression
;; This is the inverse operation of te-parse
;; It returns a fully parenthesized infix form of the procedures.

;; Type: [TE -> List union Symbol]
;; Example:
;; (te-concrete (te-parse 'T))                     => 'T
;; (te-concrete (te-parse '(T -> T)))              => '(T -> T)
;; (te-concrete (te-parse '(T * T -> T)))          => '(T * T -> T)
;; (te-concrete (te-parse '(T -> T -> T)))         => '(T -> (T -> T))
;; (te-concrete (te-parse '(Empty -> T)))          => '(Empty -> T)
;; (te-concrete (te-parse '(T * T -> T * T -> T))) => '((T * T -> T) * T -> T)
(define te-concrete
  (lambda (te)
    (cond ((atomic-te? te) te)
          ((type-variable? te) te)
          ((tuple-te? te) (tuple-te-concrete te))
          ((proc-te? te) (proc-te-concrete te)))))

;; Type: [Tuple-te -> List]
;; Purpose: turn the abstract syntax of a tuple into its concrete syntax rep.
;; Example: (tuple-te-concrete (make-tuple-te (list 'Number 'Number)))
;;          ==> (Number * Number)
;;          (tuple-te-concrete (make-empty-tuple-te))
;;          ==> (Empty)
;;          (tuple-te-concrete (make-tuple-te (list 'Number)))
;;          ==> (Number)
(define tuple-te-concrete
  (letrec
      ;; Turn a list (a b c ...z) into (a * b * c * ... * z)
      ((ls->lsdelim
        (lambda (l delim)
          (cond ((empty? l) l)
                ((empty? (cdr l)) l)
                (else (cons (car l)
                            (cons delim
                                  (ls->lsdelim (cdr l) delim))))))))
    (lambda (tte)
      (if (empty-tuple-te? tte)
          (list 'Empty)
          (ls->lsdelim (map te-concrete
                            (tuple-te->components tte)) '*)))))

(define proc-te-concrete
  (lambda (pte)
    (append (te-concrete (proc-te->parameters pte))
            (list '->
                  (te-concrete (proc-te->return pte))))))
