#lang racket

(require "sql-asp.rkt")
(require "relational-algebra.rkt")
(provide (all-defined-out))


;; Query Language over Relations - Interpreter
;; ===========================================

;; Define a relational query language and its interpreter.

;; Query Examples:
;; Given a database like relation-algebra.DB1
;; the following queries are well defined:
(define q1 '(select (S_StudentId S_Name) from Students where (S_StudentId = 0)))
(define q2 '(select (S_StudentId G_Grade) from (Students product Grades) where (G_Grade < 90)))
(define q3 '(select (S_StudentId G_Grade) from (Students join Grades on (S_StudentId = G_StudentId)) where (G_Grade > 56)))
(define q4 '(select (S_StudentId S_Course G_Grade) from (Students join 
                                                                  (Projects join Grades on (P_ProjectId = G_ProjectId))
                                                                  on (S_StudentId = G_StudentId))))


;; Interpreter
;; ===========
;; Evaluation rules (using primitives from relational-algebra):
;; 
;; eval-query(query db):
;;   let attributes = query-attributes(query)
;;       relation = eval-relation(query-relation(query) db)
;;       pred = eval-predicate(query-predicate(query) relation-schema(relation))
;;     eval-query(query db) = (project attributes (select pred relation))
;;
;; eval-relation(relation db):
;;   I.  table-var?(relation): (table-relation (db-table (table-var-table relation) db))
;;   II. composite?(relation):
;;      1. product?(relation):
;;         eval-relation(relation db) = (product (eval-relation (product-relation1 relation) db)
;;                                               (eval-relation (product-relation2 relation) db))
;;      2. join?(relation):
;;         let rel1 = (eval-relation (join-relation1 relation) db)
;;             rel2 = (eval-relation (join-relation2 relation) db)
;;         eval-relation(relation db) = (join rel1 rel2 
;;                                            (eval-predicate (join-predicate relation)
;;                                                            (make-schema (append (relation-schema rel1)
;;                                                                                 (relation-schema rel2)))))
;; 
;; eval-predicate(pred schema):
;;   let arg1 = eval-operand(predicate-op1(pred) schema)
;;       arg2 = eval-operand(predicate-op2(pred) schema)
;;       operator = eval-operator(predicate-operator(pred))
;;     eval-predicate(pred schema) = (lambda (row) (operator (arg1 row) (arg2 row)))
;;
;; eval-operand(op schema):
;;   I.  attribute?(op):  
;;       eval-operand(op schema) = row-accessor(op schema)
;;   II. primitive?(op): 
;;       eval-operand(op db) = (lambda (row) op)
;;
;; eval-operator(op):
;;   1. op = 'eq?: eval-operator(op) = equal?
;;   2. op = '<:   eval-operator(op) = <
;;   3. op = '>:   eval-operator(op) = >
;;   4. op = '=:   eval-operator(op) = =




;; Signature: eval-query(query db)
;; Type: [Query * Db -> Relation]
(define eval-query
  (lambda (query db)
    (let* ([attributes (query-attributes query)]
           [relation (eval-relation (query-relation query) db)]
           [pred (eval-predicate (query-predicate query) (relation-schema relation))])
      (project attributes (select pred relation)))
    ))
           

;; Signature: eval-relation(relation-exp db)
;; Type: [Relation-exp -> Relation]
(define eval-relation
  (lambda (relation-exp db)
    (cond [(table-var? relation-exp) (table-relation (db-table db (table-var-table relation-exp)))]
          [(composite-relation? relation-exp)
                (cond [(product? relation-exp)
                       (product (eval-relation (product-relation1 relation-exp) db)
                                (eval-relation (product-relation2 relation-exp) db))]
                      [(join? relation-exp)
                       (let ((rel1 (eval-relation (join-relation1 relation-exp) db))
                             (rel2 (eval-relation (join-relation2 relation-exp) db)))
                         (join rel1 rel2 (eval-predicate (join-predicate relation-exp)
                                               (make-schema (append (relation-schema rel1) (relation-schema rel2))))))]
                      [(union? relation-exp)
                       (union (eval-relation (union-relation1 relation-exp) db)
                              (eval-relation (union-relation2 relation-exp) db))]
                      )])))
                       
                                      
                                               


;; Signature: eval-predicate(op)
;; Type: [Symbol -> [T * T -> Boolean]]
(define eval-predicate
  (lambda (pred schema)
    (if (empty? pred) (lambda (x) #t)
    (let ((arg1 (eval-operand (predicate-op1 pred) schema))
          (arg2 (eval-operand (predicate-op2 pred) schema))
          (operator (eval-operator (predicate-operator pred))))
      (lambda (row) (operator (arg1 row) (arg2 row)))))))
    

;; Signature: eval-operand(op)
;; Type: [Symbol -> [Row -> T]]
;; Purpose: convert an operand to a function that accesses the operand on a row.
;; - If the operand is a primitive, the function has the form (lambda (row) constant)
;; - Else it has the form (lambda (row) (accessor row))
(define eval-operand
  (lambda (op schema)
    (if (attribute? op)
        (let ((accessor (row-accessor op schema)))
          (lambda (row) (accessor row)))
        (lambda (row) op))))
    
;; Signature: eval-operator(op)
;; Type: [Symbol -> [T * T -> Boolean]]
(define eval-operator
  (lambda (op)
    (cond ((eq? op 'eq?) equal?)
          ((eq? op '<) <)
          ((eq? op '>) >)
          ((eq? op '=) =))))


;; ================================================
;; Examples
(display "==================\n")
(display "Testing eval-query\n")
(display "==================\n")

 (eval-query (parse-query q1) DB1)
 (eval-query (parse-query q2) DB1)
 (eval-query (parse-query q3) DB1)
 (eval-query (parse-query q4) DB1)