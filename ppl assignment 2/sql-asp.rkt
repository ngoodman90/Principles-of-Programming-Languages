#lang racket
;; Query Language over Relations - AST and ASP
;; ===========================================

;; Declare that all functions defined in this file are imported
;; when another file uses: (import "sql-ask.rkt")
(provide (all-defined-out))

;; Define a relational query language and its interpreter.

;; BNF
;; Abstract Syntax
;; Interpreter rules:
;; - Primitives
;; - Composite expressions

;; Query Examples:
;; Given a database:
;; (define DB1
;;  '(
;; 	  (Students
;;		(S_StudentId S_Name S_Course)
;;		(0 Avi 'CS330)
;;		(1 Yosi 'CS142)
;;		(2 Sarah 'CS630))
;;	  (Projects
;;		(P_ProjectId P_Course P_Title)
;;		(0 CS330 'Intro)
;;		(1 CS330 'PPL)
;;		(2 CS142 'SPL)
;;		(3 CS630 'Compilation))
;;	  (Grades
;;		(G_StudentId G_ProjectId G_Grade)
;;		(0 0 10)
;;		(1 2 99)
;;		(2 3 60))))
;; 
;; The following queries are well defined:
;; (select (S_StudentId S_Name) from Students where (S_StudentId = 0))
;; (select (S_StudentId G_Grade) from (Students product Grades) where (G_Grade < 90))
;; (select (S_StudentId G_Grade) from (Students join Grades on (S_StudentId = G_StudentId)) where (G_Grade > 56))
;; (select (S_StudentId S_Course G_Grade) from (Students join 
;;                                                (Projects join Grades on (P_ProjectId = G_ProjectId))
;;                                                on (S_StudentId = G_StudentId)))
;;
;; Note that relation expressions can be embedded in a recursive manner - predicates cannot.

;; ===
;; BNF
;; ===
;; <query> -> '(' 'select' '(' <attribute>+ ')' 'from' <relation-exp> ['where' <predicate>] ')'			[] means optional - appears 0 or 1 times
;; <attribute> -> symbol
;; <relation-exp> -> <table> | <composite-relation>
;; <composite-relation> -> '(' <relation-exp> 'product' <relation-exp> ')' | '(' <relation-exp> 'join' <relation-exp> 'on' <predicate> ')' | '(' <relation-exp> 'union' <relation-exp> ')'
;; <predicate> -> '(' <operand> <operator> <operand> ')'
;; <operand> -> <attribute> | <primitive>
;; <primitive> -> boolean | number | <quoted-symbol>
;; <quoted-symbol> -> '(' 'quote' symbol ')'
;; <boolean> -> '#t' | '#f'
;; <op> -> '=' | 'eq?' | '>' | '<'


;; ===============
;; Abstract syntax
;; ===============
;; <query> :: Components: attributes, relation-exp, predicate
;; <relation-exp> :: Kinds: <table-var>, <composite-relation>
;; <attribute> :: symbol
;; <table-var> :: symbol
;; <composite-relation> ::  <product> | <join> | <union>
;; <product> :: Components: relation1: <relation-exp>, relation2: <relation-exp>
;; <join> :: Components: relation1, relation2: <relation-exp>, predicate: <predicate>
;; <union> :: Components: relation1, relation2: <relation-exp>
;; <predicate> :: Components: op1, op2: <operand>, operator: <op>
;; <operand> :: Kinds: <attribute>, <primitive>
;; <primitive> :: Kinds: <boolean> | <quoted-symbol> | <number>
;; <op> :: Kinds: a member of '(= eq? < >)


;; Signature: make-query(attributes relation predicate)
;; Purpose: value constructor for type Query
;; Type: [List(Attribute) * Relation-exp * Predicate -> Query]
(define make-query
  (lambda (attributes relation predicate)
    (list 'query attributes relation predicate)))
;; Signature: query?(x)
;; Purpose: type predicate for type Query
;; Type: [T -> Boolean]
(define query?
  (lambda (x) (and (list? x) (not (empty? x)) (eq? (car x) 'query))))
;; Signature: query-attributes(x)
;; Purpose: accessor for type Query
;; Type: [Query -> List(Attribute)]
(define query-attributes
  (lambda (x) (second x)))
;; Signature: query-relation(x)
;; Purpose: accessor for type Query
;; Type: [Query -> Relation-exp]
(define query-relation
  (lambda (x) (third x)))
;; Signature: query-predicate(x)
;; Purpose: accessor for type Query
;; Type: [Query -> Predicate]
(define query-predicate
  (lambda (x) (fourth x)))

;; Signature: relationexp?(x)
;; Purpose: type predicate for abstract type Relation-exp
;; Type: [T -> Boolean]
(define relation-exp?
  (lambda (x)
    (or (table-var? x)
        (composite-relation? x))))
;; Signature: composite-relation?(x)
;; Purpose: type predicate for abstract type Composite-relation
;; Type: [T -> Boolean]
(define composite-relation?
  (lambda (x)
    (or (product? x) (join? x) (union? x))))

;; Signature: make-table-var(table)
;; Purpose: value constructor for type Table-var
;; Type: [Symbol -> Table-var]
(define make-table-var
  (lambda (table)
    (list 'table-var table)))
;; Signature: table-var?(x)
;; Purpose: type predicate for type Table-var
;; Type: [T -> Boolean]
(define table-var?
  (lambda (x) (and list? x) (not (empty? x)) (eq? (car x) 'table-var)))
;; Signature: table-var-table(x)
;; Purpose: accessor for type Table-var
;; Type: [Table-var -> Symbol]
(define table-var-table
    (lambda (table-var) (second table-var)))


;; Signature: make-product(relation1 relation2)
;; Purpose: value constructor for type Product
;; Type: [Relation-exp * Relation-exp -> Product]
(define make-product
  (lambda (relation1 relation2)
    (list 'product relation1 relation2)))
;; Signature: product?(x)
;; Purpose: type predicate for type Product
;; Type: [T -> Boolean]
(define product?
  (lambda (x) (and (list? x) (not (empty? x)) (eq? (car x) 'product))))
;; Signature: product-relation1(x)
;; Purpose: accessor for type Product
;; Type: [Product -> Relation-exp]
(define product-relation1
  (lambda (x)
     (second x)))
;; Signature: product-relation2(x)
;; Purpose: accessor for type Product
;; Type: [Product -> Relation-exp]
(define product-relation2
   (lambda (x)
     (third x)))


;; Signature: make-join(relation1 relation2 predicate)
;; Purpose: value constructor for type Join
;; Type: [Relation-exp * Relation-exp * Predicate -> Join]
(define make-join
  (lambda (relation1 relation2 predicate)
    (list 'join relation1 relation2 predicate)))
;; Signature: join?(x)
;; Purpose: type predicate for type Join
;; Type: [T -> Boolean]
(define join?
  (lambda (x) (and (list? x) (not (empty? x)) (eq? (car x) 'join))))
;; Signature: join-relation1(x)
;; Purpose: accessor for type Join
;; Type: [Join -> Relation-exp]
(define join-relation1
  (lambda (x)
     (second x)))
;; Signature: join-relation2(x)
;; Purpose: accessor for type Join
;; Type: [Join -> Relation-exp
(define join-relation2 third)
;; Signature: join-predicate(x)
;; Purpose: accessor for type Join
;; Type: [Join -> Predicate]
(define join-predicate fourth)


;; Signature: make-union(relation1 relation2)
;; Purpose: value constructor for type Union
;; Type: [Relation-exp * Relation-exp -> Union]
(define make-union
  (lambda (relation1 relation2)
    (list 'union relation1 relation2)))
;; Signature: union?(x)
;; Purpose: type predicate for type Union
;; Type: [T -> Boolean]
(define union?
  (lambda (x) (and (list? x) (not (empty? x)) (eq? (car x) 'union))))
;; Signature: union-relation1(x)
;; Purpose: accessor for type Union
;; Type: [Union -> Relation-exp]
(define union-relation1
  (lambda (x)
     (second x)))
;; Signature: union-relation2(x)
;; Purpose: accessor for type Union
;; Type: [Union -> Relation-exp
(define union-relation2 third)


;; Signature: make-predicate(op1 op2 op)
;; Purpose: value constructor for type Predicate
;; Type: [Operand * Operand * Operator -> Predicate]
(define make-predicate
  (lambda (op1 op2 op)
    (list 'predicate op1 op2 op)))
;; Signature: predicate?(x)
;; Purpose: type predicate for type Predicate
;; Type: [T -> Boolean]
(define predicate?
  (lambda (x) (and (list? x) (not (empty? x)) (eq? (car x) 'predicate))))
;; Signature: predicate-op1(x)
;; Purpose: accessor for type Predicate
;; Type: [Predicate -> Operand]
(define predicate-op1 second)
;; Signature: predicate-op2(x)
;; Purpose: accessor for type Predicate
;; Type: [Predicate -> Operand]
(define predicate-op2
  (lambda (x)
     (third x)))
;; Signature: predicate-op(x)
;; Purpose: accessor for type Predicate
;; Type: [Predicate -> Operator]
(define predicate-operator
  (lambda (x)
     (fourth x)))

;; Signature: quoted-symbol?(x)
;; Type: [T -> Boolean]
;; Purpose: type predicate for type Quoted-symbol
(define quoted-symbol? 
  (lambda (x) (and (list? x) (not (empty? x)) (eq? (car x) 'quote) (symbol? (cadr x)))))
;; Signature: attribute?(x)
;; Type: [T -> Boolean]
;; Purpose: type predicate for type Attribute
(define attribute? symbol?)
;; Signature: primitive?(x)
;; Type: [T -> Boolean]
;; Purpose: type predicate for abstract type Primitive
(define primitive?
  (lambda (x) (or (boolean? x) (quoted-symbol? x) (number? x))))
;; Signature: operand?(x)
;; Type: [T -> Boolean]
;; Purpose: type predicate for abstract type Operand
(define operand?
  (lambda (x) (or (attribute? x) (primitive? x))))
;; Signature: operator?(x)
;; Type: [T -> Boolean]
;; Purpose: type predicate for type Operator
(define operator?
  (lambda (x) (member x '(= eq? < >))))


;; ======================
;; Abstract Syntax Parser
;; ======================

;; This parser assumes the input is syntactically correct.
;; It returns the AST of the concrete syntax.

;; Signature: parse-query(exp)
;; Type: [List -> Query]
;; Purpose: convert the concrete syntax of a query into its AST.
;; Precondition: the concrete syntax is a legal expression according to the Query BNF
;; Example: (parse-query '(select (a b) from T where (a > b)))
;; ==> '(query (a b) (table-var T) (predicate a b >))
;; <query> -> '(' 'select' '(' <attribute>+ ')' 'from' <relation-exp> ['where' <predicate>] ')'			[] means optional - appears 0 or 1 times
(define parse-query
  (lambda (exp)
    (let ((attributes (second exp))
          (relation (fourth exp))
          (predicate (if (> (length exp) 5) (sixth exp) empty)))
      (make-query 
       attributes 
       (parse-relation relation)
       (if (empty? predicate) empty (parse-predicate predicate))))))

;; Signature: parse-predicate(exp)
;; Type: [List -> Predicate]
;; Purpose: convert the concrete syntax of a predicate to its AST
;; Precondition: exp is a legal predicate according to the Query BNF
;; Example: (parse-predicate '(a > b))
;; ==> '(predicate a b >)
;; <predicate> -> '(' <operand> <operator> <operand> ')'
(define parse-predicate
  (lambda (exp)
    (let ((op1 (first exp))
          (op (second exp))
          (op2 (third exp)))
      (make-predicate op1 op2 op))))

;; Signature: parse-relation(exp)
;; Type: [List -> Relation-exp]
;; Purpose: convert the concrete syntax of a relation to its AST
;; Precondition: exp is a legal relation according to the Query BNF
;; Example: 
;; (parse-relation 'T) ==> '(table-var T)
;; (parse-relation '(Students product Grades)) ==> '(product (table-var Students) (table-var Grades))
(define parse-relation
  (lambda (exp)
    (cond 
      ((symbol? exp) (make-table-var exp))
      ((eq? (second exp) 'product) (parse-product exp))
      ((eq? (second exp) 'join) (parse-join exp))
      ((eq? (second exp) 'union) (parse-union exp))
      (else (error "Bad relation expression" exp)))))

;; Signature: parse-product(exp)
;; Type: [List -> Product]
;; Purpose: convert the concrete syntax of a product relation to its AST
;; Precondition: exp is a legal product according to the Query BNF
;; Example: 
;; (parse-product '(Students product Grades)) 
;; ==> '(product (table-var Students) (table-var Grades))
(define parse-product
  (lambda (exp)
    (let ((rel1 (first exp))
          (rel2 (third exp)))
      (make-product (parse-relation rel1) (parse-relation rel2)))))

;; Signature: parse-join(exp)
;; Type: [List -> Join]
;; Purpose: convert the concrete syntax of a join relation to its AST
;; Precondition: exp is a legal join according to the Query BNF
;; Example: 
;; (parse-join '(Students join Grades on (S_Sid = G_Sid))) 
;; ==> '(join (table-var Students) (table-var Grades) (predicate S_Sid G_Sid =))
(define parse-join
  (lambda (exp)
    (let ((rel1 (first exp))
          (rel2 (third exp))
          (pred (fifth exp)))
      (make-join 
       (parse-relation rel1) 
       (parse-relation rel2)
       (parse-predicate pred)))))


;; Signature: parse-union(exp)
;; Type: [List -> Union]
;; Purpose: convert the concrete syntax of a union relation to its AST
;; Precondition: exp is a legal join according to the Query BNF
;; Example:

;;not a good example
;; (parse-union '(Students union Grades on (S_Sid = G_Sid))) 
;; ==> '(join (table-var Students) (table-var Grades) (predicate S_Sid G_Sid =))
(define parse-union
  (lambda (exp)
    (let ((rel1 (first exp))
          (rel2 (third exp)))
      (make-union
       (parse-relation rel1)
       (parse-relation rel2)))))







;; ==============================
;; For debugging: AST to Concrete
;; ==============================
;; Linearize ASTs into concrete syntax (inverse operation of parse).

;; Signature: query->concrete(query)
;; Type: [Query -> List]
(define query->concrete
  (lambda (query)
    (let ((prefix (list 'select (query-attributes query) 'from (relation->concrete (query-relation query)))))
      (if (empty? (query-predicate query))
          prefix
          (append prefix (list 'where (predicate->concrete (query-predicate query))))))))
          

;; Signature: relation->concrete(relation)
;; Type: [Relation-exp -> List]
(define relation->concrete
  (lambda (relation)
    (cond ((table-var? relation) (table-var-table relation))
          ((product? relation) (list (relation->concrete (product-relation1 relation)) 
                                     'product 
                                     (relation->concrete (product-relation2 relation))))
          ((join? relation) (list (relation->concrete (join-relation1 relation)) 
                                     'join
                                     (relation->concrete (join-relation2 relation))
                                     'on
                                     (predicate->concrete (join-predicate relation))))
          ((union? relation) (list (relation->concrete (union-relation1 relation)) 
                                     'union
                                     (relation->concrete (union-relation2 relation))))
          )))

;; Signature: predicate->concrete(pred)
;; Type: [Predicate -> List]
(define predicate->concrete
  (lambda (pred)
    (list (predicate-op1 pred) (predicate-operator pred) (predicate-op2 pred))))


;; Examples:

(display "===================\n")
(display "Testing parse-query\n")
(display "===================\n")

(parse-query '(select (S_StudentId S_Name) from Students where (S_StudentId = 0)))
(parse-query '(select (S_StudentId G_Grade) from (Students product Grades) where (G_Grade < 90)))
(parse-query '(select (S_StudentId G_Grade) from (Students join Grades on (S_StudentId = G_StudentId)) where (G_Grade > 56)))
(parse-query '(select (S_StudentId S_Course G_Grade) from (Students join 
                                                                    (Projects join Grades on (P_ProjectId = G_ProjectId))
                                                                    on (S_StudentId = G_StudentId))))


(query->concrete (parse-query '(select (S_StudentId S_Name) from Students where (S_StudentId = 0))))
(query->concrete (parse-query '(select (S_StudentId G_Grade) from (Students product Grades) where (G_Grade < 90))))
(query->concrete (parse-query '(select (S_StudentId G_Grade) from (Students join Grades on (S_StudentId = G_StudentId)) where (G_Grade > 56))))
(query->concrete (parse-query '(select (S_StudentId S_Course G_Grade) from (Students join 
                                                                    (Projects join Grades on (P_ProjectId = G_ProjectId))
                                                                    on (S_StudentId = G_StudentId)))))
