#lang racket
;; Naive Relational Algebra in Scheme
;; ==================================
;; See https://en.wikipedia.org/wiki/Relational_algebra for background.

;; Declare that all functions defined in this file are imported
;; when another file uses: (import "relational-algebra.rkt")
(provide (all-defined-out))

;; Implement data types and primitive operators over relations and tables.
;; The provided data types are:
;; Db, Schema, Relation, Table
;; The provided operators are:
;; project, select, product, join, union

;; Signature:schema?(schema)
;; Purpose:detects whether x is a valid schema, 
;; meaning that there arent any duplicates
;; Type:[scheme-expression => boolean]
;; Precondition:x is a list
;; Tests:((schema? (make-schema '(Noam Hila Noam))) => #f)
;;       ((schema? (make-schema '(Noam Hila))) => #t)
(define schema?
  (lambda (x)
    (if (list? x)
    (not (ormap (lambda (e)
                (if (symbol? e) (member e (cdr (member e x))) #t)) x))
    (symbol? x))))

;; Signature:relation?(x)
;; Purpose:verify that an object is a valid Relation:
;;- The relation schema is valid
;;- The rows are all consistent with the schema (same length as the schema)
;; Type:[scheme-expression => boolean]
;; Precondition:x is a list
;; Tests:((relation? (make-relation (make-schema '(S_StudentId S_Name S_Course) '(0 'Avi 'CS330))) => #t)
(define relation?
  (lambda (x)
    (cond [(not (list? x)) #f]
          [(not (schema? (car x))) #f]
          [(andmap (lambda (row) (= (length (relation-schema x)) (length row))) (relation-rows x)) #t] 
          [else #f]
       )))
  
;; Signature:table?(x)
;; Purpose:verify that an object is a valid Table
;; Type:[scheme-expression => boolean]
;; Precondition:x is a list
;; Tests:()
(define table? 
  (lambda (x)
    (if (list? x)
    (and (symbol? (table-name x)) (relation? (table-relation x)))
    #f)
    ))
  
  
;; Signature:database?(x)
;; Purpose:verify that an object is a valid Database
;;- Every table in the database is valid
;;- There are no tables in the database that have the same name
;;- The schema of all the table relations are pairwise disjoint
;; Type:[scheme-expression => boolean]
;; Precondition:x is a list
;; Tests:()
(define database? 
  (lambda (x)
    (cond [(not (list? x)) #f]
          [(not (andmap table? x)) #f];checks if all tables are valid
          [(ormap (lambda (e)
                (member e (cdr (member e (map car x))))) (map car x)) #f]
          [(not (schema? (flatten (foldr cons '() (map table-schema x))))) #f]
          [else #t]
    )))

  
  
;; Types used in defining a database:
;; Schema = List(Symbol)	-- the name of the fields in a relation
;; Row = List				-- heterogeneous list of values
;; Relation = Pair(Schema, List(Row))
;; Table = Pair(Symbol, Relation)
;; Database = List(Table)
;;
;; General note: to avoid dealing with renaming - we impose in databases
;;               that all attribute names are distinct across tables.
;;               We do not specify and check the type of attributes in relations.
;;               Values in rows are atomic (number, symbol, boolean)
;;               Symbols appear as quoted symbols inside the rows.
;;               Attributes appear as simple symbols.

;; Concrete representation of a sample database
(define DB1
  '(
    (Students
     (S_StudentId S_Name S_Course)
     (0 'Avi 'CS330)
     (1 'Yosi 'CS142)
     (2 'Sarah 'CS630))
    (Projects
     (P_ProjectId P_Course P_Title)
     (0 'CS330 'Intro)
     (1 'CS330 'PPL)
     (2 'CS142 'SPL)
     (3 'CS630 'Compilation))
    (Grades
     (G_StudentId G_ProjectId G_Grade)
     (0 0 10)
     (1 2 99)
     (2 3 60))))

(define DB2
    '(
    (Students
     (S_StudentId S_Name S_Course)
     (0 'Avi 'CS330)
     (1 'Yosi 'CS142)
     (2 'Sarah 'CS630))
    (Projects
     (S_StudentId P_Course P_Title)
     (0 'CS330 'Intro)
     (1 'CS330 'PPL)
     (2 'CS142 'SPL)
     (3 'CS630 'Compilation))
    (Grades
     (G_StudentId G_ProjectId G_Grade)
     (0 0 10)
     (1 2 99)
     (2 3 60))))

;; Signature: make-db(tables)
;; Type: [List(Table) -> Database]
;; Purpose:a
;; Precondition: The schemas of all tables are disjoint.
;; Tests: (make-db (list (make-table 'Students 
;;                            (make-schema '(StudentId Name Course))
;;                            (list '(0 'Avi 'CS330)
;;                                  '(1 'Sarah 'CS330)))))
;; ==> db
(define make-db
  (lambda (tables) tables))

;; Signature: db-table(db table)
;; Type: [Db * Symbol -> Table]
;; Purpose: accessor for the Db type.
;; Precondition: a
;; Tests:()
(define db-table
  (lambda (db table)
    (assoc table db)))

;; Signature: make-table(table-name, schema, rows)
;; Type: [Symbol * Schema * List(Row) -> Table]
;; Purpose:a
;; Precondition: all rows are compatible with the schema (same length).
;; Tests: (make-table 'Students 
;;                   (make-schema '(StudentId Name Course))
;;                   (list '(0 'Avi 'CS330)
;;                         '(1 'Yosi 'CS330)))
;; ==> table
(define make-table
  (lambda (table-name schema rows)
    (cons table-name 
          (make-relation schema rows))))

;; Signature: table-name(table)
;; Type: [Table -> Symbol]
;; Purpose:a
;; Precondition: a
;; Tests:()
(define table-name
  (lambda (table)
    (car table)))
;; Signature: table-schema(table)
;; Type: [Table -> Schema]
;; Purpose:a
;; Precondition: a
;; Tests:()
(define table-schema
  (lambda (table)
    (cadr table)))
;; Signature: table-rows(table)
;; Type: [Table -> List(Row)]
;; Purpose:a
;; Precondition: a
;; Tests:()
(define table-rows
  (lambda (table)
    (cddr table)))
;; Signature: table-relation(table)
;; Type: [Table -> Relation]
;; Purpose:a
;; Precondition: a
;; Tests:()
(define table-relation
  (lambda (table)
    (cdr table)))

;; Signature: make-relation(schema, rows)
;; Type: [Schema * List(Row) -> Relation]
;; Purpose:a
;; Precondition: a
;; Tests:()
(define make-relation
  (lambda (schema rows)
    (cons schema rows)))
;; Signature: relation-schema(relation)
;; Type: [Relation -> Schema]
;; Purpose:a
;; Precondition: a
;; Tests:()
(define relation-schema
  (lambda (relation)
    (car relation)))
;; Signature: relation-rows(relation)
;; Type: [Relation -> List(Row)]
;; Purpose:a
;; Precondition: a
;; Tests:()
(define relation-rows
  (lambda (relation)
    (cdr relation)))

;; Signature: make-schema(names)
;; Type: [List(Symbol) -> Schema]
;; Preconditions: all symbols in the schema are distinct
;; Purpose:a
;; Tests:()
(define make-schema
  (lambda (names) names))


;; Row manipulation tools
;; ======================

;; Signature: enumerate(list)
;; Purpose: return a list of the form ((0 . x0) (1 . x1) ...) from a list (x0 x1 ...)
;; Type: [List(T) -> List(Pair(Number,T))
;; Tests: (enumerate '(a b c)) ==> '((0 . a) (1 . b) (2 . c))
(define enumerate
  (lambda (list)
    (letrec ((iter (lambda (i l)
                     (if (empty? l) 
                         empty
                         (cons (cons i (car l)) 
                               (iter (+ i 1) (cdr l)))))))
      (iter 0 list))))

;; Signature: row-accessors(schema)
;; Purpose: construct accessor functions for each field in a schema
;; Type: [list(Symbol) -> List(Pair(Symbol,<closure>))]
;; Test: ((row-accessors (list 'Noam 'Hila 'Noa)) =>
;; ((Noam . #<procedure>) (Hila . #<procedure>) (Noa . #<procedure>)))
(define row-accessors
  (lambda (schema)
    ;; @@ Should use enumerate and map
    (let ((attributes (enumerate schema)))
      (map (lambda (attr-i)
             (cons (cdr attr-i) (lambda (row) (list-ref row (car attr-i)))))
           attributes))
    ))

;; Signature: row-accessor(attribute, schema)
;; Purpose: convert the name of an attribute in schema to a row-accessor
;; Type: [Symbol*List(Symbol) -> Symbol]
;; Test: ((row-accessor 'Noam (list 'Noam 'Noa)) (list 'TheBest 'Second))
;; ==> 
;; 'TheBest
(define row-accessor
  (lambda (attribute schema)
    (let ((accessors (row-accessors schema)))
      (cdr (assoc attribute accessors)))
    ))


;; Relational operators
;; ====================

;; Signature: project(attributes, relation)
;; Purpose: compute the projection of a relation according to the relational algebra definition
;; Type: [List(Symbol) * Relation -> Relation]
;; Precondition: attributes is a subset of relation-schema(relation)
;; Tests:
;; (project '(a c) (make-relation (make-schema '(a b c)) 
;;                                (list '(10 20 30)
;;                                      '(11 21 31))))
;; ==> '((a c) (10 30) (11 31))
;;
;; (project '(c a) (make-relation (make-schema '(a b c)) 
;;                                (list '(10 20 30)
;;                                      '(11 21 31))))
;; ==> '((c a) (30 10) (31 11))
(define project
  (lambda (attributes relation)
    ;; @@ use row-accessor and map
    (cons attributes (map (lambda (row)
           (map (lambda (attribute)
                  ((row-accessor attribute (relation-schema relation)) row))
           attributes))
           (relation-rows relation)))             
    ))

;; Signature: select(predicate, relation)
;; Purpose: compute the selection of a relation according to the relational algebra definition
;; Type: [[Row -> Boolean] * Relation -> Relation]
;; Precondition:a
;; Tests:
;; (let ((relation (make-relation (make-schema '(a b c)) 
;;                                (list '(10 20 30)
;;                                      '(11 21 31)))))
;;     (select (lambda (row) (> ((row-accessor 'a (relation-schema relation)) row) 10)) relation)) 
;; ==> '((a b c) (11 21 31))
(define select
  (lambda (predicate relation)
    ;; @@ use filter
    (cons (relation-schema relation) (filter predicate (relation-rows relation)))
    ))

;; Signature: product(relation1, relation2)
;; Purpose: compute the cartesian product of 2 relations according to the relational algebra definition
;; Type: [Relation * Relation -> Relation]
;; Precondition: the schemas of relation1 and relation2 are disjoint.
;; Tests:
;; (product (make-relation (make-schema '(a b)) (list '(10 11) '(20 21)))
;;          (make-relation (make-schema '(x y)) (list '('u 'v) '('p 'q))))
;; ==> ((a b x y)
;;      (10 11 'u 'v)
;;      (10 11 'p 'q)
;;      (20 21 'u 'v)
;;      (20 21 'p 'q))
(define product
  (lambda (relation1 relation2)
    ;; @@ Use map and foldr
     (make-relation (append (relation-schema relation1) (relation-schema relation2))
                    (foldr append (list) (map (lambda (x)
                            (map (lambda (y)
                                   (append x y)) 
                                          (relation-rows relation2)))
                                              (relation-rows relation1)))
                    )
    ))

;; Signature: join(relation1, relation2, predicate)
;; Purpose: compute the join of 2 relations according to the relational algebra definition
;; Type: [Relation * Relation * [Row -> Boolean] -> Relation]
;; Precondition: the schemas of relation1 and relation2 are disjoint.
;; Tests:
;; (let ((schema1 (make-schema '(a b)))
;;       (schema2 (make-schema '(x y)))
;;       (schemap (make-schema '(a b x y))))
;;   (let ((row-a (row-accessor 'a schemap))
;;         (row-x (row-accessor 'x schemap)))
;;     (join (make-relation (make-schema '(a b)) (list '(10 11) '(20 21)))
;;           (make-relation (make-schema '(x y)) (list '(10 'v) '(20 'q)))
;;           (lambda (row) 
;;             (= (row-a row) (row-x row))))))
;; ==> ((a b x y)
;;      (10 11 10 'v)
;;      (20 21 20 'q))
(define join 
  (lambda (relation1 relation2 predicate)
    ;; @@ Compose product and select
    (select predicate (product relation1 relation2))
    ))

;; Signature: union(relation1, relation2)
;; Purpose: compute the union of 2 relations according to the relational algebra definition
;; Type: [Relation * Relation -> Relation]
;; Precondition: the schemas of relation1 and relation2 are identical.
;; Tests:
;; (union (make-relation (make-schema '(a b)) (list '(10 11)))
;;        (make-relation (make-schema '(a b)) (list '(20 21))))
;; ==> ((a b) (10 11) (20 21))
;; We can return the same values for the rows more than once:
;; (union (make-relation (make-schema '(a b)) (list '(10 11) '(20 21)))
;;        (make-relation (make-schema '(a b)) (list '(20 21))))
;; ==> ((a b) (10 11) (20 21) (20 21))
(define union
  (lambda (relation1 relation2)
    ;; @@
    (cons (relation-schema relation1) (append (relation-rows relation1) (relation-rows relation2)))
    ))




(display "==========================\n")
(display "Testing relational algebra\n")
(display "==========================\n")


(make-db (list (make-table 'Students 
                           (make-schema '(StudentId Name Course))
                           (list '(0 'Avi 'CS330)
                                 '(1 'Sarah 'CS330)))))

(make-table 'Students 
                   (make-schema '(StudentId Name Course))
                   (list '(0 'Avi 'CS330)
                         '(1 'Yosi 'CS330)))

(project '(a c) (make-relation (make-schema '(a b c)) 
                               (list '(10 20 30)
                                     '(11 21 31))))
;; ==> '((a c) (10 30) (11 31))

(project '(c a) (make-relation (make-schema '(a b c)) 
                               (list '(10 20 30)
                                     '(11 21 31))))
;; ==> '((c a) (30 10) (31 11))

(let ((relation (make-relation (make-schema '(a b c)) 
                               (list '(10 20 30)
                                     '(11 21 31)))))
  (select (lambda (row) (> ((row-accessor 'a (relation-schema relation)) row) 10)) relation))
;; ==> '((a b c) (11 21 31))

(product (make-relation (make-schema '(a b)) (list '(10 11) '(20 21)))
         (make-relation (make-schema '(x y)) (list '('u 'v) '('p 'q))))
;; ==> ((a b x y)
;;      (10 11 'u 'v)
;;      (10 11 'p 'q)
;;      (20 21 'u 'v)
;;      (20 21 'p 'q))


(let ((schema1 (make-schema '(a b)))
      (schema2 (make-schema '(x y)))
      (schemap (make-schema '(a b x y))))
  (let ((row-a (row-accessor 'a schemap))
        (row-x (row-accessor 'x schemap)))
    (join (make-relation (make-schema '(a b)) (list '(10 11) '(20 21)))
          (make-relation (make-schema '(x y)) (list '(10 'v) '(20 'q)))
          (lambda (row) 
            (= (row-a row) (row-x row))))))
;; ==> ((a b x y)
;;      (10 11 10 'v)
;;      (20 21 20 'q))

(union (make-relation (make-schema '(a b)) (list '(10 11)))
       (make-relation (make-schema '(a b)) (list '(20 21))))
;; ==> ((a b) (10 11) (20 21))

(union (make-relation (make-schema '(a b)) (list '(10 11) '(20 21)))
       (make-relation (make-schema '(a b)) (list '(20 21))))
;; ==> ((a b) (10 11) (20 21) (20 21))

