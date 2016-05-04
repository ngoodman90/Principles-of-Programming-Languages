#lang racket

(require "utils.rkt")
(require "relational-algebra.rkt")
(require "sql-interpreter.rkt")
(require "sql-asp.rkt")

;; sql-interpreter tests
(define (sql-tests)
  (run-tests
   (test (eval-query (parse-query q2) DB1) => '((S_StudentId G_Grade) (0 10) (0 60) (1 10) (1 60) (2 10) (2 60)) )
   (test (eval-query (parse-query q3) DB1) => '((S_StudentId G_Grade) (1 99) (2 60)) )
   (test (eval-query (parse-query q4) DB1) => '((S_StudentId S_Course G_Grade) (0 'CS330 10) (1 'CS142 99) (2 'CS630 60)) )
   ))

;; relational-algebra tests
(define (algebra-tests)
  (run-tests
   (test (let ((relation (make-relation (make-schema '(a b c)) 
                                        (list '(10 20 30)
                                              '(11 21 31)))))
           (select (lambda (row) (> ((row-accessor 'a (relation-schema relation)) row) 10)) relation)) => '((a b c) (11 21 31)))
   
   (test (project '(a c) (make-relation (make-schema '(a b c)) 
                                        (list '(10 20 30)
                                              '(11 21 31)))) => '((a c) (10 30) (11 31)) )
   
   (test (project '(c a) (make-relation (make-schema '(a b c)) 
                                        (list '(10 20 30)
                                              '(11 21 31)))) => '((c a) (30 10) (31 11)))
   (test (product (make-relation (make-schema '(a b)) (list '(10 11) '(20 21)))
                  (make-relation (make-schema '(x y)) (list '('u 'v) '('p 'q)))) => '((a b x y) (10 11 'u 'v) (10 11 'p 'q) (20 21 'u 'v) (20 21 'p 'q)))
   
   (test (let ((schema1 (make-schema '(a b)))
               (schema2 (make-schema '(x y)))
               (schemap (make-schema '(a b x y))))
           (let ((row-a (row-accessor 'a schemap))
                 (row-x (row-accessor 'x schemap)))
             (join (make-relation (make-schema '(a b)) (list '(10 11) '(20 21)))
                   (make-relation (make-schema '(x y)) (list '(10 'v) '(20 'q)))
                   (lambda (row) 
                     (= (row-a row) (row-x row)))))) => '((a b x y) (10 11 10 'v) (20 21 20 'q)))
   
   (test (union (make-relation (make-schema '(a b)) (list '(10 11)))
                (make-relation (make-schema '(a b)) (list '(20 21)))) => '((a b) (10 11) (20 21)))
   (test (enumerate '(a b c)) => '((0 . a) (1 . b) (2 . c)) )
   (test (letrec ((row-b (row-accessor 'b '(a b c))))
           (row-b '(1 2 3))) => 2)
   (test (make-db (list (make-table 'Students 
                                    (make-schema '(StudentId Name Course))
                                    (list '(0 'Avi 'CS330)
                                          '(1 'Sarah 'CS330))))) => '((Students (StudentId Name Course) (0 'Avi 'CS330) (1 'Sarah 'CS330))))
   (test (make-table 'Students 
                     (make-schema '(StudentId Name Course))
                     (list '(0 'Avi 'CS330)
                           '(1 'Yosi 'CS330))) => '(Students (StudentId Name Course) (0 'Avi 'CS330) (1 'Yosi 'CS330)))
   ))

;; relational-algebra tests
(define (sql-asp-tests)
  (run-tests
   (test (parse-query '(select (S_StudentId S_Course G_Grade) from (Students join 
                                                                             (Projects join Grades on (P_ProjectId = G_ProjectId))
                                                                             on (S_StudentId = G_StudentId)))) =>
                                                                                                               '(query
                                                                                                                 (S_StudentId S_Course G_Grade)
                                                                                                                 (join (table-ref Students) (join (table-ref Projects) (table-ref Grades) (predicate P_ProjectId G_ProjectId =)) (predicate S_StudentId G_StudentId =))
                                                                                                                 ()))
   (test (parse-query '(select (S_StudentId G_Grade) from (Students join Grades on (S_StudentId = G_StudentId)) where (G_Grade > 56))) =>
         '(query (S_StudentId G_Grade) (join (table-ref Students) (table-ref Grades) (predicate S_StudentId G_StudentId =)) (predicate G_Grade 56 >)))
   (test (parse-query '(select (S_StudentId G_Grade) from (Students product Grades) where (G_Grade < 90))) =>
         '(query (S_StudentId G_Grade) (product (table-ref Students) (table-ref Grades)) (predicate G_Grade 90 <)))
   (test (parse-query '(select (S_StudentId S_Name) from Students where (S_StudentId = 0))) =>
         '(query (S_StudentId S_Name) (table-ref Students) (predicate S_StudentId 0 =)))
   (test (query->concrete (parse-query '(select (S_StudentId S_Name) from Students where (S_StudentId = 0)))) =>
         '(select (S_StudentId S_Name) from Students where (S_StudentId = 0)))
   (test (query->concrete (parse-query '(select (S_StudentId G_Grade) from (Students product Grades) where (G_Grade < 90)))) =>
         '(select (S_StudentId G_Grade) from (Students product Grades) where (G_Grade < 90)))
   (test (query->concrete (parse-query '(select (S_StudentId G_Grade) from (Students join Grades on (S_StudentId = G_StudentId)) where (G_Grade > 56)))) =>
         '(select (S_StudentId G_Grade) from (Students join Grades on (S_StudentId = G_StudentId)) where (G_Grade > 56)))
   (test (query->concrete (parse-query '(select (S_StudentId S_Course G_Grade) from (Students join 
                                                                                              (Projects join Grades on (P_ProjectId = G_ProjectId))
                                                                                              on (S_StudentId = G_StudentId))))) =>
                                                                                                                                 '(select (S_StudentId S_Course G_Grade) from
                                                                                                                                          (Students join (Projects join Grades on (P_ProjectId = G_ProjectId)) on
                                                                                                                                                    (S_StudentId = G_StudentId))))   
   ))


(display "\n\n===================\n")
(display "acceptance tests:\n")
(display "===================\n")
(display "   sql-interpreter tests:\t")
(sql-tests)
(display "   sql-asp tests:\t\t")
(sql-asp-tests)
(display "   relational-algebra tests:\t")
(algebra-tests)