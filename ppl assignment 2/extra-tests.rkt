#lang racket

(require "utils.rkt")
(require "relational-algebra.rkt")
(require "sql-interpreter.rkt")
(require "sql-asp.rkt")

(define DB2
  '(
    (Students
     (S_StudentId S_Name S_Course)
     (0 'Avi 'CS330)
     (1 'Yosi 'CS142)
     (2 'Sarah 'CS630))
    (Students2
     (S_StudentId S_Name S_Course)
     (3 'David 'CS350)
     (4 'Moshe 'PSY342)
     (5 'Eli 'ENG632))
    (Students3
     (S_StudentId S_Name S_Course)
     (3 'David 'CS350)
     (12 'Moshe 'PSY342)
     (5 'Eli 'ENG632))
    (Students4
     (S_StudentId S_Name S_Course)
     (0 'David 'CS350)
     (12 'Moshe 'PSY342)
     (5 'Eli 'ENG632))
    ))

(define qu0 '(select (S_StudentId S_Name S_Course) from (Students union Students2) where (S_StudentId = 0)))
(define qu1 '(select (S_StudentId S_Name S_Course) from (Students union Students2) where (S_StudentId = 3)))
(define qu2 '(select (S_StudentId S_Name S_Course) from (Students union Students2)))
(define qu3 '(select (S_Course) from (Students union Students2)))
(define qu4 '(select (S_StudentId S_Course) from (Students2 union Students) where (S_StudentId > 2)))
(define qu5 '(select (S_Course S_StudentId) from (Students2 union Students3) where (S_StudentId > 2)))
(define qu6 '(select (S_Course S_StudentId) from (Students2 union (Students3 union Students)) where (S_StudentId > 0)))
(define qu7 '(select (S_Name) from ((Students2 union Students4) union (Students3 union Students)) where (S_StudentId > 0)))

;; sql-interpreter tests
(define (union-tests)
  (run-tests
   (test (eval-query (parse-query qu0) DB2) => '((S_StudentId S_Name S_Course) (0 'Avi 'CS330)))
   (test (eval-query (parse-query qu1) DB2) => '((S_StudentId S_Name S_Course) (3 'David 'CS350)))
   (test (eval-query (parse-query qu2) DB2) => '((S_StudentId S_Name S_Course) (0 'Avi 'CS330) (1 'Yosi 'CS142) (2 'Sarah 'CS630) (3 'David 'CS350) (4 'Moshe 'PSY342) (5 'Eli 'ENG632)) )
   (test (eval-query (parse-query qu3) DB2) => '((S_Course) ('CS330) ('CS142) ('CS630) ('CS350) ('PSY342) ('ENG632)))
   (test (eval-query (parse-query qu4) DB2) => '((S_StudentId S_Course) (3 'CS350) (4 'PSY342) (5 'ENG632)))
   (test (eval-query (parse-query qu5) DB2) => '((S_Course S_StudentId) ('CS350 3) ('PSY342 4) ('ENG632 5) ('CS350 3) ('PSY342 12) ('ENG632 5)))
   (test (eval-query (parse-query qu6) DB2) => '((S_Course S_StudentId) ('CS350 3) ('PSY342 4) ('ENG632 5) ('CS350 3) ('PSY342 12) ('ENG632 5) ('CS142 1) ('CS630 2)))
   (test (eval-query (parse-query qu7) DB2) => '((S_Name) ('David) ('Moshe) ('Eli) ('Moshe) ('Eli) ('David) ('Moshe) ('Eli) ('Yosi) ('Sarah)))
   ))

;; type-predicates tests
(define (type-predicates-tests)
  (run-tests
   (test (schema? 1) => #f)
   (test (schema? #t) => #f)
   (test (relation? 255) => #f)
   (test (relation? 'a) => #f) 
   (test (table? 100) => #f)
   (test (table? 'Moshe) => #f)    
   (test (database? 354) => #f)
   (test (database? #f) => #f)    
   (test (schema? (make-schema '(a b c d e))) => #t)  
   (test (schema? (make-schema '(a b c d a))) => #f)     
   (test (schema? (make-schema (list 'XYZ 'Moshe 'XYZ))) => #f) 
   (test (schema? (make-schema (list 1 2 3 4))) => #f)
   (test (relation? (make-relation (make-schema '(a b c)) (list '(10 20 30) '(11 21 31)))) => #t)   
   (test (relation? (make-relation (make-schema '(a b c)) (list (list 10 #t 'abc) '(11 21 31)))) => #t)
   (test (relation? (make-relation (make-schema '(a b c)) (list (list 10 #t 'abc) '(11 21 31 'a)))) => #f)   
   (test (table? (make-table 'Students (make-schema '(StudentId Name Course))(list '(0 'Avi 'CS330) '(1 'Yosi 'CS330)))) => #t)     
   (test (table? (make-table 1 (make-schema '(StudentId Name Course))(list '(0 'Avi 'CS330) '(1 'Yosi 'CS330)))) => #f)   
   (test (table? (make-table #f (make-schema '(StudentId Name Course))(list '(0 'Avi 'CS330) '(1 'Yosi 'CS330)))) => #f)
   (test (database?   '((Students
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
                         (2 3 60)))) => #t)
      (test (database?   '((Students
                         (S_StudentId S_Name S_Course)
                         (0 'Avi 'CS330)
                         (1 'Yosi 'CS142)
                         (2 'Sarah 'CS630))
                        (Students
                         (G_StudentId G_ProjectId G_Grade)
                         (0 0 10)
                         (1 2 99)
                         (2 3 60)))) => #f) 
      (test (database?   '((Students
                         (S_StudentId S_Name S_Course)
                         (0 'Avi 'CS330)
                         (1 'Yosi 'CS142)
                         (2 'Sarah 'CS630 1))
                        (ABC
                         (G_StudentId G_ProjectId G_Grade)
                         (0 0 10)
                         (1 2 99)
                         (2 3 60)))) => #f)   
      (test (database?   '((Students
                         (S_StudentId G_Grade S_Course)
                         (0 'Avi 'CS330)
                         (1 'Yosi 'CS142)
                         (2 'Sarah 'CS630 1))
                        (ABC
                         (G_StudentId G_ProjectId G_Grade)
                         (0 0 10)
                         (1 2 99)
                         (2 3 60)))) => #f)       
))


(display "\n\n===================\n")
(display "union tests:\n")
(display "===================\n")
(union-tests)
(display "\n\n===================\n")
(display "type predicates tests:\n")
(display "===================\n")
(type-predicates-tests)