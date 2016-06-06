#lang racket

(provide (all-defined-out))
;; Auxiliaries


;; Flatten a heterogeneous list
(define flatten
  (lambda (tree)
    (if (not (list? tree))
        (list tree)
        (foldl append (list) (map flatten tree)))))

(define set-equal?
  (lambda (set1 set2)
    (and (list? set1)
         (list? set2)
         (eq? (length set1) (length set2))
         (empty? (filter (lambda (el) (not (member el set2)))
                         set1))
         (empty? (filter (lambda (el) (not (member el set1)))
                         set2)))))

;; Purpose: assoc-lst is a list of 2-element lists: ((ind1 val1)... (indn valn)).
;;          returns the value associated with index, or #f if no such pair.
;; Note the confusion with (index #f) pairs.
(define val-of-index
  (lambda (index assoc-lst)
    (let ((index-val-pair (assoc index assoc-lst)))
      (if index-val-pair
          (cadr index-val-pair)
          #f))))
