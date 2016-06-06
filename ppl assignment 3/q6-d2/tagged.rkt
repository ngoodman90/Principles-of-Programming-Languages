#lang racket
;; Tagged ADT
(provide (all-defined-out))

;; Type: Symbol * T -> Tagged(Symbol, T)
(define make-tagged
  (lambda (tag content)
    (cons tag content)))

;; Type: T -> Boolean
(define tagged?
  (lambda (obj)
    (and (pair? obj)
         (symbol? (car obj)))))

;; Type: Symbol -> T -> Boolean
(define tagged-by?
  (lambda (tag)
    (lambda (obj)
      (and (tagged? obj)
           (eq? (car obj) tag)))))

;; Type: Tagged(Symbol, T) -> Symbol
(define tagged->tag car)

;; Type: Tagged(Symbol, T) -> T
(define tagged->content cdr)

