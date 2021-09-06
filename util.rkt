#lang racket

(provide (all-defined-out))

(define (split-into lst n)
  (define-values (head tail) (split-at lst n))
  (if (null? tail)
      (list head)
      (cons head (split-into tail n))))

(define (toss-coin?)
  (zero? (random 2)))

(define (list-random-ref list)
  ;; Faster than the generic `random-ref` in `racket/random'.
  (let loop ((l list) (acc '()) (len 0))
    (if (null? l)
        (list-ref acc (random len))
        (loop (cdr l) (cons (car l) acc) (add1 len)))))

(define (dict-ref-in dict keys (not-found #f))
  (if (null? keys)
      dict
      (dict-ref-in (dict-ref dict (car keys)) (cdr keys) not-found)))

(define (dict-refs dict keys (not-found #f))
  (if (null? keys)
      '()
      (cons (dict-ref dict (car keys) not-found)
            (dict-refs dict (cdr keys) not-found))))

(define (dict-update-in dict keys function . args)
  (dict-update dict (car keys)
               (if (null? (cdr keys))
                   (λ (v) (apply function v args))
                   (λ (v) (apply dict-update-in v (cdr keys) function args)))))

(define (dict-set-in dict keys value)
  (dict-update-in dict keys (thunk* value)))
