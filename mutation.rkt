#lang racket
(require "auto.rkt")
(provide mutate
         mutate-population)

;; MUTATION
(define (mutate an-auto)
  (let ([flatten-one (list->vector (flatten-automaton an-auto))]
        [r (random 21)])
    (if (member r (list 1 5 9 13 17))
        (vector-set! flatten-one r (random 3))
        (vector-set! flatten-one r (random 5)))
    (make-automaton (vector->list flatten-one))))

(define (mutate-population m population)
  (let* ([r (for/list ([n m]) (random (length population)))]
         [vectored (list->vector population)]
         [mutated (map (lambda (x)
                         (mutate (list-ref population x)))
                       r)])
    (map (lambda (y z)
           (vector-set! vectored y z))
         r
         mutated)
    (vector->list vectored)))
