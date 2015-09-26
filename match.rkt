#lang racket
(require "auto.rkt")
(provide match-claims
         match-pair
         match-population)

(define (match-claims claims)
  (if (<= (apply + claims) 2)
      (map convert-payoff claims)
      (list 0 0)))

(define (convert-payoff x)
  (cond [(= x -1) 0]
        [(= x 0) 1]
        [(= x 1) 5]
        [(= x 2) 9]))

(define (match-pair* auto1 auto2 results previous-claims countdown)
  (if (zero? countdown)
      results
      (let ([result-state1 (jump-to-state (last previous-claims) auto1)]
            [result-state2 (jump-to-state (car previous-claims) auto2)]
            [reaction1 (react (last previous-claims) auto1)]
            [reaction2 (react (car previous-claims) auto2)])
        (match-pair* (update auto1 result-state1)
                     (update auto2 result-state2)
                     (append results (list
                                      (match-claims previous-claims)))
                     (list reaction1 reaction2)
                     (sub1 countdown)))))

;; match a pair of automaton for n rounds
;; return a list of round results
(define (match-pair automaton-pair rounds-per-match)
  (match-pair* (car automaton-pair)
               (last automaton-pair)
               '()
               (map current-claim automaton-pair)
               rounds-per-match))

;; in each match, take mean of round results for each automaton
;; returns a pair of means
(define (take-sums round-results)
  (map (lambda (f) (apply +  (map f round-results)))
       (list first second)))

(define (match-population population rounds-per-match)
  (for/list ([i (/ (length population)
                   2)])
    (take-sums
     (match-pair (list
                  (list-ref population (* 2 i))
                  (list-ref population (add1 (* 2 i))))
                 rounds-per-match))))
