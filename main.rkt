(require "auto.rkt"
         "fit.rkt"
         "match.rkt"
         "mutation.rkt"
         "scan.rkt"
         "tv.rkt"
	"out.rkt")


;; generate population
(define (create-population N)
  (for/list
      ([n N])
    (generate-auto)))

(define A (create-population 100))



(define population-mean '())
(define demographic '())
;; evolve the population over cycles
;; N=100
(define (evolve population cycles speed mutation rounds-per-match)
  (let* ([N (length population)]
         [demo (scan-types population)]
         [round-results (match-population population rounds-per-match)]
         [average-payoff (exact->inexact (/ (apply + (flatten round-results))
                                            (* rounds-per-match N)))]
         [accum-fitness (accumulate (payoff-percentages (flatten round-results)))]
         [survivors (drop population speed)]
         [successors
          (randomise-over-fitness accum-fitness population speed)]
         [before-mutation
          (shuffle (append survivors successors))]
         [new-population
          (mutate-populations mutation before-mutation)]
         )
    (set! population-mean
          (append population-mean (list average-payoff)))
    (out-rank cycles population)
     ;; (set! demographic
     ;;       (append demographic (list demo)))
    (if (zero? cycles)
        (begin
          (plot-mean population-mean)
          (out-mean population-mean))
        (evolve new-population (sub1 cycles) speed mutation rounds-per-match))))
