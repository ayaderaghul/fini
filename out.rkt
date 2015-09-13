#lang racket
(require "csv.rkt")
(require "scan.rkt")
(provide out-data
         out-mean
         out-rank)

;; data:
;; '((1 2..)
;;   (2 3..))

;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

(define (out-mean data)
  (out-data "mean" (map list data)))

(define (out-rank day population)
  (out-data "rank" (append (list (list day))
                           (map list (rank-flattened population)))))
