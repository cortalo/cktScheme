
(define (same-op-type? query-op query-type op type)
  (and (eq? query-op op) (eq? query-type type)))

(define (table-append op type item old-table)
  (lambda (query-op query-type)
    (if (same-op-type? query-op query-type op type)
        item
        (old-table query-op query-type))))

(define (table op type) false)


(define (put op type item)
  (set! table (table-append op type item table)))

(define (get op type)
  (table op type))
