;;; 2022 Sept. 02
;;; A simlpe Scheme program to calculate network impedance, symbolically!
;;;
;;; Long He
;;; helong_ms@outlook.com

(define nil ())

(define (all-nodes-option) 'all-nodes)


;;; determine if two pairs are symbolic euqal
;;; they can be the same order or different order.
(define (pair-eq? first second)
  (cond ((eq? (car first) (car second))
         (eq? (cadr first) (cadr second)))
        ((eq? (car first) (cadr second))
         (eq? (cadr first) (car second)))
        (else
         (eq? (car first) (car second)))))

;;; return if ele in set s
(define (in-set ele s)
  (cond ((null? s) false)
        ((eq? (car s) ele) true)
        (else (in-set ele (cdr s)))))

;;; remove element from set s
(define (rm-set element s)
  (cond ((null? s) s)
        ((eq? (car s) element) (cdr s))
        (else (cons (car s) (rm-set element (cdr s))))))

(define (-set s1 s2)
  (if (null? s2)
      s1
      (-set (rm-set (car s2) s1) (cdr s2))))

;;; merge two set into one set
(define (make-set new-set old-set)
  (cond ((null? new-set) old-set)
        ((in-set (car new-set) old-set)
         (make-set (cdr new-set) old-set))
        (else
         (make-set (cdr new-set) (cons (car new-set) old-set)))))

;;; add one new-element to the old-values of the parallel value
(define (make-parallel new-element old-values)
  (if (null? old-values)
      (list '|| new-element)
      (cons '|| (cons new-element (cdr old-values)))))


;;; =====================
;;; Abstraction: ELEMENT
;;;
(define (make-element element)
  element)

(define element-nodes car)

(define element-type cadr)

(define element-name caddr)


(define (parallel-vals elements)
  (cons '|| (map element-name elements)))
;;;
;;; Abstraction: ELEMENT
;;; =======================


;;; =========================
;;; Abstraction Barriers: NETWORK
;;; =========================
;;; network constructor
;;; (init-network (list 'gnd 'n1) 'r 'r1)

(define (init-network elements)
  elements)

;;; element selector
;;; return a list of elements
(define (sel-element network node-pair)
  (define (same-node-pair? element)
    (pair-eq? (element-nodes element) node-pair))
  (filter same-node-pair? network))

(define (all-elements network) network)

;;; all-nodes selector
(define (all-nodes network)
  (if (null? network)
      nil
      (make-set (element-nodes (car network)) (all-nodes (cdr network)))))

;;; =========================
;;; Abstraction Barriers: NETWORK
;;; =========================


(define (other-nodes all-set nodes)
  (-set all-set nodes))

(define (add-element network element)
  (init-network (cons element (all-elements network))))

(define (rm-element network element)
  (init-network (delete element (all-elements network))))

(define (neighbors network node)
  (filter
   (lambda(x)
     (not (null? (sel-element network (list x node)))))
   (all-nodes network)))


(define (rm-betwn-nodes network nodes)
  (if (null? (sel-element network nodes))
      network
      (rm-betwn-nodes
       (rm-element network (car (sel-element network nodes)))
       nodes)))



(define (rm-series-node network node)
  (let* ((nodeA (car (neighbors network node)))
        (nodeB (cadr (neighbors network node)))
        (valA (parallel-vals (sel-element network (list node nodeA))))
        (valB (parallel-vals (sel-element network (list node nodeB))))
        (valSeries (list '+ valA valB))
        (eleSeries (make-element (list (list nodeA nodeB) 'r valSeries))))
    (rm-betwn-nodes (rm-betwn-nodes (add-element network eleSeries) (list node nodeB)) (list node nodeA))
    )
  )


(define (rm-open-node network node)
  (let* ((nodeA (car (neighbors network node))))
    (rm-betwn-nodes network (list node nodeA))))

(define (reduce-network network node-pair)
  (define (series? node)
    (= 2 (length (neighbors network node))))
  (define (open? node)
    (= 1 (length (neighbors network node))))
  (define (series-nodes network node-pair)
    (filter series? (other-nodes (all-nodes network) node-pair)))
  (define (open-nodes network node-pair)
    (filter open? (other-nodes (all-nodes network) node-pair)))
  (define (series-node network node-pair)
    (car (series-nodes network node-pair)))
  (define (open-node network node-pair)
    (car (open-nodes network node-pair)))
  (define (has-series-node network node-pair)
    (not (null? (series-nodes network node-pair))))
  (define (has-open-node network node-pair)
    (not (null? (open-nodes network node-pair))))
  (cond ((has-open-node network node-pair)
         (rm-open-node network (open-node network node-pair)))
        ((has-series-node network node-pair)
         (rm-series-node network (series-node network node-pair)))
        (else
         'fail-reduce-network))
  )


(define (impedance network node-pair)
  (define (clean? network)
    (= 2 (length (all-nodes network))))
  (if (clean? network)
      (parallel-vals (sel-element network node-pair))
      (impedance (reduce-network network node-pair) node-pair))
  )


;;; TODO
;;; pattern matching to simplify result

;;; Testing
(define elements
  (map make-element '(((n1 n2) r r1) ((n2 gnd) r r2))))

(define network (init-network elements))
(define network
  (add-element network (make-element '((gnd n1) r r3))))

(define network
  (add-element network (make-element '((gnd n3) r r4))))

(define network
  (add-element network (make-element '((n1 n3) r r5))))

(define network
  (add-element network (make-element '((n2 n3) r r6))))

