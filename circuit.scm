;;; 2022 Sept. 02
;;; A simlpe Scheme program to calculate network impedance
;;;
;;; Long He

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

(define (add-element network element)
  (init-network (cons element (all-elements network))))

(define (rm-element network element)
  (init-network (delete element (all-elements network))))

(define (rm-betwn-nodes network nodes)
  (if (null? (sel-element network nodes))
      network
      (rm-betwn-nodes
       (rm-element network (car (sel-element network nodes)))
       nodes)))


(define (neighbors network node)
  (filter
   (lambda(x)
     (not (null? (sel-element network (list x node)))))
   (all-nodes network)))


(define (impedance network node-pair)
  (define (clean? network)
    (= 2 (length (network (all-nodes-option)))))
  )


(define elements
  (map make-element '(((n1 n2) r r1) ((n2 gnd) r r2))))

(define network (init-network elements))
(define network
  (add-element network (make-element '((n2 n3) r r3))))
(define network
  (add-element network (make-element '((n1 n2) r r4))))
;;(define network (add-element network (list 'n2 'gnd) 'r 'r2))

