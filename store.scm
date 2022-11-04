;; store.scm

;;complexity of vector-ref?

(define the-store! 'uninitialized)
(define store-size! 'uninitialized)   ;;Ask MATT what is the best design practice for the store datastructure
(define STORE-MAXSIZE 1000)

(define copy-vector (lambda [small-v big-v idx]
    (if (= idx (vector-length small-v)) 
      (void)
      [begin
      (vector-set! big-v idx (vector-ref small-v idx))
      (copy-vector small-v big-v (+ 1 idx))
      ]
    )
  )
)
; (define double-store! (lambda () [
;     (let [[new-store (make-vector (* 2 (vector-length the-store!)))]]   

;     )
;   ]
; ))

;; (empty-store) returns an empty Scheme list representing the empty store.
;(define empty-store (lambda () '()))

;vector empty-store

(define empty-store (make-vector STORE-MAXSIZE))

;; (initialize-store!) initializes the-store! to (empty-store).
(define initialize-store!
  (lambda () (set! the-store! (empty-store)) (set! store-size! 0) ))

;; (newref! ev) takes an expval ev adds to the-store! and returns
;; a ref-val that points to the added value.
; (define newref!
;   (lambda (ev)
;     (set! the-store! (append the-store! (list ev)))
;     (ref-val (- (length the-store!) 1))
;     ))

; (define newref! 
;   (lambda (val)  ;;val = Scheme's value
;     (if (>= store-size! (vector-length the-store!))

;     )
;   )
; )

;; (deref ev) takes an expressed value which should be a (ref-val ref)
;; and returns the value associated with ref in the-store!.
; (define deref
;   (lambda (ev)
;     (list-ref the-store! (expval->ref ev))
;     ))

;vector deref
(define deref
  (lambda (ev)
    (vector-ref the-store! (expval->ref ev))  
))

;; (setref! ev1 ev2) takes two expvals, the first which should be a
;; (ref-val ref) it sets the cell ref in the-store! to ev2.  Returns (unit-val).
; (define setref!
;   (lambda (ev1 ev2)
;     (let [[ref (expval->ref ev1)]]
;       (set! the-store! (setref!* the-store! ref ev2))
;       (unit-val)
;     )))

; (define setref!*
;   (lambda (store ref ev)
;     (cond
;       [(= 0 ref) (cons ev (cdr store))]
;       [else (cons (car store) (setref!* (cdr store) (- ref 1) ev))])))

;;vector setref!
(define setref!
  (lambda (ev1 ev2)
    (let [[ref (expval->ref ev1)]]
      (vector-set! the-store! ref ev2) ref
      (unit-val)
    )
  )
)
  