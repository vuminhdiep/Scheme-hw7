;; store.scm

;;@Ask MATT what is the best design practice for the store datastructure

(define the-store! 'uninitialized)
(define store-size! 'uninitialized)   
(define head-free! 'uninitialized)
(define tail-free! 'uninitialized)
(define STORE-MAXSIZE 2)

(define-datatype store-cell store-cell?
  (expval-cell (val expval?) (mark-bit boolean?)) ;;@unsure: constructor-type will have to reinit since immutable, vector-size-2 will lose semantics
  (free-cell  (next-ref integer?))
)

(define copy-vector! 
  (lambda (small-v big-v idx)
    (if (= idx (vector-length small-v)) 
      (void)
      [begin
      (vector-set! big-v idx (vector-ref small-v idx))
      (copy-vector! small-v big-v (+ 1 idx))
      (void)
      ]
    )
  )
)

(define double-store! 
  (lambda ()  
    (let [(new-store (make-vector (* 2 (vector-length the-store!)) (free-cell -1)))]
      (copy-vector! the-store! new-store 0)
      (set! the-store! new-store)
    )
  )
)

;; (empty-store) returns an empty Scheme list representing the empty store.
;(define empty-store (lambda () '()))

;vector empty-store

(define empty-store (lambda () (make-vector STORE-MAXSIZE (free-cell -1))))

;; (initialize-store!) initializes the-store! to (empty-store).
(define initialize-store!
  (lambda () (set! the-store! (empty-store)) (set! store-size! 0) (set! head-free! 0) (set! tail-free! 0)))

;; (newref! ev) takes an expval ev adds to the-store! and returns
;; a ref-val that points to the added value.
; (define newref!
;   (lambda (ev)
;     (set! the-store! (append the-store! (list ev)))
;     (ref-val (- (length the-store!) 1))
;     ))

(define newref! 
  (lambda (val)  ;;val = Scheme's value
    (begin 
      (if (>= store-size! (vector-length the-store!))
        (double-store!)
      )
      (let [(cell (vector-ref the-store! head-free!)) (head* head-free!)]
        (vector-set! the-store! head-free! (expval-cell val #f))
        (if (>= head-free! store-size!) (set! store-size! (+ 1 store-size!)))

        (cases store-cell cell
          [free-cell (ref) 
            (if (= -1 ref) 
              [begin (assert (= head-free! tail-free!)) (set! head-free! store-size!) (set! tail-free! head-free!)]
              (set! head-free! ref)
            )
          ]
          [else (raise-exception 'newref! "Must be impossible to reach here. Implementation error elsewhere")]
        )

        (if (>= store-size! (vector-length the-store!))
          (double-store!)   ;;@tail assumes that that position must be in existence
        )
        (ref-val head*)
      )
    )
  )
)

;; (deref ev) takes an expressed value which should be a (ref-val ref)
;; and returns the value associated with ref in the-store!.
; (define deref
;   (lambda (ev)
;     (list-ref the-store! (expval->ref ev))
;     ))

;vector deref
(define deref
  (lambda (ev)
    (let [(cell (vector-ref the-store! (expval->ref ev)))]
      (cases store-cell cell
        [expval-cell [val bit] val]
        [else (raise-exception 'deref "invalid address" 1)] ;;@should it be invalid address
      )
    )
  )  
)

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
      (vector-set! the-store! ref (expval-cell ev2 #f))
      (unit-val)      ;;@check invalid address access, setref! on a free-cell
    )
  )
)

(define dfs-from 
  (lambda [cell-idx]  
    (let [(cell (vector-ref the-store! cell-idx))] 
      (cases store-cell cell
      [expval-cell (val mark-bit) 
        (if (not mark-bit)
          [begin 
          ; (display "marking") (display " ") (display cell-idx) (newline)
          (set! mark-bit #t)
          (vector-set! the-store! cell-idx (expval-cell val mark-bit))
          (cases expval val 
            (ref-val [r] (dfs-from r))
            (proc-val [params body saved-env] (mark saved-env))
            (list-val [ls] (mark-for-list ls))
            [else (void)]
          )]
        )
      ]
      [free-cell [next-ref] (void)]
      [else (void)]
      )
    )
  )
)

(define sweep 
  (lambda [idx]
    (if (>= idx store-size!) 
      (void)
      (let [(cell (vector-ref the-store! idx))]
        [cases store-cell cell
          (expval-cell (val mark-bit) 
            (if mark-bit 
              (begin (vector-set! the-store! idx (expval-cell val #f)) (sweep (+ 1 idx)) )
              (begin (vector-set! the-store! tail-free! (free-cell idx)) (vector-set! the-store! idx (free-cell -1))
               (set! tail-free! idx) (sweep (+ 1 idx)))
            )
          )
          (else (sweep (+ 1 idx)))
        ]
      )
    )
  )
)
(define mark-for-list
  (lambda [ls] 
    (if (null? ls)
      (void)
      [let ((top (car ls)))
        (cases expval top
          [ref-val (ref) (dfs-from ref)]
          [else (void)]
        )
        (mark-for-list (cdr ls))
      ]
    )
  )
)
(define mark 
  (lambda [source-env] 
    (cases environ source-env 
      [empty-env () (void)]
      [extend-env (var val env*) 
        (cases expval val 
          [ref-val (ref) (dfs-from ref)]
          [else (raise-exception 'mark "invalid values in environment" 1)]
        )
        (mark env*)
      ]
      [extend-env-rec (name params body saved-env)
        (mark saved-env)
      ]
    )
  )
)

(define garbage-collector
  (lambda [source-env] 
    (mark source-env)
    (sweep 0)
    (unit-val)
  )
)