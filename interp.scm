;; hw6-soln.scm
;; Fall 2022

(load "helpers.scm")

;; ================ Parser Definitions ==================================

;; This defines the translation from the concrete syntax to the abstract syntax
(define the-grammar
  '((program                        ;; <Program> ::= 
     (expression)                   ;;   Concrete    <Expression>
     a-prog)                        ;;   Abstract    (a-prog exp)
    
    (expression                     ;; <Expression> ::= 
     (number)                       ;;   Concrete       <Number> 
     const-exp)                     ;;   Abstract       (const-exp num)

    ;; ============== LET =================
      
    (expression                     ;; <Expression> ::= 
     ("zero?(" expression ")")      ;;   Concrete       zero?(<Expression>)
     zero?-exp)                     ;;   Abstract       (zero?-exp exp)
    
    (expression                                             ;; <Expression> ::= 
     ("if" expression "then" expression "else" expression)  ;;   Concrete       if <Expression> then <Expression> else <Expression>
     if-exp)                                                ;;   Abstract       (if-exp exp1 exp2 exp3)
        
    (expression                     ;; <Expression> ::= 
     (identifier)                   ;;   Concrete       <Identifier>
     var-exp)                       ;;   Abstract       (var-exp var)
    
    (expression                                          ;; <Expression> ::= 
     ("let" identifier "=" expression "in" expression)   ;;   Concrete       let <Identifier> = <Expression> in <Expression>
     let-exp)                                            ;;   Abstract       (let-exp var exp1 exp2)

     ;; ============== PROC ==================
    
    ;; (expression                                        ;; <expression> ::=
    ;;  ("proc(" identifier ")" expression)              ;;   Concrete  proc (<identifier>) <expression>
    ;;  proc-exp)                                         ;;   Abstract  (proc-exp param body)
    
    ;; (expression                                        ;; <expression> ::=
    ;;  ("(" expression expression ")")                   ;;   Concrete  (<expression> <expression>)
    ;;  call-exp)                                         ;;   Abstract  (call-exp rator rand)

    ;; ============== LETREC =================
    
    ;; (expression                                                            ;; <expression> ::=
    ;;  ("letrec" identifier "(" identifier ") =" expression "in" expression) ;;   letrec <id> (<id>) = <exp> in <exp>
    ;;  letrec-exp)                                                           ;;   (letrec-exp f-name f-param f-body body)

    ;; ============== HW 5 =================

    (program                               ;; <Program> ::= 
     ("def!" identifier "=" expression)    ;;  Concrete     def! <Identifier> = <Expression>
     def-prog)                             ;;  Abstract     (def-prog var exp)
    
    (expression                            ;; <Expression> ::= 
     ("#true")                             ;;   Concrete       #true
     const-true-exp)                       ;;   Abstract       (const-true-exp)
    
    (expression                            ;; <Expression> ::=
     ("#false")                            ;;   Concrete       #false
     const-false-exp)                      ;;   Abstract       (const-false-exp)
     
    (expression                            ;; <Expression> ::= 
     ("*(" expression "," expression ")")  ;;   Concrete       *(<Expression>,<Expression>)
     times-exp)                            ;;   Abstract       (times-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("/(" expression "," expression ")")  ;;   Concrete       /(<Expression>,<Expression>)
     div-exp)                              ;;   Abstract       (div-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("-(" expression "," expression ")")  ;;   Concrete       -(<Expression>,<Expression>)
     diff-exp)                             ;;   Abstract       (diff-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("+(" expression "," expression ")")  ;;   Concrete       +(<Expression>,<Expression>)
     plus-exp)                             ;;   Abstract       (plus-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("=(" expression "," expression ")")  ;;   Concrete       =(<Expression>,<Expression>)
     equal-exp)                            ;;   Abstract       (equal-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("<(" expression "," expression ")")  ;;   Concrete       <(<Expression>,<Expression>)
     less-than-exp)                        ;;   Abstract       (less-than-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("&(" expression "," expression ")")  ;;   Concrete       &(<Expression>,<Expression>)
     and-exp)                              ;;   Abstract       (and-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("|(" expression "," expression ")")  ;;   Concrete       |(<Expression>,<Expression>)
     or-exp)                               ;;   Abstract       (or-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("!(" expression ")")                 ;;   Concrete       !(<Expression>)
     not-exp)                              ;;   Abstract       (not-exp exp)

    (expression                               ;; <Expression> ::=
     ("cons(" expression "," expression ")")  ;;   Concrete       cons(<Expression>,<Expression>)
     cons-exp)                                ;;   Abstract       (cons-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::=
     ("car(" expression ")")               ;;   Concrete       car(<Expression>)
     car-exp)                              ;;   Abstract       (car-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("cdr(" expression ")")               ;;   Concrete       cdr(<Expression>)
     cdr-exp)                              ;;   Abstract       (cdr-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("null?(" expression ")")             ;;   Concrete       null?(<Expression>)
     null?-exp)                            ;;   Abstract       (null?-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("emptylist")                         ;;   Concrete       emptylist
     emptylist-exp)                       ;;   Abstract       (emptylist-exp)
    
    ;; ============= Explicit References ===============
    
    (expression
     ("newref!(" expression ")")
     newref!-exp)
    
    (expression
     ("deref(" expression ")")
     deref-exp)
    
    (expression
     ("setref!(" expression "," expression ")")
     setref!-exp)
    
    ;; ============= Implicit References ===============
    (expression
     ("set!" identifier "=" expression)
     set!-exp)
    
    
    ;; ============== HW 6 Definitions below ========================
    
    (expression                                           ;; <Expression> ::=
     ("{" (arbno expression) "}")                         ;;   Concrete       {<Expression>*}
     block-exp)                                           ;;   Abstract       (block-exp exps)
    
    (expression                                           ;; <Expression> ::=
     ("print!" "(" expression ")")                        ;;   Concrete       print!(<Expression>)
     print-exp)                                           ;;   Abstract       (print-exp exp)                         
    
    (expression                                           ;; <Expression> ::=
     ("newline!")                                         ;;   Concrete       newline!
     newline-exp)                                         ;;   Abstract       (newline-exp) 

    (expression                                    ;; <Expression> ::=                                 
     ("letrec" identifier "(" (arbno identifier)   ;;   Concrete       letrec <Identifier>(<Identifier>*) = <Expression> in <Expression>
      ")" "=" expression "in" expression)          ;;   Abstract       (letrec-exp p-name p-vars p-body body)
     letrec-exp)


    (expression                                     ;; <Expression> ::=
     ("proc" "(" (arbno identifier) ")" expression) ;;   Concrete       proc (<Identifier>*) <Expression>
     proc-exp)                                      ;;   Abstract       (proc-exp vars exp) 
    
    (expression                                    ;; <Expression> ::= 
     ("(" expression (arbno expression) ")")       ;;   Concrete       (<Expression> <Expression>*)
     call-exp)                                     ;;   Abstract       (call-exp exp exps)
    
  ))

(load "lex-scan-parse.scm")

;; =============== Environment Definition =============================


;; This is an implementation of the var-val pair list representation
;; of an environment, we wrote earlier.  I translated the
;; representation into a define-datatype so we get the constructors
;; and type checking predicate for free, and can use cases to process.

(define-datatype environ environ?
  (empty-env)                   ;; (empty-env) gives an empty environment
  (extend-env                   ;; (extend-env var val env) extends the environment
   (var symbol?)
   (val ref-val?)
   (env environ?))
  (extend-env-rec
   (p-name symbol?)
   (p-params (list-of symbol?))
   (p-body expression?)
   (saved-env environ?))
  )

;; (apply-env env target-var) s to figure out the maping of target-var
;; in the environment env.
(define apply-env ; Env x Var -> Expval
  (lambda (env target-var)
    (cases environ env
      [extend-env (var val env)
        (if (equal? var target-var)
          val
          (apply-env env target-var))]
      [empty-env () (raise-exception 'apply-env "No binding for ~s" target-var)]
      [extend-env-rec [p-name p-params p-body saved-env]
        (if (equal? p-name target-var)
          (newref! (proc-val p-params p-body ;;@why is there a newref! here, why can't we return the proc-val immediately
            (extend-env-rec p-name p-params p-body saved-env)))
          (apply-env saved-env target-var))]
      )))

(define make-init-env
  (lambda ()
    (extend-env 
     'pi (newref! (num-val 3.14159))
     (extend-env
      'e (newref! (num-val 2.71828))
      (empty-env)))))

(define env->string
  (lambda (env)
    (cases environ env
      [empty-env () "[]"]
      [extend-env (var val env*)
        (string-append "[" (symbol->string var) 
		       " = "  (expval->string (deref val)) 
		       (env->string* env*) "]")]
      [extend-env-rec (p-name p-params p-body saved-env)
                      (string-append "[" (symbol->string p-name) 
                                     "(" (fold-left string-append "" (map symbol->string p-params)) ")" 
                                     (env->string* saved-env) "]")])))

(define env->string*
  (lambda (env)
    (cases environ env
      [empty-env () ""]
      [extend-env (var val env*) 
        (string-append ", " (symbol->string var) 
          " = " (expval->string (deref val)) 
          (env->string* env*))]
      [extend-env-rec (p-name p-params p-body saved-env)
        (string-append ", " (symbol->string p-name) "(" (fold-left string-append "" (map symbol->string p-params)) ")"
          (env->string* saved-env))])))

;; ==================== Expressed Values ==================================

;; Expressed values are Int + Bool + Proc + Unit + List(Expvals) + Emptylist
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (params (list-of symbol?))
   (body expression?)
   (saved-env environ?))
  (unit-val)
  (list-val
   (ls (list-of expval?)))
  (emptylist-val)  ;; Extra value to distinguish pairs and lists.
  (ref-val
    (ref integer?))
)

(define ref-val?
  (lambda (v)
    (and (expval? v)
      (cases expval v
        [ref-val (r) #t]
        [else #f]))))

(define expval->num 
  (lambda (ev)
    (cases expval ev
      [num-val (num) num]
      [bool-val (bool) (if bool 1 0)]
      [else (raise-exception 'expval->num "Expressed value is not a number or a Boolean: ~s" ev)])))

(define expval->bool
  (lambda (ev)
    (cases expval ev
      [bool-val (bool) bool]
      [num-val (num) (not (= num 0))]
      [else (raise-exception 'expval->bool "Expressed value is not a Boolean or a number: ~s" ev)])))

(define expval->ref
  (lambda (ev)
    (cases expval ev
      [ref-val (ref) ref]
      [else (raise-exception 'expval->ref "Expressed value is not a ref: ~s" ev)])))

(define expval->list
  (lambda (ev)
    (cases expval ev
      [list-val (ls) ls]
      [else (raise-exception 'expval->list "Expressed value is not a list: ~s" ev)])))

(define expval->string
  (lambda (ev)
    (cases expval ev
           [bool-val (bool) (if bool "#true" "#false")]
           [num-val (num) (number->string num)]
           [proc-val (params body saved-env)
                     (string-append "#proc(" (number->string (length params)) ")")] 
           [unit-val () "#void"]
           [list-val (ls)  ;; Clunky attempt to mirror scheme list / pair display.
                     (let
                         [[last (list-ref ls (- (length ls) 1))]]
                       (let
                           [[s
                             (fold-left
                              string-append
                              ""
                              (map
                               (lambda (ev) (string-append (expval->string ev) " "))
                               (reverse (cdr (reverse ls)))))]]
                         (cases expval last
                                [emptylist-val ()
                                               (string-append "("
                                                              (if (> (string-length s) 0)
                                                                  (string-truncate! s (- (string-length s) 1))
                                                                  "")
                                                              ")")]
                                [else (string-append "(" s ". " (expval->string last) ")")])))]
           [emptylist-val () "()"]
           [ref-val (ref) (string-append "ref(" (number->string ref) ")")]
           )))

      
(load "store.scm")

;; ==================== Evaluater ====================================

;; Curried evaluation of operators.
(define make-eval-op
  (lambda (arg-> result-cons)
    (lambda (op exp1 exp2 env)
      (result-cons (op (arg-> (value-of-exp exp1 env)) (arg-> (value-of-exp exp2 env))))
      )))

;; Specialized for arithmetic operators and comparions.
(define eval-bin-arith (make-eval-op expval->num num-val))
(define eval-bin-comp (make-eval-op expval->num bool-val))
;; Doesn't work for and / or because they aren't functions in scheme.

(define value-of
  (lambda (prog env)
    (cases program prog
      [a-prog (exp) (cons (value-of-exp exp env) env)]
      [def-prog (var exp) (cons (unit-val) (extend-env var (newref! (value-of-exp exp env)) env))]
      [else (raise-exception 'value-of-prog "Abstract syntax case not implemented: ~s" (car prog))])))

(define value-of-exp
  (lambda (exp env)
    (cases expression exp
      ;; ======== STARTER ==========
      [const-exp (num) (num-val num)]

      ;; ========== LET ===========
      [diff-exp (exp1 exp2) (eval-bin-arith - exp1 exp2 env)]
      [zero?-exp (exp1) (bool-val (= (expval->num (value-of-exp exp1 env)) 0))]
      [if-exp (exp1 exp2 exp3) (if (expval->bool (value-of-exp exp1 env)) (value-of-exp exp2 env) (value-of-exp exp3 env))]
      [var-exp (var) (deref (apply-env env var))]
      [let-exp (var exp1 exp2) (value-of-exp exp2 (extend-env var (newref! (value-of-exp exp1 env)) env))]

      ;; ========== PROC ==========
      ;; [proc-exp [param body] (proc-val param body env)]
      ;; [call-exp [rator rand]
      ;;   (let [[ev1 (value-of-exp rator env)]]
      ;;     (cases expval ev1
      ;;       [proc-val [param body saved-env]
      ;;         (value-of-exp body
      ;;           (cases expression rand
      ;;             [var-exp (var)
      ;;               (extend-env param (apply-env env var) saved-env)]
      ;;             [else
      ;;               (extend-env param (newref! (value-of-exp rand env)) saved-env)]))]
      ;;       [else
      ;;         (raise-exception
      ;;           'value-of-exp
      ;;           "Attempted to call non-procedure: ~s" rator)]))]
      
      ;; ========= LETREC =========
      ;; [letrec-exp [p-name p-param p-body body]
      ;;   (value-of-exp body (extend-env-rec p-name p-param p-body env))]

      
      ;; ========== HW 5 ==========
      [const-true-exp () (bool-val #t)]
      [const-false-exp () (bool-val #f)]
      [plus-exp (exp1 exp2) (eval-bin-arith + exp1 exp2 env)]
      [div-exp (exp1 exp2) 
	       (let 
		   [[val1 (expval->num (value-of-exp exp1 env))]
                    [val2 (expval->num (value-of-exp exp2 env))]]
		 (if (= val2 0)
		     (raise-exception 'value-of-exp "Divide by zero exp = ~s with env = ~s" exp env)
		     (num-val (/ val1 val2))))]
      [times-exp (exp1 exp2) (eval-bin-arith * exp1 exp2 env)]
      [less-than-exp (exp1 exp2) (eval-bin-comp < exp1 exp2 env)]
      [equal-exp (exp1 exp2) (eval-bin-comp = exp1 exp2 env)]
      [and-exp (exp1 exp2) (bool-val (and (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
      [or-exp (exp1 exp2) (bool-val (or (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
      [not-exp (exp1) (bool-val (not (expval->bool (value-of-exp exp1 env))))]

      ;; === HW 5 - Extra Credit ===
      ;; Implementation is a little clunky.
      [cons-exp (exp1 exp2)
                (let
                    [[ev1 (value-of-exp exp1 env)]
                     [ev2 (value-of-exp exp2 env)]]
                  (cases expval ev2
                        [list-val (lst) (list-val (cons ev1 (expval->list ev2)))]
                        [else (list-val (list ev1 ev2))]))]
      [car-exp (exp1)
	       (let
		   [[ls (expval->list (value-of-exp exp1 env))]]
                 (cond
                  [(null? ls) (raise-exception 'value-of-exp "Attempting to car empty list." exp env)]
                  [else
                   (cases expval (car ls)
                          [emptylist-val () (raise-exception 'value-of-exp "Attempting to car empty list." exp env)]
                          [else (car ls)])]))]
      [cdr-exp (exp1)
	       (let
		   [[ls (expval->list (value-of-exp exp1 env))]]
                 (cond
                  [(null? ls) (raise-exception 'value-of-exp "Attempting to cdr empty list." exp env)]
                  [(= (length ls) 1) (raise-exception 'value-of-exp "Attempting to cdr empty list." exp env)]
                  [(= (length ls) 2)
                   (cases expval (cadr ls)
                          [emptylist-val () (list-val (list (emptylist-val)))]
                          [else (cadr ls)])]
                  [else (list-val (cdr ls))]))]
      [null?-exp (exp1) (bool-val (equal? (list (emptylist-val)) (expval->list (value-of-exp exp1 env))))]
      [emptylist-exp () (list-val (list (emptylist-val)))]

      ;; ======== REF ==============
      [newref!-exp (exp)  (newref! (value-of-exp exp env))]
      [deref-exp (exp)  (deref (value-of-exp exp env))]
      [setref!-exp (exp1 exp2) (setref! (value-of-exp exp1 env) (value-of-exp exp2 env))]

      ;; ======== IMP ==============
      [set!-exp (var exp) (setref! (apply-env env var) (value-of-exp exp env)) (unit-val)]



      ;; ======== HW 6 =============
      [proc-exp (vars body) (proc-val vars body env)]


      [call-exp
       (rator rands)
       (let [[ev1 (value-of-exp rator env)]]
         (cases expval ev1
                [proc-val
                 [params body saved-env]
                 (cond
                  [(= (length params) (length rands))
                   (value-of-exp body
                                 (fold-left
                                  (lambda (acc param exp)
                                    (extend-env param
                                                (cases expression exp
                                                       [var-exp (var) (apply-env env var)]
                                                       [else (newref! (value-of-exp exp env))])
                                                acc))
                                  saved-env
                                  params
                                  rands))]
                  [else (raise-exception 'value-of-exp 
                                         "Attempt to apply procedure with inconsistent number of arguments: ~s expect, ~s given."
                                         (length params) (length rands))])]
            [else
              (raise-exception
                'value-of-exp
                "Attempted to call non-procedure: ~s" rator)]))]

      
      ;; [call-exp (exp exps)
      ;;   	(let*
      ;;   	    ([val (value-of-exp exp env)]
      ;;   	     [params (expval->proc-param val)]
      ;;   	     [body (expval->proc-body val)]
      ;;   	     [saved-env (expval->proc-saved-env val)])
      ;;   	  (cond
      ;;   	   [(= (length params) (length exps))
      ;;   	    (let
      ;;   		([vals (map (lambda (x) (value-of-exp x env)) exps)])
      ;;   	      (value-of-exp body 
      ;;   			    (fold-left
      ;;   			     (lambda (acc head) (extend-env (car head) (cdr head) acc)) 
      ;;   			     saved-env
      ;;   			     (reverse (map (lambda (param val) (cons param val)) params vals)))))]
      ;;   	   [else (raise-exception 'value-of-exp 
      ;;   				  "Attempt to apply function with inconsistent number of arguments: ~s ~s." exp exps)]))]
      [letrec-exp (p-name p-vars p-body body) 
		    (value-of-exp body (extend-env-rec p-name p-vars p-body env))]     
      
      [print-exp (exp) (display (expval->string (value-of-exp exp env))) (unit-val)]
      [newline-exp ()  (newline) (unit-val)]

      [block-exp (exps) (fold-left (lambda (acc exp) (value-of-exp exp env)) (unit-val) exps)]
			


      
      [else (raise-exception 'value-of-exp "Abstract syntax case not implemented: ~s" (car exp))])))


;; ==================== Interpreter ====================================

;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (display "\n=== Welcome to the HW 5 Interpreter === \n\n")
      (initialize-store!)
      (read-eval-print (make-init-env)))))

;; (read-eval-print) -- Main read, eval, and print loop.
(define read-eval-print
  (lambda (env)
    ;; Display an interpreter prompt
    ;;(display "==> ")
    ;; Read a line user input
    (let [[code (get-input-string)]]
      (cond 
       [(equal? code "!quit")
	(display "Goodbye!")  ;; Quit if 'quit entered.
	(newline)]
       [else   ;; Do something
	(cond
	 [(equal? code "!debug0")
	  (untrace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string)]
	 [(equal? code "!debug1")
	  (trace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string)]
	 [(equal? code "!debug2")
	  (trace value-of value-of-exp expval->num expval->bool expval->string)]
	 [(equal? code "!debug3")
	  (trace value-of value-of-exp expval->num expval->bool expval->string apply-env extend-env empty-env)]
	 [(equal? code "!env")
	  (display (env->string env))
	  (newline)]
   [(equal? code "!env raw") (display env) (newline)]
   [(equal? code "!store") (display the-store!) (newline)] ;;@for quick debugging purpose
   [(equal? code "!store-size") (display store-size!) (newline)]
	 [(equal? code "!reset-env")
	  (set! env (make-init-env))]
   [(equal? code "!reset-store") (initialize-store!) (make-init-env)] ;;also needs to reset all refs
   [(equal? code "!force-gc") (garbage-collector env)]

	 [else
	  ;; Parse code, eval expression, and print result.
	  (guard  ;; Catches parse exceptions from sllgen
	   (parse-except 
	    [else    ;; With only an else case this catches any every exception.
	     (display "Parse error:\n")
	     (display-exception parse-except)
	     ])
	   (let
	       [[abstract-code (parse code)]]  ;; Try to parse the input line
	     (guard   ;; Catches runtime exceptions from value-of
	      (value-of-except 
	       [else
		(display "Runtime error:\n")
		(display-exception value-of-except)
		])
	      (let*
		  [[result (value-of abstract-code env)]
		   [val (car result)]
		  [new-env (cdr result)]]
		(display (expval->string val))
		(set! env new-env)  
		(newline)
    (garbage-collector env)
		))))])
	(read-eval-print env)]))))



