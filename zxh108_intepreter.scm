;Zhenglin Huang
;zxh108@case.edu
;bnf-refined

(load "simpleParser.scm")

;takes a file and run the call tree generated from it
(define intepreter
  (lambda (file)
    (run (parser file) (initialize '()))))

;takes a call tree and runs statements one by one
(define run
  (lambda (queue state)
    (if (null? queue)
        state
        (run (cdr queue) (Mstate (car queue) state)))))

;takes a statement and a state, and generates a new state
(define Mstate
  (lambda (statement state)
    (cond
      ;if the statement is empty, do nothing
      ((null? statement) state)
      ;if the first word is var, it is an declaration
      ((eq? 'var (operator statement)) (Mstate_declaration statement state))
      ;if the first word is "=", it is an assignment
      ((eq? '= (operator statement)) (Mstate_assignment statement state))
      ;if it starts with "return", we retrive the boolean or numerical value of the next item, the expression
      ((eq? 'return (operator statement)) (M_return statement state))
      ;if it is an if statement
      ((eq? 'if (operator statement)) (Mstate_if statement state))
      ;if it is none of the above, it is an error
      (else (error 'statement_not_recognized)))))

;generates an return value
(define M_return
  (lambda (statement state)
    ((lambda (print)
       (cond
         ((eq? print #t) 'true)
         ((eq? print #f) 'false)
         (else print)))
     (Mexpression (secondterm statement) state))))

;takes an if statement and a state, and generates a new state
(define Mstate_if
  (lambda (statement state)
    ; if it is true
    (if (Mboolean (secondterm statement) state)
        ;do this as then
        (Mstate (thirdterm statement) state)
        ;if it falls to else
        (if (null? (cdddr statement))
            state  ;<-else does not exist, do nothing
            ;if else does exist as below-
            (Mstate (cadddr statement) state)))))

;takes an declaration and returns a state
(define Mstate_declaration
  (lambda (statement state)
    ;if it is a (var x) style
    (if (null? (aftersecondterm statement))
        ;adds a new one, not initialized. Remove the same name variable if there is any
        (addvar (secondterm statement) (removevar (secondterm statement) state))
        ;adds a new one with declared value. Removed the same variable previously first
        (addvalue (secondterm statement) (Mexpression (thirdterm statement) (removevar (secondterm statement) state)) (removevar (secondterm statement) state)))))

;takes an assignment statement
(define Mstate_assignment
  (lambda (statement state)
    ;if the variable is declared
    (if (member? (secondterm statement) (v_name state))
        ;add the value
        (addvalue (secondterm statement) (Mexpression (thirdterm statement) state) (removevar (secondterm statement) state))
        ;else, it is an error
        (error 'not_declared_error_on_assignment))))

;takes an expression, and returns its value either in boolean or integer
(define Mexpression
  (lambda (expression state)
    (cond
      ((boolean? expression) (Mboolean expression state))
      ((number? expression) (Mvalue expression state))
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mexpression (get_value expression state) state))
      ;check for numerical calculation
      ((member? (operator expression) '(+ - * / %)) (Mvalue expression state))
      ;check for boolean calculation
      ((member? (operator expression) '(|| && ! == != > < <= >=)) (Mboolean expression state))
      ;if it is not boolean or numerical, we can't solve it for now
      (else (error 'expression_not_supported)))))

;retrieve the numerical value of an expression
(define Mvalue
  (lambda (expression state)
    (cond
      ((boolean? expression) (error 'type_incompatible))
      ((number? expression) expression)
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mvalue (get_value expression state) state))
      ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state)
                                         (Mvalue (rightoperand expression) state)))
      ((eq? '- (operator expression)) (if (null? (cddr expression))
                                          (- 0 (Mvalue (leftoperand expression) state))
                                          (- (Mvalue (leftoperand expression) state)
                                             (Mvalue (rightoperand expression) state))))
      ((eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state)
                                         (Mvalue (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state)
                                                (Mvalue (rightoperand expression) state)))
      ((eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state)
                                                 (Mvalue (rightoperand expression) state)))
      (else (error 'bad_operator)))))

;retrieves a boolean number by executing the expression
(define Mboolean
  (lambda (expression state)
    (cond
      ((number? expression) (error 'type_incompatible))
      ((boolean? expression) expression)
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mboolean (get_value expression state) state))
      ((eq? '|| (operator expression)) (if (Mboolean (leftoperand expression) state)
                                           true
                                           (Mboolean (rightoperand expression) state)))
      ((eq? '&& (operator expression)) (if (Mboolean (leftoperand expression) state) 
                                           (Mboolean (rightoperand expression) state)
                                           false))
      ((eq? '!  (operator expression)) (not (Mboolean (leftoperand expression) state)))
      ((eq? '== (operator expression)) (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? '>  (operator expression)) (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<  (operator expression)) (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<= (operator expression)) (<= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '>= (operator expression)) (>= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (error 'bad-operator)))))



;abstractions for getting the three things right
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
(define aftersecondterm cddr)
(define secondterm cadr)
(define thirdterm caddr)
(define atom? (lambda (v) (not (pair? v))))

;--------------Environment manipulations that is seperated from the environment --------------

;method to remove a variable

;method to add a new variable and assign its value, show that it is initialized
(define addvalue
  (lambda (var value s)
    (add var value #t s)))

;method to only declare a variable, show that it is initialized
(define addvar
  (lambda (var s)
    (add var -1 #f s)))

;returns a value, if it is initialized, or an error.
(define get_value
  (lambda (var s)
    (if (find var (v_name s) (v_init? s))
        (find var (v_name s) (v_value s))
        (error 'variable_not_initialized))))

;environment specific interfaces, strictly three lists and attributes are hard coded

;method to remove a variable from the environment, and all attributes associated with it.
(define removevar
  (lambda (var s)
    (cond
      ((null? (v_name s)) s)
      ((eq? var (car (v_name s))) (trimstate s))
      (else (add (car (v_name s)) (car (v_value s)) (car (v_init? s)) (removevar var (trimstate s)))))))

;the way to retrieve name, value and init? from the environment
(define v_name car)
(define v_value cadr)
(define v_init? caddr)

;pass three variables and glue it to the beginning of the list
(define add
  (lambda (x1 x2 x3 s)
    (combine (cons x1 (v_name s)) (cons x2 (v_value s)) (cons x3 (v_init? s)))))

;find an item x from key(list), and return the corresponding item in the target(list)
(define find
  (lambda (x key target)
    (cond
      ((null? key) (error 'variableNotFoundError))
      ((null? target) (error 'targetistooshort))
      ((eq? x (car key)) (car target))
      (else (find x (cdr key) (cdr target))))))

;create a state with 3 properties, empty
(define initialize
  (lambda (l)
    (emptyall (wrap '(name value init?)))))

;pass in three lists
;returns a state consists of 3 lists
(define combine
  (lambda (l1 l2 l3)
    (cons l1 (cons l2 (cons l3 '())))))

;pass in a state with the first variable removed
(define trimstate
  (lambda (s)
    (combine (cdr (v_name s)) (cdr (v_value s)) (cdr (v_init? s)))))

;Abstraction helper
(define wrap
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (cons (car l) '()) (wrap (cdr l)))))))

;Abstraction helper
(define emptyall
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (emptyall (car l)) (emptyall (cdr l))))
      (else (emptyall (cdr l))))))

;takes an atom and a list, to check whether the aton is in the list
(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))