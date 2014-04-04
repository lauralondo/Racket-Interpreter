#| Laura Londo
   COMP360
   25 February 2014
   HW3

   I pledge to demonstrate personal and academic integrity in all matters with 
   my fellow students, the faculty, and the administration here at USD. I 
   promise to be honest and accountable for my actions; and to uphold the 
   statutes of scholastic honesty to better myself and those around me.
|#

#lang plai-typed


;; values
(define-type Value
  [numV (n : number)]
  [closV (params : (listof symbol))
         (body : FirstClassFuncs) 
         (env : Env)])

;; numV-op
(define (numV-op [op : (number number -> number)]
                 [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'numV-op "one argument was not a number")]))

;; numV-cmp
(define (numV-cmp [cmp : (number number -> boolean)]
                 [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (if (cmp (numV-n l) (numV-n r)) 1 0))]
    [else
     (error 'numV-op "one argument was not a number")]))


;; BINDING
;; A binding will be an entry in our map from symbols to values
(define-type Binding
  [bind (name : symbol) (val : Value)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)

;; EXTEND-ENV
;; (listof symbol) * (listof Value) * Env -> Env 
;; adds new bindings to an environment
(define (extend-env [params : (listof symbol)] ;symbols
                    [args : (listof Value)]    ;values for each symbol
                    [oldEnv : Env]) : Env      ;environment to add to
  (cond
    [(> (length params) 0) (append  ;if there are more parameters to add, append
                            (list (bind (first params) (first args))) ;a new binding
                            (extend-env (rest params) (rest args) oldEnv))] ;recursive call
    [else oldEnv])) ;base case: return the old environment
                         

;; CORE LANGUAGE
;;the core language
(define-type FirstClassFuncs
  [num-fcf (n : number)]
  [plus-fcf (l : FirstClassFuncs) (r : FirstClassFuncs)]
  [mult-fcf (l : FirstClassFuncs) (r : FirstClassFuncs)]
  [div-fcf  (l : FirstClassFuncs) (r : FirstClassFuncs)]  ;;in the core language because it would be extremely dificult to express
                                                          ;;using only = * or /
  [eq-fcf   (l : FirstClassFuncs) (r : FirstClassFuncs)]  ;;in the core language because eq cannot be expressed with only = * or /
  [if-fcf   (s : FirstClassFuncs) (t : FirstClassFuncs) (f : FirstClassFuncs)]  ;;in core language because if cannot 
                                                                                ;;be expressed using only + * or /
  [id-fcf (name : symbol)]
  [lambda-fcf (l : (listof symbol)) (f : FirstClassFuncs)]
  [call-fcf (f : FirstClassFuncs) (l : (listof FirstClassFuncs))]
  [define-locals-fcf (b : Env) (f : FirstClassFuncs)])


;; SUGAR LANGUAGE
;;the sugary language
(define-type FirstClassFuncsS
  [num-fcfS (n : number)]
  [plus-fcfS (l : FirstClassFuncsS) (r : FirstClassFuncsS)]
  [mult-fcfS (l : FirstClassFuncsS) (r : FirstClassFuncsS)]
  [bmin-fcfS (l : FirstClassFuncsS) (r : FirstClassFuncsS)] ;;can be expressed with plus and mult
  [umin-fcfS (r : FirstClassFuncsS)]                      ;;can be expressed with plus and mult
  [div-fcfS  (l : FirstClassFuncsS) (r : FirstClassFuncsS)]
  [eq-fcfS   (l : FirstClassFuncsS) (r : FirstClassFuncsS)] 
  [if-fcfS (s : FirstClassFuncsS) 
           (t : FirstClassFuncsS) 
           (f : FirstClassFuncsS)]
  [id-fcfS (name : symbol)]
  [lambda-fcfS (l : (listof symbol)) (f : FirstClassFuncsS)]
  [call-fcfS (f : FirstClassFuncsS) (l : (listof FirstClassFuncsS))]
  [define-locals-fcfS (b : Env) (f : FirstClassFuncsS)]
  [cond-fcfS (l : (listof FirstClassFuncsS))])  ;;can be expressed with nested if statements
                                                ;;contains a list of FirstClassFuncsS to allow more than one argument




;; LOOKUP
;; symbol * Env -> value
;; Finds value associated with symbol in the given eivnronment
(define (lookup [name : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? name (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup name (rest env))])]))

(test (lookup 'x (list (bind 'x (numV 12)) (bind 'y (numV 91)))) (numV 12))

;; SLICE
;; returns a sub-list of elements between the specified indices
(define (slice [li : (listof 'a)] [s : number] [e : number]) : (listof 'a)
  (cond
    [(< s 0) (error 'slice "start index smaller than 0")]
    [(>= e (length li)) (error 'slice "end index greater than list length")]
    [(< e s) (error 'slice "end index is smaller than start index")]
    [else (cond 
            [(> s 0) (slice (rest li) (- s 1) (- e 1))]
            [(= e 0) (list (first li))]
            [(> (- (length li) 1) e) (append (list (first li)) 
                                             (slice (rest li) s (- e 1)))]
            [else li])]))

(test (slice (list 0 1 2 3 4 2 3 4 2 4 5 6 7 8 9) 5 8)  (list 2 3 4 2))
(test (slice (list 5) 0 0)  (list 5))



;; LAST
;; returns the last item in the list
(define (last [li : (listof 'a)]) : 'a
  (cond
    [(= (length li) 0) (error 'last "list is empty!")]
    [(> (length li) 1) (last (rest li))]
    [else (first li)]))

(test (last (list 1 2 3 4 5 5 6 7))   7)
(test (last (list 3)) 3)



;; BIND-STUFF
;; takes a list of s-expressions and returns an environment
(define (bind-stuff [ls : (listof s-expression)]) : Env
  (let ([firstLs (s-exp->list (first ls))]) 
    (if (> (length ls) 1) 
      (append (list (bind (s-exp->symbol (first firstLs))
                          (interp (desugar (parse (second firstLs))) mt-env))) 
              (bind-stuff (rest ls)))
      (list (bind (s-exp->symbol (first firstLs))
                  (interp (desugar (parse (second firstLs))) mt-env))))))



;; PARSE
;; s-expression -> FirstClassFuncsS
(define (parse [s : s-expression]) : FirstClassFuncsS
  (cond
    [(s-exp-number? s) (num-fcfS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(symbol=? '+ (s-exp->symbol (first sl)))
          (plus-fcfS (parse (second sl)) (parse (third sl)))]
         [(symbol=? '* (s-exp->symbol (first sl)))
          (mult-fcfS (parse (second sl)) (parse (third sl)))]
         [(symbol=? '- (s-exp->symbol (first sl)))
          (cond 
            [(= (length sl) 2) (umin-fcfS (parse (second sl)))]         ;;its unary minus
            [else (bmin-fcfS (parse (second sl)) (parse (third sl)))])] ;;else its binary minus
         [(symbol=? '/ (s-exp->symbol (first sl)))
          (div-fcfS (parse (second sl)) 
                    (parse (third sl)))]
         [(symbol=? 'eq-fcf (s-exp->symbol (first sl)))
          (eq-fcfS (parse (second sl)) 
                   (parse (third sl)))]
         [(symbol=? 'if-fcf (s-exp->symbol (first sl)))
          (if-fcfS (parse (second sl))
                   (parse (third sl))
                   (parse (fourth sl)))]
         
         [(symbol=? 'lambda-fcf (s-exp->symbol (first sl)))
          (lambda-fcfS (cond  ;check if there are any parameters
                         [(> (length sl) 2) (map s-exp->symbol 
                                                 (slice sl 1 (- (length sl) 2)))] 
                         [else empty]) ;if not, use empty list
                       (parse (last sl)))] ;parse body
         
         [(symbol=? 'call-fcf (s-exp->symbol (first sl)))
          (call-fcfS (parse (second sl)) (map parse (rest (rest sl))))]
         
         [(symbol=? 'define-locals (s-exp->symbol (first sl)))
          (define-locals-fcfS 
            (bind-stuff (s-exp->list (second sl))) 
            (parse (third sl)))]
         
         [(symbol=? 'cond-fcf (s-exp->symbol (first sl)))
          (cond-fcfS (map parse (rest sl)))] ;;parse each element of the list
         
         [else (error 'parse "bad stuff happened!")]))]
    [else (id-fcfS (s-exp->symbol s))]));(error 'parse "lonely symbol")]))



;; INTERP
;; FirstClassFuncs -> number
;; interprets functions of the core language
(define (interp [arg : FirstClassFuncs] [e : Env]) : Value
  (type-case FirstClassFuncs arg
    [num-fcf (n) (numV n)]
    [plus-fcf (l r) (numV-op + (interp l e) (interp r e))]
    [mult-fcf (l r) (numV-op * (interp l e) (interp r e))]
    [div-fcf  (l r) (numV-op / (interp l e) (interp r e))]
    [eq-fcf   (l r) (numV-cmp = (interp l e) (interp r e))]
    [if-fcf (s t f) (if (= (numV-n (interp s e)) 0) (interp f e) (interp t e))]
    [id-fcf (n) (lookup n e)]   ;look up the symbol in the environment
    [lambda-fcf (p f) (closV p f e)]
    [call-fcf (f a) (let ([fd (interp f e)])
                      (cond
                        [(= (length (closV-params fd)) (length a)) ;correct number of args
                         (interp (closV-body fd)
                                 (extend-env             ;extend the old environment
                                  (closV-params fd)                 ;parameters
                                  (map (lambda (f) (interp f e)) a) ;list of arguments
                                  (closV-env fd)))]                 ;old environment
                        [else (error 'interp "incorrect number of args")]))]
    [define-locals-fcf (l f) (interp f l)]
    ))



;; DESUGAR
;; FirstClassFuncsS -> FirstClassFuncs
;; convert sugary syntax into core language
(define (desugar [sugar : FirstClassFuncsS]) : FirstClassFuncs
  (type-case FirstClassFuncsS sugar
    [num-fcfS (n) (num-fcf n)]
    [plus-fcfS (l r) (plus-fcf
                      (desugar l)
                      (desugar r))]
    [mult-fcfS (l r) (mult-fcf
                      (desugar l)
                      (desugar r))]
    [bmin-fcfS (l r) (plus-fcf       ;;translates binary minus to addition of the neg of the second argument
                      (desugar l)
                      (mult-fcf
                       (num-fcf -1)
                       (desugar r)))]
    [umin-fcfS (v) (desugar (bmin-fcfS (num-fcfS 0) v))] ;;unary minus translates to subtracting the argument from 0
    [div-fcfS (l r) (div-fcf      ;;in the core language
                     (desugar l) 
                     (desugar r))]
    [eq-fcfS (l r)  (eq-fcf (desugar l) (desugar r))] ;;in the core language 
    [if-fcfS (s t f) (if-fcf (desugar s)     ;;in the core language 
                             (desugar t)
                             (desugar f))]
    [id-fcfS (n) (id-fcf n)] 
    [lambda-fcfS (l f) (lambda-fcf l (desugar f))]
    [call-fcfS (f la) (call-fcf (desugar f) (map desugar la))]
    [define-locals-fcfS (e f) (define-locals-fcf e (desugar f))]
    [cond-fcfS (l) (cond                     ;;expresses cond in terms of nested if statements
                     [(> (length l) 1) (if-fcf (desugar (first l))  ;;first item is the conditional statement
                                               (desugar (second l)) ;;second item is the result if the statement is true
                                               (desugar (cond-fcfS (rest (rest l)))))] ;;rest of the list is made into a nested
                                                                                       ;;if statement in the case that the
                                                                                       ;;previous statement is false
                     [(= (length l) 1) (desugar (first l))]          ;;if there is only 1 item left in the list just return it
                                                                     ;;as the default else case
                     [else (error 'desugar "need a default condition")])])) ;;else the wrond number of parameters was entered
      


;;testing parse
(test (parse '(+ 4 5)) (plus-fcfS (num-fcfS 4) (num-fcfS 5)))
(test (parse '(* 2 4)) (mult-fcfS (num-fcfS 2) (num-fcfS 4)))
(test (parse '(- 3 4)) (bmin-fcfS (num-fcfS 3) (num-fcfS 4)))
(test (parse '(if-fcf (eq-fcf 14 13) (* 2 4) (- 25)))
      (if-fcfS (eq-fcfS (num-fcfS 14) (num-fcfS 13))
               (mult-fcfS (num-fcfS 2) (num-fcfS 4))
               (umin-fcfS (num-fcfS 25))))
(test (parse '(cond-fcf 
                (eq-fcf 1 2) -12
                (eq-fcf -3 5) 23
                ))
      (cond-fcfS (list (eq-fcfS (num-fcfS 1) (num-fcfS 2)) (num-fcfS -12)
                       (eq-fcfS (num-fcfS -3) (num-fcfS 5)) (num-fcfS 23))))
(test (parse '(define-locals ([x 5] [y 4]) (+ x y))) 
      (define-locals-fcfS (list (bind 'x (numV 5)) (bind 'y (numV 4))) (plus-fcfS (id-fcfS 'x) (id-fcfS 'y))))
(test (parse '(lambda-fcf (+ 1 2))) 
      (lambda-fcfS empty (plus-fcfS (num-fcfS 1) (num-fcfS 2))))
(test (parse '(lambda-fcf x y (+ x y))) (lambda-fcfS (list 'x 'y) (plus-fcfS (id-fcfS 'x) (id-fcfS 'y))))
(test (parse '(call-fcf (lambda-fcf x 1) 12)) (call-fcfS (lambda-fcfS (list 'x) (num-fcfS 1)) (list (num-fcfS 12))))


;;testing desugar
(test (desugar (num-fcfS 9)) (num-fcf 9))
(test (desugar (plus-fcfS (num-fcfS 7) (umin-fcfS (num-fcfS 5))))
      (plus-fcf (num-fcf 7) (plus-fcf (num-fcf 0) (mult-fcf (num-fcf -1) (num-fcf 5)))))
(test (desugar (div-fcfS (num-fcfS 4) (num-fcfS 2)))
      (div-fcf (num-fcf 4) (num-fcf 2)))
(test (desugar (eq-fcfS (num-fcfS -12) (num-fcfS 12)))
      (eq-fcf (num-fcf -12) (num-fcf 12)))
(test (desugar (if-fcfS (num-fcfS 1) (num-fcfS 3) (num-fcfS 4)))
      (if-fcf (num-fcf 1) (num-fcf 3) (num-fcf 4)))
(test (desugar (cond-fcfS
                (list (num-fcfS 4))))
      (num-fcf 4))
(test (desugar (cond-fcfS
                 (list 
                  (eq-fcfS (num-fcfS 1) (num-fcfS 2))  (num-fcfS 3)
                  (eq-fcfS (num-fcfS 1) (num-fcfS 1))  (num-fcfS 4)
                  (num-fcfS 5))))
      (if-fcf (eq-fcf (num-fcf 1) (num-fcf 2)) (num-fcf 3) (if-fcf (eq-fcf (num-fcf 1) (num-fcf 1)) (num-fcf 4) (num-fcf 5))))
(test (desugar (id-fcfS 'x)) (id-fcf 'x))
(test (desugar (lambda-fcfS (list 'x 'y 'z) (num-fcfS 12))) (lambda-fcf (list 'x 'y 'z) (num-fcf 12)))
(test (desugar (call-fcfS (num-fcfS 12) (list (num-fcfS 1) (num-fcfS 2))) )
      (call-fcf (num-fcf 12) (list (num-fcf 1) (num-fcf 2))))
(test (desugar (define-locals-fcfS (list (bind 'x (numV 1))) (num-fcfS 12)))
      (define-locals-fcf (list (bind 'x (numV 1))) (num-fcf 12)))
(test (desugar (lambda-fcfS (list 'y) (num-fcfS 12))) (lambda-fcf (list 'y) (num-fcf 12)))
(test (desugar (lambda-fcfS (list 'x) (plus-fcfS (id-fcfS 'x) (num-fcfS 1)))) (lambda-fcf (list 'x) (plus-fcf (id-fcf 'x) (num-fcf 1))))
(test (desugar (call-fcfS (lambda-fcfS (list 'y) (num-fcfS 12)) (list (num-fcfS 2)))) (call-fcf (lambda-fcf (list 'y) (num-fcf 12)) (list (num-fcf 2))))

;;testing bind-stuff
(test (bind-stuff (list '(x 12))) (list (bind 'x (numV 12))))
(test (bind-stuff (list '(x 12) '(y 24) '(z 71))) (list (bind 'x (numV 12)) (bind 'y (numV 24)) (bind 'z (numV 71))))

;;testing extend-env
(test (extend-env (list 'x 'y 'z) 
                  (list (numV 12) (numV 13) (numV 14))
                  (list (bind 'w (numV 11))))
      (list (bind 'x (numV 12)) (bind 'y (numV 13)) 
            (bind 'z (numV 14)) (bind 'w (numV 11))))

;;testing interp
(test (interp (num-fcf 12) mt-env) (numV 12))
(test (interp (plus-fcf (mult-fcf (num-fcf 2) (num-fcf 4))
                        (div-fcf (num-fcf 16) (num-fcf 8))) mt-env) (numV 10))
(test (interp (if-fcf (eq-fcf (num-fcf 4) (num-fcf 5)) (num-fcf 7) (num-fcf 34)) mt-env)
      (numV 34))
(test (interp (if-fcf (eq-fcf (num-fcf -12) (num-fcf -12)) (num-fcf 7) (num-fcf 34)) mt-env)
      (numV 7))
(test (interp (define-locals-fcf (list (bind 'x (numV 12))) (plus-fcf (id-fcf 'x) (num-fcf 10))) mt-env) (numV 22))
(test (interp (lambda-fcf (list 'x 'y) 
                          (plus-fcf (id-fcf 'x) (id-fcf 'y))) mt-env)
      (closV (list 'x 'y) (plus-fcf (id-fcf 'x) (id-fcf 'y)) mt-env))
(test (interp (lambda-fcf empty (num-fcf 12)) mt-env)
      (closV empty (num-fcf 12) mt-env))
(test (interp (call-fcf (lambda-fcf (list 'x) (num-fcf 12)) 
                        (list (num-fcf 5))) mt-env)
      (numV 12))