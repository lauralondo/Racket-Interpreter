#| Laura Londo
   COMP360
   17 February 2014
   HW2

   I pledge to demonstrate personal and academic integrity in all matters with my fellow students, 
   the faculty, and the administration here at USD. I promise to be honest and accountable for my
   actions; and to uphold the statutes of scholastic honesty to better myself and those around me.
|#

#lang plai-typed


;;the core language
(define-type ArithWithCond
  [num-awc (n : number)]
  [plus-awc (l : ArithWithCond) (r : ArithWithCond)]
  [mult-awc (l : ArithWithCond) (r : ArithWithCond)]
  [div-awc  (l : ArithWithCond) (r : ArithWithCond)]  ;;in the core language because it would be extremely dificult to express
                                                      ;;using only = * or /
  [eq-awc   (l : ArithWithCond) (r : ArithWithCond)]  ;;in the core language because eq cannot be expressed with only = * or /
  [if-awc   (s : ArithWithCond) (t : ArithWithCond) (f : ArithWithCond)])  ;;in core language because if cannot 
                                                                           ;;be expressed using only + * or /


;;the sugary language
(define-type ArithWithCondS
  [num-awcS (n : number)]
  [plus-awcS (l : ArithWithCondS) (r : ArithWithCondS)]
  [mult-awcS (l : ArithWithCondS) (r : ArithWithCondS)]
  [bmin-awcS (l : ArithWithCondS) (r : ArithWithCondS)] ;;can be expressed with plus and mult
  [umin-awcS (r : ArithWithCondS)]                      ;;can be expressed with plus and mult
  [div-awcS  (l : ArithWithCondS) (r : ArithWithCondS)]
  [mod-awcS  (l : ArithWithCondS) (r : ArithWithCondS)] ;;can be expressed with subtraction
  [pow-awcS  (l : ArithWithCondS) (r : ArithWithCondS)] ;;can be expressed with nested mult statements
  [eq-awcS   (l : ArithWithCondS) (r : ArithWithCondS)] 
  [if-awcS (s : ArithWithCondS) 
           (t : ArithWithCondS) 
           (f : ArithWithCondS)]
  [cond-awcS (l : (listof ArithWithCondS))])  ;;can be expressed with nested if statements
                                              ;;contains a list of ArithWithCondS to allow more than one argument
  


;; desugars a mod-awcS to combinaitons of ArithWithCond functions
;; used in desugar
(define (mod [left : ArithWithCondS] [right : ArithWithCondS]) : ArithWithCond
  (let ([l (interp (desugar left))] [r (interp (desugar right))])
    (cond
      [(> r l) (desugar left)] ;;base-case, right does not fit into the left, the remainder is the left
      [(or (< r 0) (< l 0)) (error 'mod "no negatives!!")]
      [else (mod (bmin-awcS left right) right)]))) ;;subtractleft from the right and recursively try again


;; desugars a pow-awcS to combinations of multC's
;; used it in desugar
(define (pow [left : ArithWithCondS] [right : ArithWithCondS]) : ArithWithCond
  (let ([exp (interp (desugar right))])
    (cond
      [(> exp 1) (mult-awc (desugar left) (pow left (bmin-awcS right (num-awcS 1))))] ;;if the exponent is greater than 1, recurse
      [(= exp 1) (desugar left)] ;;if the exponent is 1, return the original number
      [(= exp 0) (num-awc 1)] ;;if the exponent is 0, return 1
      [else (error 'pow "no negatives!!")])))


;; s-expression -> ArithWithCondS
(define (parse [s : s-expression]) : ArithWithCondS
  (cond
    [(s-exp-number? s) (num-awcS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(symbol=? '+ (s-exp->symbol (first sl)))
          (plus-awcS (parse (second sl)) (parse (third sl)))]
         [(symbol=? '* (s-exp->symbol (first sl)))
          (mult-awcS (parse (second sl)) (parse (third sl)))]
         [(symbol=? '- (s-exp->symbol (first sl)))
          (cond 
            [(= (length sl) 2) (umin-awcS (parse (second sl)))]         ;;its unary minus
            [else (bmin-awcS (parse (second sl)) (parse (third sl)))])] ;;else its binary minus
         [(symbol=? '/ (s-exp->symbol (first sl)))
          (div-awcS (parse (second sl)) 
                    (parse (third sl)))]
         [(symbol=? '% (s-exp->symbol (first sl)))
          (mod-awcS (parse (second sl)) 
                    (parse (third sl)))]
         [(symbol=? '^ (s-exp->symbol (first sl)))
          (pow-awcS (parse (second sl)) 
                    (parse (third sl)))]
         [(symbol=? '= (s-exp->symbol (first sl)))
          (eq-awcS (parse (second sl)) 
                   (parse (third sl)))]
         [(symbol=? 'if (s-exp->symbol (first sl)))
          (if-awcS (parse (second sl))
                   (parse (third sl))
                   (parse (fourth sl)))]
         [(symbol=? 'cond (s-exp->symbol (first sl)))
          (cond-awcS (map parse (rest sl)))] ;;parse each element of the list
         [else (error 'parse "bad stuff happened!")]))]
    [else (error 'parse "lonely symbol")]))


;;testing parse
(test (parse '(+ 4 5)) (plus-awcS (num-awcS 4) (num-awcS 5)))
(test (parse '(* 2 4)) (mult-awcS (num-awcS 2) (num-awcS 4)))
(test (parse '(- 3 4)) (bmin-awcS (num-awcS 3) (num-awcS 4)))
(test (parse '(/ (% 14 5) 2))
      (div-awcS (mod-awcS (num-awcS 14) (num-awcS 5)) (num-awcS 2)))
(test (parse '(if (= 14 13) (^ 2 4) (- 25)))
      (if-awcS (eq-awcS (num-awcS 14) (num-awcS 13))
               (pow-awcS (num-awcS 2) (num-awcS 4))
               (umin-awcS (num-awcS 25))))

(test (parse '(cond 
                (= 1 2) -12
                (= -3 5) 23
                ))
      (cond-awcS (list (eq-awcS (num-awcS 1) (num-awcS 2)) (num-awcS -12)
                       (eq-awcS (num-awcS -3) (num-awcS 5)) (num-awcS 23))))


;; ArithWithCond -> number
;; interprets functions of the core language
(define (interp [arg : ArithWithCond]) : number
  (type-case ArithWithCond arg
    [num-awc (n) n]
    [plus-awc (l r) (+ (interp l) (interp r))]
    [mult-awc (l r) (* (interp l) (interp r))]
    [div-awc  (l r) (/ (interp l) (interp r))]  
    [eq-awc   (l r) (if (= (interp l) (interp r)) 1 0)]   
    [if-awc (s t f) (if (= (interp s) 0) (interp f) (interp t))]
    ))


;;testing interp
(test (interp (num-awc 12)) 12)
(test (interp (plus-awc (mult-awc (num-awc 2) (num-awc 4))
                        (div-awc (num-awc 16) (num-awc 8)))) 10)
(test (interp (if-awc (eq-awc (num-awc 4) (num-awc 5)) (num-awc 7) (num-awc 34)))
      34)
(test (interp (if-awc (eq-awc (num-awc -12) (num-awc -12)) (num-awc 7) (num-awc 34)))
      7)




;; ArithWithCondS -> ArithWithCond
;; convert sugary syntax into core language
(define (desugar [sugar : ArithWithCondS]) : ArithWithCond
  (type-case ArithWithCondS sugar
    [num-awcS (n) (num-awc n)]
    [plus-awcS (l r) (plus-awc
                      (desugar l)
                      (desugar r))]
    [mult-awcS (l r) (mult-awc
                      (desugar l)
                      (desugar r))]
    [bmin-awcS (l r) (plus-awc       ;;translates binary minus to addition of the neg of the second argument
                      (desugar l)
                      (mult-awc
                       (num-awc -1)
                       (desugar r)))]
    [umin-awcS (v) (desugar (bmin-awcS (num-awcS 0) v))] ;;unary minus translates to subtracting the argument from 0
    [div-awcS (l r) (div-awc      ;;in the core language
                     (desugar l) 
                     (desugar r))]
    [mod-awcS (l r) (mod l r)]      ;;uses a helper function to translate mod to a series of ArithWithCond functions
    [pow-awcS (l r) (pow l r)]      ;;uses a helper function to translate mod to a series of ArithWithCond functions
    [eq-awcS (l r)  (eq-awc (desugar l) (desugar r))] ;;in the core language 
    [if-awcS (s t f) (if-awc (desugar s)     ;;in the core language 
                             (desugar t)
                             (desugar f))]
    [cond-awcS (l) (cond                     ;;expresses cond in terms of nested if statements
                     [(> (length l) 1) (if-awc (desugar (first l))  ;;first item is the conditional statement
                                               (desugar (second l)) ;;second item is the result if the statement is true
                                               (desugar (cond-awcS (rest (rest l)))))] ;;rest of the list is made into a nested
                                                                                       ;;if statement in the case that the
                                                                                       ;;previous statement is false
                     [(= (length l) 1) (desugar (first l))]          ;;if there is only 1 item left in the list just return it
                                                                     ;;as the default else case
                     [else (error 'desugar "need a default condition")])])) ;;else the wrond number of parameters was entered
                                       
               

;;testing desugar
(test (desugar (num-awcS 9)) (num-awc 9))
(test (desugar (plus-awcS (num-awcS 7) (umin-awcS (num-awcS 5))))
      (plus-awc (num-awc 7) (plus-awc (num-awc 0) (mult-awc (num-awc -1) (num-awc 5)))))
(test (desugar (div-awcS (num-awcS 4) (num-awcS 2)))
      (div-awc (num-awc 4) (num-awc 2)))
(test (desugar (mod-awcS (num-awcS 4) (num-awcS 3)))
      (plus-awc (num-awc 4) (mult-awc (num-awc -1) (num-awc 3))))
(test (desugar (pow-awcS (num-awcS 2) (num-awcS 2)))
      (mult-awc (num-awc 2) (num-awc 2)))
(test (desugar (pow-awcS (num-awcS 5) (num-awcS 4)))
      (mult-awc (num-awc 5) (mult-awc (num-awc 5) (mult-awc (num-awc 5) (num-awc 5)))))
(test (desugar (eq-awcS (num-awcS -12) (num-awcS 12)))
      (eq-awc (num-awc -12) (num-awc 12)))
(test (desugar (if-awcS (num-awcS 1) (num-awcS 3) (num-awcS 4)))
      (if-awc (num-awc 1) (num-awc 3) (num-awc 4)))
(test (desugar (cond-awcS
                (list (num-awcS 4))))
      (num-awc 4))
(test (desugar (cond-awcS
                 (list 
                  (eq-awcS (num-awcS 1) (num-awcS 2))  (num-awcS 3)
                  (eq-awcS (num-awcS 1) (num-awcS 1))  (num-awcS 4)
                  (num-awcS 5))))
      (if-awc (eq-awc (num-awc 1) (num-awc 2)) (num-awc 3) (if-awc (eq-awc (num-awc 1) (num-awc 1)) (num-awc 4) (num-awc 5))))



;;testing mod helper function
(test (mod (num-awcS 2) (num-awcS 3)) (num-awc 2))
(test (mod (num-awcS 3) (num-awcS 3))
      (plus-awc (num-awc 3) (mult-awc (num-awc -1) (num-awc 3))))
(test (mod (num-awcS 4) (num-awcS 3)) 
      (plus-awc (num-awc 4) (mult-awc (num-awc -1) (num-awc 3))))

;;testing pow helper function
(test (pow (num-awcS 2) (num-awcS 2)) 
      (mult-awc (num-awc 2) (num-awc 2)))
(test (pow (num-awcS 2) (num-awcS 0)) 
      (num-awc 1))



;;a combined test
(test (interp (desugar (parse 
                        '(cond 
                           (= 1 2) 4
                           (= 1 1) (* 4 (- (% 10 4)))
                           6))))
      -8)
      
      
      
     