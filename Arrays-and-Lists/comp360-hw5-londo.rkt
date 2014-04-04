#| Laura Londo
   COMP360
   2 March 2014
   HW5

   I pledge to demonstrate personal and academic integrity in all matters with 
   my fellow students, the faculty, and the administration here at USD. I 
   promise to be honest and accountable for my actions; and to uphold the 
   statutes of scholastic honesty to better myself and those around me.
|#

#lang plai-typed
;(print-only-errors #t)

(define-type Value
  [numV (n : number)]
  [closV (param : symbol)
         (body : ExprC) (env : Env)]
  [emptyV]
  [listV (li : (listof Value)) (type : symbol)]
  [arrayV (boxes : (listof (boxof Value))) (size : number) (type : symbol)]
  [nullV]
  )


(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [varC (name : symbol)]
  [setC (name : symbol) (val : ExprC)]
  [appC (func : ExprC) (arg : ExprC)]
  [lamC (param : symbol) (body : ExprC)]
  [seqC (expr1 : ExprC) (expr2 : ExprC)]
  [emptyC]
  [consC (new : ExprC) (li : ExprC)]
  [firstC (li : ExprC)]
  [restC (li : ExprC)]
  [arrayC (type : symbol) (size : ExprC)]
  [getElementAtC (arr : ExprC) (index : ExprC)]
  [setElementAtC (arr : ExprC) (index : ExprC) (new : ExprC)]
  )

;; A binding will be an entry in our map from symbols to values
(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (val : Location)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (loc : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define extend-store cons)

(define-type Result
  [val+store (val : Value) (sto : Store)])

;; Returns next number in sequence of all integers from 0 to infinity
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;; lookup: symbol * Env -> number
;; Finds value associated with symbol in the given environment
(define (lookup [name : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? name (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup name (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "location not found")]
    [else (cond
            [(= loc (cell-loc (first sto)))
             (cell-val (first sto))]
            [else (fetch loc (rest sto))])]))

;; numV-op: (number number -> number) * Value * Value -> Value
;; Performs a binary operation on the numbers stored in numVs (2 params),
;; returning the result as a new numV
(define (numV-op [op : (number number -> number)]
                 [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'numV-op "one argument was not a number")]))



;; getType: Value -> symbol
;; returns a symbol representation of the type of value passed in
(define (getType [v : Value]) : symbol
  (type-case Value v
    [numV (n) 'numV]
    [closV (p b e) 'closV]
    [emptyV () 'emptyV]
    [listV (l t) 'listV]
    [arrayV (b s t) 'arrayV]
    [nullV () 'nullV]))

;testing getType
(test (getType (numV 5))
      'numV)

(test (getType (nullV))
      'nullV)


;; initArray: number * (listof (boxof Value)) -> (listof (boxof Value))
;; constructs a list of null boxes of the specified size
;; helper function for interp of arrayC
(define (initArray [size : number] [list : (listof (boxof Value))])
  : (listof (boxof Value))
  (cond
    ;if the user passed in negative size
    [(< size 0) (error 'initArray "cannot have negative array size")]
    ;add a new box to the list
    [(> size 0) (initArray (- size 1) (cons (box (nullV)) list))] 
    ;base case: list is the correct length, return the list
    [else list])) 

;testing initArray
(test (initArray 3 empty)
      (list (box (nullV)) (box (nullV)) (box (nullV))))

(test/exn (initArray -1 empty)
          "cannot have negative array size")
      

;; getElement: number * (listof (boxof Value)) -> Value
;; gets the value in the list at the specified index
;; helper function for interp of getElementAtC
(define (getElement [index : number] [list : (listof (boxof Value))]) : Value
  (cond
    ;if the index is invalid
    [(or (< index 0) (>= index (length list))) 
     (error 'initArray "array index out of bounds")] 
    ;have not yet reached the intended item, recurse
    [(> index 0) (getElement (- index 1) (rest list))] 
    ;base case: the first item in the list is the intended item
    [else (unbox (first list))] 
    ))

;testing getElement
(test/exn (getElement 0 empty)
      "array index out of bounds")

(test/exn (getElement 2 (list (box (numV 1)) (box (numV 4))))
      "array index out of bounds")

(test/exn (getElement -4 (list (box (numV 1)) (box (numV 4)) 
                               (box (numV 6)) (box (numV 2))))
      "array index out of bounds")

(test (getElement 2 (list (box (numV 1)) (box (numV 4)) 
                          (box (numV 6)) (box (numV 2))))
      (numV 6))
                    


;; setElement: number * Value * (listof (boxof Value))
;; sets the specified element in the given array to the new value
(define (setElement [index : number] [new : Value] [list : (listof (boxof Value))])
  (cond
    ;index is negative
    [(< index 0) (error 'setElement "array index out of bounds")] 
    ;index too large
    [(>= index (length list)) (error 'setElement "array index out of bounds")] 
    ;else set the box at that index to new value
    [else (set-box! (list-ref list index) new)])) 
    



;; interp: ExprC * Env * (listof FunDefC) -> number
;; Evaulates the given expression in the specified environment.
(define (interp [expr : ExprC]
                [env : Env]
                [sto : Store]) : Result
  (type-case ExprC expr
    [numC (n) (val+store (numV n) sto)]
    [lamC (a b) (val+store (closV a b env) sto)]
    [varC (s) (val+store (fetch (lookup s env) sto) sto)]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [val+store (b1-v b1-s)
                               (interp b2 env b1-s)])]
    [setC (name value) (type-case Result (interp value env sto)
                         [val+store (value-val value-sto)
                                    (val+store value-val
                                               (extend-store (cell (lookup name env)
                                                                   value-val)
                                                             value-sto))])]
    [plusC (l r) (type-case Result (interp l env sto)
                   [val+store (l-v l-s)
                              (type-case Result (interp r env l-s)
                                [val+store (r-v r-s)
                                           (val+store (numV-op + l-v r-v)
                                                      r-s)])])]
    [multC (l r) (type-case Result (interp l env sto)
                   [val+store (l-v l-s)
                              (type-case Result (interp r env l-s)
                                [val+store (r-v r-s)
                                           (val+store (numV-op * l-v r-v)
                                                      r-s)])])]
    [appC (f a) (type-case Result (interp f env sto)
                  [val+store (f-v f-s)
                             (type-case Result (interp a env f-s)
                               [val+store (a-v a-s)
                                          (let ([loc (new-loc)])
                                            (interp (closV-body f-v)
                                                    (extend-env (bind (closV-param f-v)
                                                                      loc)
                                                                (closV-env f-v))
                                                    (extend-store (cell loc a-v)
                                                                  a-s)))])]
                  )]
    ; returns an empty value
    [emptyC () (val+store (emptyV) sto)]
    
    ; returns a list with the new element at the front of the given list
    [consC (new li) 
           (type-case Result (interp new env sto)
             [val+store (new-v new-s)
                 (type-case Result (interp li env new-s)
                   [val+store (li-v li-s)
                       (type-case Value li-v
                         [numV (n) (error 'interp "this is a numV, not a listV")]
                         [closV (p b e) (error 'interp "this is a closV, not a listV")]
                         ;if cons to an empty list
                         [emptyV () 
                             (val+store (listV (list new-v) (getType new-v)) li-s)] 
                         ;if cons to another list
                         [listV (l t) 
                             (if (eq? t (getType new-v))
                                 (val+store (listV (cons new-v l) 
                                                   (getType new-v)) li-s)
                                 (error 'interp "list type mismatch"))]
                         [arrayV (b s t) (error 'interp "this is a numV, not a listV")]
                         [nullV () (error 'interp "this is a nullV, not a listV")])])])]
    ; returns the first item in the given list
    [firstC (li) 
            (type-case Result (interp li env sto)
              [val+store (li-v li-s)
                   (type-case Value li-v ;make sure the list is a listV
                     [numV (n) (error 'interp "this is a numV, not a listV")]
                     [closV (p b e) (error 'interp "this is a closV, not a listV")]
                     [emptyV () (error 'interp "the list is empty")]
                     [listV (list type) (val+store (first list) li-s)]
                     [arrayV (b s t) (error 'interp "this is a numV, not a listV")]
                     [nullV () (error 'interp "this is a nullV, not a listV")])])]
    ; returns the given list minus the first item
    [restC (li) 
        (type-case Result (interp li env sto)
          [val+store (li-v li-s)
               (type-case Value li-v ;make sure list is a listV or emptyV
                 [numV (n) (error 'interp "this is a numV, not a listV")]
                 [closV (p b e) (error 'interp "this is a closV, not a listV")]
                 [emptyV () (error 'interp "the list is empty")]
                 [listV (list type) 
                        (if (> (length list) 1) ;if the list length is greater than 1,
                            (val+store (listV (rest list) type) li-s) ;return the rest 
                            (val+store (emptyV) li-s))] ;else return empty list
                 [arrayV (b s t) (error 'interp "this is a numV, not a listV")]
                 [nullV () (error 'interp "this is a nullV, not a listV")])])]
    ;creates a new empty array of a certain size
    [arrayC (type size) 
            (type-case Result (interp size env sto)
              [val+store (size-v size-s)
                         (type-case Value size-v
                           ;send to helper method to create a list of empty boxes
                           [numV (n) (val+store (arrayV (initArray n empty) n type) 
                                                size-s)]
                           [closV (p b e) (error 'interp "size is a closV, not a numV")]
                           [emptyV () (error 'interp "size is an emptyV, not a numV")]
                           [listV (list type) (error 'interp "size is a listV, not a numV")]
                           [arrayV (b s t) (error 'interp "size is a numV, not a numV")]
                           [nullV () (error 'interp "size is a nullV, not a numV")])])]            
    ; get the array element at the specified index
    [getElementAtC (array index) 
         (type-case Result (interp array env sto) 
           [val+store (array-v array-s)
                ;make sure the array is an arrayV
                (type-case Value array-v 
                  [numV (n)  (error 'interp "this is a numV, not an arrayV")]
                  [closV (p b e) (error 'interp "this is a closV, not an arrayV")]
                  [emptyV () (error 'interp "this is an emptyV, not an arrayV")]
                  [listV (list type) (error 'interp "this is a listV, not an arrayV")]
                  [arrayV (b s t) 
                       ;make sure index is a numV
                       (type-case Result (interp index env array-s)
                         [val+store (index-v index-s)
                              (type-case Value index-v
                                ;get the element from the list
                                [numV (n) (val+store (getElement n b) 
                                                     index-s)]
                                [closV (p b e) (error 'interp "index is a closV, not a numV")]
                                [emptyV () (error 'interp "index is an emptyV, not a numV")]
                                [listV (list type) (error 'interp "index is a listV, not a numV")]
                                [arrayV (b s t) (error 'interp "index is a numV, not a numV")]
                                [nullV () (error 'interp "index is a nullV, not a numV")])])]
                  [nullV () (error 'interp "this is a nullV, not an arrayV")])])]
    ; set the array element at the specified index to the new value
    [setElementAtC (array index new) 
         (type-case Result (interp array env sto)
           [val+store (array-v array-s)
                ;make sure this value is an arrayV
                (type-case Value array-v 
                  [numV (n)  (error 'interp "this is a numV, not an arrayV")]
                  [closV (p b e) (error 'interp "this is a closV, not an arrayV")]
                  [emptyV () (error 'interp "this is an emptyV, not an arrayV")]
                  [listV (list type) (error 'interp "this is a listV, not an arrayV")]
                  [arrayV (b s t) 
                       (type-case Result (interp index env array-s)
                         [val+store (index-v index-s)
                              ;make sure the index is a numV
                              (type-case Value index-v 
                                [numV (n) 
                                      ;get the new Value to add
                                      (type-case Result (interp new env index-s) 
                                        [val+store (new-v new-s)
                                             ;make sure the types match
                                             (if (eq? t (getType new-v)) 
                                                 (begin
                                                   (setElement n new-v b)
                                                   (val+store array-v index-s))
                                                 (error 'interp "array type mismatch"))])]
                                [closV (p b e) (error 'interp "index is a closV, not a numV")]
                                [emptyV () (error 'interp "index is an emptyV, not a numV")]
                                [listV (list type) (error 'interp "index is a listV, not a numV")]
                                [arrayV (b s t) (error 'interp "index is a numV, not a numV")]
                                [nullV () (error 'interp "index is a nullV, not a numV")])])]
                  [nullV () (error 'interp "this is a nullV, not an arrayV")])])]))



; testing emptyC
(test (interp (emptyC) mt-env mt-store) 
      (val+store (emptyV) mt-store))

; testing consC
(test (interp (consC (numC 10) (emptyC)) mt-env mt-store) 
      (val+store (listV (list (numV 10)) 'numV) mt-store))

(test (interp (consC (numC 10) (consC (numC 11) (emptyC))) mt-env mt-store) 
      (val+store (listV (list (numV 10) (numV 11)) 'numV) mt-store))

; testing firstC
(test/exn (interp (firstC (emptyC)) mt-env mt-store) 
      "the list is empty")

(test (interp (firstC (consC (numC 10) (consC (numC 11) (emptyC)))) 
              mt-env mt-store)
      (val+store (numV 10) mt-store))

; testing restC
(test/exn (interp (restC (emptyC)) mt-env mt-store)
      "the list is empty")

(test (interp (restC (consC (numC 10) (emptyC))) mt-env mt-store)
      (val+store (emptyV) mt-store))

(test (interp (restC (consC (numC 10) (consC (numC 11) (emptyC)))) 
              mt-env mt-store) 
      (val+store (listV (list (numV 11)) 'numV) mt-store))

; testing arrayC
(test (interp (arrayC 'numV (numC 0)) mt-env mt-store)
      (val+store (arrayV empty 0 'numV) mt-store))

(test (interp (arrayC 'numV (numC 3)) mt-env mt-store)
      (val+store (arrayV (list (box (nullV)) (box (nullV)) (box (nullV))) 
                         3 'numV) mt-store))

; testing get & set element
(test/exn (interp (setElementAtC (arrayC 'numV (numC 2)) (numC 3) (numC 12)) 
                  mt-env mt-store)
      "index out of bounds")

(test/exn (interp (setElementAtC (arrayC 'numV (numC 2)) (numC -3) (numC 12)) 
                  mt-env mt-store)
      "index out of bounds")

(test (interp (setElementAtC (arrayC 'numV (numC 2)) (numC 0) (numC 12)) 
              mt-env mt-store)
      (val+store (arrayV (list (box (numV 12)) (box (nullV))) 2 'numV) 
                 mt-store))

(test/exn (interp (getElementAtC (arrayC 'numV (numC 1)) (numC 2)) 
                  mt-env mt-store)
      "index out of bounds")

(test (interp (getElementAtC (arrayC 'numV (numC 2)) (numC 1)) 
              mt-env mt-store)
      (val+store (nullV) mt-store))

(test (interp (getElementAtC
               (setElementAtC (arrayC 'numV (numC 3)) (numC 2) (numC 10)) 
               (numC 2)) mt-env mt-store)
      (val+store (numV 10) mt-store))
      

 
 
 
 






