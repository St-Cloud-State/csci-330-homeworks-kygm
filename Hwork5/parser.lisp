;Kevin Gutierrez - CSCI 330 Programming Language Concepts
;Top down parser implementation

;Notice: Functions *must* be defined in this order, as lisp requires a function
;to be defined before being used

;G -> x|y|z|w
(defun Gfn(str)
  (print '( in Gfn)) (print str)

;begin logic
  (cond ((eql (car str) 'x) (cdr str)) ;if first char is x then return rest
        ((eql (car str) 'y) (cdr str))  ;if first char is y then return rest
        ((eql (car str) 'z) (cdr str))  ;if first char is z then return rest
        ((eql (car str) 'w) (cdr str)) ;if first char is w then return rest
        (t (list 'err))) ;else return error
)

;L -> s L’
(defun Lfn(str)
  (print '( in Lfn)) (print str)
;begin logic

  (cond ((eql (car str) 's) (L_p_fn(cdr str))) ;if first char is equal to s, call L' with rest of str as param 
        (t str) ;otherwise just return the string
        )
)

;L’ -> ε | L
(defun L_p_fn(str)
  (print '( in L_p_fn)) (print str)
;begin logic

  (cond ((eql (car str) 's) (Lfn (cdr str))) ;if first char is s then call Lfn with rest of str
        ;((eql str nil) (list 'err))         ;if string is null return error? verify that this is correct behavior here
        (t (cdr str)))                      ;else return rest of str 
)

;S -> s | dLb
(defun Sfn(str)
  (print '( in Sfn)) (print str)
;begin logic

  (cond ((eql (car str) 's) (cdr str)) ;if first char is s, return rest of string
        ((eql (car str) 'd) ;if string starts with d then
          (let ((rest (Lfn (cdr str)))) ;call Lfn on the rest
            (if (and (not (null rest)) (eql (car rest) 'b))
                (cdr rest) ;if next is b then return rest of string
                
                ;(append rest (list 'err))
                ))) ;else return error
        ;(t (append str (list 'err)))
        ) ;else error and print str
)

;E -> GE’
(defun Efn(str)
  (print '( in Efn)) (print str)
;begin logic

  (let ((g (Gfn str))) ;first call Gfn
    (if (not (eq g 'err)) ;since Gfn returns err when error, check if error
        (E_p_fn g) ; If G is valid, continue parsing E'
        ;(append str (list 'err))
        )) ;else return error with rest
)

;;E’ -> oGE’ | ε
(defun E_p_fn(str)
  (print '( in E_p_fn)) (print str)
;begin logic

  (cond ((eql (car str) 'o) ;if first is o then
         (let ((rest (Gfn (cdr str)))) ;call gfn
           (if (not (eq rest 'err)) ;then if result of rest from gfn is not an error then
               (E_p_fn rest) ;call E' with rest of rest var
               
               ;(append rest (list 'err))
               ))) ; Otherwise, error

        (t str)) ; Epsilon case, return rest of string
)

;;I -> iES I’
(defun Ifn(str)
  (print '( in Ifn)) (print str)
;begin logic

  (if (eql (car str) 'i)
      (let ((rest (Efn (cdr str)))) ;call Efn
        (if (not (eq rest 'err)) ;if result not error then
            (let ((sfnResult (Sfn rest))) ;define rest var and call Sfn with rest
              (if (not (eq sfnResult 'err))
                  (I_p_fn sfnResult) ;call I_p_fn
                  (append sfnResult (list 'err))))) ; Error if Sfn fails
        ;(append rest (list 'err))
        )) ; Error if Efn fails
      ;(append str (list 'err)) ; Error if input doesn’t start with 'i'
)

;I’ -> ε | eS
(defun I_p_fn(str)
  (print '( in I_p_fn)) (print str)
;begin logic

  (cond 
    ((null str) str)  ; If `str` is nil, return it (epsilon case)
    ((eql (car str) 'e) (Sfn (cdr str)))  ; If first element is 'e', call Sfn
    (t str))  ; Otherwise, return str as it is (valid epsilon case)
)
