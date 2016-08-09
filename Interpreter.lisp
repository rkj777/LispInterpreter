;Name: Rajan Jassal

;This is the inital function that will handle all the built in function and start the process of
;handling user programmed functions
(defun fl-interp (E P)
    
  (cond 
    ;Handles atoms and nil
	((atom E) E) 
        (t
           (let ( (f (car E))  (arg (cdr E)) )
	      (cond 
                ; handle built-in functions
                ((eq f 'if) (if (fl-interp (car arg) P) (fl-interp (cadr arg) P ) (fl-interp (caddr arg) P )))
                ((eq f 'null) (null (fl-interp (car arg) P)))
                ((eq f 'null) (null (fl-interp (car arg) P)))
                ((eq f 'atom) (atom (fl-interp (car arg) P)))
                ((eq f 'eq) (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f 'first)  (car (fl-interp (car arg) P)))
                ((eq f 'rest) (cdr (fl-interp (car arg) P)))
                ((eq f 'cons) (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f 'equal) (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f 'number) (numberp (fl-interp (car arg) P)))
                ((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f '>) (> (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f '<) (< (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f '=) (= (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f 'and) (and (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f 'or) (or (fl-interp (car arg) P) (fl-interp (cadr arg) P) ))
                ((eq f 'not) (not (fl-interp (car arg) P)))
                
                ;Handling the user programed functions. Will check if the 
                ;function exists and then execute it if it does.
                ((isProgrammed f P) (findFunction f arg P P))
                
                ;Will return everything else as data 
                (t E))))))

;This function will check if an function name is
;defined by the user
(defun isProgrammed (f P)
    (cond
        ((null P) nil)
        ((eq f (caar P)) T)
        (T (isProgrammed f (cdr P)) )))

;This function will actually find the programmed function
;and sent it to setupFunction to setup a context to interpret the function.
;Note that a full unedited version of P will be passed along as the intial P will get
;destryed in the process of evaulating the user function. This will be used for interpters
;after the function has executed.
(defun findFunction (f arg P fullP)
    (cond
        ((null P) nil)
        
        ;For a match in function name the function will be sent
        ;to setupFunction to be processed. Note an empty context is passed in to start
        ((eq f (caar P)) (setupFunction arg (cdar P) () () fullP))

        ;Going through the program list
        (T (findFunction f arg (cdr P) fullP) )))

;This function will set up the context with name/value paired lists. This will be
;based on the arguments given to programmed function
(defun setupFunction (arg P names values fullP)
        (cond

            ;If an = is encoutered the context is fully created
            ((eq (car P ) '= ) (fl-interp-custom (cadr P) fullP names values) )

            ;Edge case of converting the value true to T so it can act as a proper lisp value
            ((eq (car arg ) 'true ) (setupFunction (cdr arg) (cdr P) (cons (car P) names) (cons T values ) fullP))

            ;Moving along the parameters passed into the user defined prgram
            (T (setupFunction (cdr arg) (cdr P) (cons (car P) names) (cons (car arg) values ) fullP))))

;This is a custom fl-interp that will be used in order to process user
;defined functions. It has all the base functionality as the above interpreter but will 
;also be able to check variables aganist the current context and run recursive functions
;This method was chosen in order to start of the first interper with an empty context
(defun fl-interp-custom (E P names values)
  
  (cond
    ;Will use functions to check if elements match anything saved
    ;in the current context. If so it will return the value for that
    ;element
    ((valueExist E names) (findValueExist E names values) ) 
    ((atom E) E) 
        (t
           (let ( (f (car E))  (arg (cdr E)) )
          (cond 
                ; handle built-in functions
                ((eq f 'if) (if (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) (fl-interp-custom (caddr arg) P names values )))
                ((eq f 'null) (null (fl-interp-custom (car arg) P names values)))
                ((eq f 'null) (null (fl-interp-custom (car arg) P names values)))
                ((eq f 'atom) (atom (fl-interp-custom (car arg) P names values)))
                ((eq f 'eq) (eq (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values)))
                ((eq f 'first)  (car (fl-interp-custom (car arg) P names values)))
                ((eq f 'rest) (cdr (fl-interp-custom (car arg) P names values)))
                ((eq f 'cons) (cons (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) ))
                ((eq f 'equal) (equal (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) ))
                ((eq f 'number) (numberp (fl-interp-custom (car arg) P names values)))
                ((eq f '+) (+ (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) ))
                ((eq f '-) (- (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) ))
                ((eq f '*) (* (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) ))
                ((eq f '>) (> (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) ))
                ((eq f '<) (< (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) ))
                ((eq f '=) (= (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) ))
                ((eq f 'and) (and (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values)))
                ((eq f 'or) (or (fl-interp-custom (car arg) P names values) (fl-interp-custom (cadr arg) P names values) ))
                ((eq f 'not) (not (fl-interp-custom (car arg) P names values)))

                ;This will handle calls to user defined programs. It checks if there is a program that mathces the 
                ;name and then will execute with the interpeted inputs
                ((isProgrammed f P) (findFunction f (cons (fl-interp-custom (car arg) P names values) nil) P P ))
                (t E))))))

;This function will check if a element exists in
;the current context by checking its name
(defun valueExist (f names)

    (cond
        ((null names) nil )
        ((eq f (car names)) T)
        (T (valueExist f (cdr names)))))

;This function will return the value for a matching name in the context passed in
(defun findValueExist (f names values)
    (cond
        ((null names) nil )
        ((eq f (car names)) (car values))
        (T (findvalueExist f (cdr names) (cdr values)))))
