#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;;*******************************************************************
;;Program 1 : Functionaity Scheme
;;
;;Descri: Build an interpreter that uses scheme to interpret 
;;SBIR in a mostly functional style
;;
;;Stephen Woodbury - 1429496 - swoodbur
;;
;;sbi.scm
;;*******************************************************************

;;***********************************ERROR HANDLING******************
;Define *stderr*
(define *stderr* (current-error-port)
)

;Find the basename of the filename provided
(define *run-file*
  (let-values
    (((dirpath basepath root?)
      (split-path (find-system-path 'run-file))))
    (path->string basepath))
)

;;Exit and print the error provided
(define (die list)
  (for-each (lambda (item) (display item *stderr*)) list)
  (newline *stderr*)
  (exit 1)
)

;;Print usage information and die
(define (usage-exit)
  (die `("Usage: " ,*run-file* " filename"))
)


;;*********************HELPER FCT'S P1*******************************
;;Read in the file
(define (readlist-from-inputfile filename)
  (let ((inputfile (open-input-file filename)))
    (if (not (input-port? inputfile))
      (die `(,*run-file* ": " ,filename ": Failed Opening"))
      (let ((program (read inputfile)))
  (close-input-port inputfile)
  program)))
)

;;s_dim definition - called in *function-table*
(define (s_dim expression)
  (symbol-variable_put! (caar expression) 
       (make-vector (value (cadar expression))) )
  (symbol-function_put! (caar expression) 
      (lambda(x) (vector-ref 
             (symbol-variable_get (caar expression)) (- x 1))))
)

;;s_let expression - called in *function-table*
(define (s_let expression)
  (if (pair? (car expression))
    (vector-set! (symbol-variable_get
       (caar expression)) (- (value (cadar expression)) 1) 
     (value (cadr expression)))
    (let ((result (value (cadr expression))))
      (symbol-variable_put! (car expression) result)
)))

;;s_goto expression - called in *function-table*
(define (s_goto label program)
  (execProg program (symbol-label_get (car label)))
)

;; s_print definition - called in *function-table*
(define (s_print token) 
  (if (not (null?  token) )
    (begin
      (if (string? (car token))  
  (display (car token))
  (display (value (car token)))      
  )         
      (s_print (cdr token))
      )
    (newline))
)

;;s_input definition - called in *function-table*
(define (s_input expression)
  (symbol-variable_put! 'inputcount 0)
  (define (symbol-get_input expression)
    (when (not (null? (car expression)))
      (symbol-variable_put! (car expression) (void))
      (let ((object (read)))
  (if (eof-object? object)
    (symbol-variable_put! 'inputcount -1)
     (if (number? object)
       (begin
        (symbol-variable_put! (car expression) object)
        (symbol-variable_put! 'inputcount 
          (+ (symbol-variable_get 'inputcount) 1))
        )
       (printf "improper #: ~a~n" object)
      )
    )
   ) 
      (when (not (null? (cdr expression)))
  (symbol-get_input (cdr expression)))
      )  
    )
  (symbol-get_input expression)
)

;;*******************LABEL TABLE SETUP*******************************
;;Creating our Label Table
(define *label-table* (make-hash)
)

;;Creating getters and setter for our Label Table
;;getter
(define (symbol-label_get key)
  (hash-ref *label-table* key)
)

;;setter
(define (symbol-label_put! key value)
  (hash-set! *label-table* key value )
)

;;*******************VARIABLE TABLE SETUP****************************
(define *variable-table* (make-hash)
)

;;Creating getters and setters for our Variable Table
;;getter
(define (symbol-variable_get key)
  (hash-ref *variable-table* key )
)

;;setter
(define (symbol-variable_put! key value)
  (hash-set! *variable-table* key value)
)

;;Filling pre-initialized values for the Variable Table
(for-each
  (lambda (pair)
    (symbol-variable_put! (car pair) (cadr pair)))
  `(
    (pi         3.141592653589793238462643383279502884197169399)
    (e          2.718281828459045235360287471352662497757247093)
    (inputcount 0)
    )
)

;;*******************FUNCTION TABLE SETUP****************************
;;Creating our Function Table
(define *function-table* (make-hash)
)

;;Creating getters and setters for our Function Table
;;getter
(define (symbol-function_get key)
  (hash-ref *function-table* key)
)

;;setter
(define (symbol-function_put! key value)
  (hash-set! *function-table* key value)
)

;;Filling Function Table Contents
(for-each
  (lambda (pair)
    (symbol-function_put! (car pair) (cadr pair)))
  `(
    (/       ,(lambda (x y)  (/ x (if (equal? y 0) 0.0 y))))
    (quot    ,(lambda (x y) (truncate (/ x y))))
    (%       ,(lambda (x y) (- x (* (div x y) y))))
    (+       ,+)
    (-       ,-)
    (*       ,*)
    (^       ,(lambda (x y) (expt x y)))
    (exp     ,exp)
    (sqrt    ,sqrt)
    (<=      ,(lambda (x y) (<= x y)))
    (>=      ,(lambda (x y) (>= x y)))
    (<       ,(lambda (x y) (< x y)))
    (>       ,(lambda (x y) (> x y)))
    (=       ,(lambda (x y) (eqv? x y)))
    (<>      ,(lambda (x y) (not (equal? x y))))
    (log     ,(lambda(x)(log (if (equal? x 0) 0.0 x))))
    (log10   ,(lambda (x) (/ (log x) (log 10.0))))
    (abs     ,abs) 
    (ceil    ,ceiling) 
    (floor   ,floor) 
    (sin     ,sin)  
    (cos     ,cos) 
    (tan     ,tan)
    (asin    ,asin) 
    (acos    ,acos)
    (atan    ,(lambda(x)(atan (if (equal? x 0) 0.0 x)))) 
    (round   ,round)
    (dim     ,s_dim)
    (let     ,s_let)
    (goto    ,s_goto)
    (if      ,(void))
    (print   ,s_print)
    (input   ,s_input)
    )
)

;;********************HELPER FUNCTIONS P2****************************
;;Length of list - Used in execProg
(define length (lambda (length_line)
    (define length_two (lambda (length_line_two n)
              (if (null? length_line_two)n
          (length_two (cdr length_line_two) (+ n 1))
          )
              )
        )        
      (length_two length_line 0)
     )
)

;;Length of line - Used in detFunction 
(define (value length_line)
  (if (pair? length_line)
    (apply (symbol-function_get (car length_line)) 
     (map value (cdr length_line)))
    (if (number? length_line) length_line
      (symbol-variable_get length_line)
      )         
    )
)

;;Called from execProg
(define (detFunction instruction program line_num)
  (if (null? instruction)
    (execProg program (+ line_num 1))
    (begin
      (when (not (hash-has-key? *function-table* (car instruction )))
  (display (car instruction))(display " is not valid")(newline)
  (usage-exit))
      (if (eqv? (car instruction) 'goto)
  (execProg program (- (symbol-label_get (cadr instruction)) 1))
  (if (eqv? (car instruction) 'if)
    (if (equal? #t (value (cadr instruction)))
      (execProg program (- (symbol-label_get (caddr instruction)) 1))
      (execProg program (+ line_num 1))
      )
    (begin
      ((symbol-function_get (car instruction)) (cdr instruction))
      (execProg program (+ line_num 1))
      )
    )
  )
)))

;;Filling out the Label Table
(define (findLabels program)
  (when (not (null? program))
    (let ((firstLineElement (caar program)))
      (when (number? firstLineElement)
  (if (not (null? (cdar program)))
    (if(not (symbol? (cadar program)))
      (void)
      (symbol-label_put! (cadar program) (caar program))
      )
     (void)
    )
  )
      )
    (findLabels (cdr program))
    )
)

;;Executing our SBIR File stored in program, line by line
(define (execProg program line_num)
  (when (> (length program) line_num)
    (let ((line (list-ref program line_num)))
      (if (= (length line) 3)
  (begin
    (set! line (cddr line))
    (detFunction (car line) program line_num)
    )
  (begin
    (if(and (= (length line) 2) (list? (cadr line)))
       (begin
        (set! line (cdr line))
        (detFunction (car line) program line_num)
        )
      (execProg program (+ line_num 1))
     )
    )
   )
      )
    ) 
)

;;Essentially our Main function
(define (main arglist)
  (if (or (null? arglist) (not (null? (cdr arglist))))
    (usage-exit)
    (let* ((sbprogfile (car arglist))
     (program (readlist-from-inputfile sbprogfile)))
      (findLabels program)
      (execProg program 0)
      )
    )
)

;;command line arguments being loaded into vector->list
(main (vector->list (current-command-line-arguments))
)
