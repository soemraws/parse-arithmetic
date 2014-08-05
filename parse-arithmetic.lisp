;;;; parse-arithmetic.lisp

(in-package :parse-arithmetic)

;; This library parses arithmetic expressions in the form people are
;; used to (infix notation), such as: "1+2*3", for use in CL.  This
;; library is not meant to allow CL to use infix notation, but rather
;; as a library for a program where a user may enter expressions,
;; functions and variables, such as in a console calculator program.
;;
;; The library is extremely simple, but at least allows for parsing of
;; arithmetic expressions and definition and use of functions and
;; variables, and adheres to operator precedence and associativity
;; rules.


;;; Expressions (numbers, variables, functions)


;; Number: An expression can just be a numerical value.

(defvar *arithmetic-float-format* *read-default-float-format*
  "The floating point type in which numbers are read from strings.")

(defun expression-number-p (expression)
  "expression-number-p expression => boolean

Test if an expression is a number."
  (numberp expression))


;; Variable: An expression can be a variable, whose name is uniquely
;; defined by a string.  The string must begin with a letter, and can
;; contain any mix of alphanumeric characters after.

(defun expression-variable-p (expression)
  "expression-variable-p expression => boolean

Test if an expression represents a variable."
  (and (listp expression)
       (eq (car expression) :variable)))

(defun expression-variable-name (expression)
  "expression-variable-name expression => string

Returns the name of the variable represented by the given expression.
The name is returned regardless of whether the variable has actually
been defined."
  (when (expression-variable-p expression)
    (cadr expression)))

(defvar *arithmetic-variables* (list (cons "pi" pi)
				  (cons "π" pi)
				  (cons "e" (exp 1.0d0)))
  "Defined arithmetic variables.")

(defun arithmetic-variable (name)
  "arithmetic-variable string => number

Returns the value of the variable with the given name."
  (or (cdr (assoc name *arithmetic-variables* :test #'string=))
      (error "Undefined variable, ~A~%" name)))
 
(defun (setf arithmetic-variable) (value name)
  "(setf (arithmetic-variable string) number)

Set the value of the variable defined with the given name.  If the
variable did not exist, it is created."
  (assert (numberp value))
  (if (assoc name *arithmetic-variables* :test #'string=)
      (setf (cdr (assoc name *arithmetic-variables* :test #'string=)) value)
      (progn
	(setf *arithmetic-variables* (acons name value *arithmetic-variables*))
	value)))

(defmacro with-arithmetic-variable ((name value) &body body)
  "with-arithmetic-variable (name value) form* => results

Evaluates the given forms while a variable with the given name string
has the given value for parsing of arithmetic."
  `(let ((*arithmetic-variables* *arithmetic-variables*))
     (setf (arithmetic-variable ,name) ,value)
     ,@body))


;; Function: An expression can be a call to a function, whose name is
;; uniquely defined by a string.  The string must begin with a letter,
;; and can contain any mix of alphanumeric characters after.  The
;; arguments of the function are expressions themselves.  Note that
;; infix operators are also represented as functions, that operate on
;; two arguments.  E.g. the string expression "1+a" will be
;; represented as an expression calling the function named "+" on the
;; number expression 1 and variable expression a.

(defun expression-function-p (expression)
  "expression-function-p expression => boolean

Test if an expression represents a call to a function."
  (and (listp expression)
       (eq (car expression) :function)))

(defun expression-function-name (expression)
  "expression-function-name expression => string

Returns the name of the function represented by the given expression.
The name is returned regardless of whether the function has actually
been defined."
  (when (expression-function-p expression)
    (cadr expression)))

(defun expression-function-arguments (expression)
  "expression-function-arguments expression => list

Returns the arguments of the function call represented by the given
expression.  Each entry in the argument list is an expression."
  (when (expression-function-p expression)
    (cddr expression)))

(defvar *arithmetic-angle-unit* :radians
  "The angle unit that trigonometric functions work on, or return.")

(defmacro trigonometric-function (function-radians)
  "trigonometric-function function-1 => function-2

For the trigonometric function FUNCTION-1 that normally takes an angle
in radians as its only argument, the returned FUNCTION-2 is a function
that, when called, returns an angle in the unit set by
*ARITHMETIC-ANGLE-UNIT*."
  (let ((arg (gensym "ARGUMENT")))
    `#'(lambda (,arg)
	 (ecase *arithmetic-angle-unit*
	   ((:radians :rad) (,function-radians ,arg))
	   ((:degrees :deg) (,function-radians (* 1/180 pi ,arg)))
	   ((:turns :cycles :revolutions) (,function-radians (* 2 pi ,arg)))
	   ((:grads :grades :gradians :grad) (,function-radians (* 1/200 pi ,arg)))
	   (:quadrants (,function-radians (* 1/2 pi ,arg)))))))

(defmacro inverse-trigonometric-function (function-radians)
  "inverse-trigonometric-function function-1 => function-2

For the trigonometric function FUNCTION-1 that normally returns an
angle in radians, the returned FUNCTION-2 is a function that, when
called, returns an angle in the unit set by *ARITHMETIC-ANGLE-UNIT*."
  (let ((args (gensym "ARGUMENT")))
    `#'(lambda (&rest ,args)
	 (ecase *arithmetic-angle-unit*
	   ((:radians :rad) (apply ,function-radians ,args))
	   ((:degrees :deg) (/ (* 180 (apply ,function-radians ,args)) pi))
	   ((:turns :cycles :revolutions) (/ (apply ,function-radians ,args) (* 2 pi)))
	   ((:grads :grades :gradians :grad) (/ (* 200 (apply ,function-radians ,args)) pi))
	   (:quadrants (/ (* 2 (apply ,function-radians ,args)) pi))))))


(defvar *arithmetic-functions* (list
				(cons "+" #'(lambda (a b) (+ a b)))
				(cons "-" #'(lambda (a b) (- a b)))
				(cons "*" #'(lambda (a b) (* a b)))
				(cons "/" #'(lambda (a b) (/ a b)))
				(cons "^" #'(lambda (a b) (expt a b)))
				(cons "**" #'(lambda (a b) (expt a b)))
				(cons "√" #'(lambda (a b) (* a (sqrt b))))
				(cons "sin" (trigonometric-function sin))
				(cons "cos" (trigonometric-function cos))
				(cons "tan" (trigonometric-function tan))
				(cons "asin" (inverse-trigonometric-function asin))
				(cons "acos" (inverse-trigonometric-function acos))
				(cons "atan" (inverse-trigonometric-function atan))
				(cons "atan2" (inverse-trigonometric-function atan))
				(cons "sinh" #'sinh)
				(cons "cosh" #'cosh)
				(cons "tanh" #'tanh)
				(cons "asinh" #'asinh)
				(cons "acosh" #'acosh)
				(cons "atanh" #'atanh)
				(cons "sqrt" #'sqrt)
				(cons "exp" #'exp)
				(cons "log" #'log)
				(cons "log10" #'(lambda (x) (log x 10)))
				(cons "heaviside" #'(lambda (x &optional (zero-value 1/2))
						      (float (if (minusp x)
								 0
								 (if (plusp x)
								     1
								     zero-value)) x))))
  "Defined arithmetic functions, including operators.")

(defun arithmetic-function (name)
  "arithmetic-function string => function

Returns a function that represents the function defined with the given
name.  The returned object is a lambda, and can thus be funcall'ed."
  (or (cdr (assoc name *arithmetic-functions* :test #'string=))
      (error "Undefined function, ~A~%" name)))

(defun (setf arithmetic-function) (function name)
  "(setf (arithmetic-function string) function)

Define the given arithmetic function under the given name.  The lambda
that is provided becomes accessible when evaluating arithmetic
expressions.  If the function name already existed, it is redefined."
  (if (assoc name *arithmetic-functions* :test #'string=)
      (setf (cdr (assoc name *arithmetic-functions* :test #'string=)) function)
      (setf *arithmetic-functions* (acons name function *arithmetic-functions*)))
  function)

(defmacro with-arithmetic-function ((name function) &body body)
  "with-arithmetic-function (name function) form* => results

Evaluates the given forms while the given function is declared under
the given name string for parsing of arithmetic."
  `(let ((*arithmetic-functions* *arithmetic-functions*))
     (setf (arithmetic-function ,name) ,function)
     ,@body))

;;; Operators

;; Infix operators are functions of two variables.  The operator is
;; represented by a string, and an arithmetic function by that name
;; must be defined in *ARITHMETIC-FUNCTIONS*.  Internally, there is no
;; difference between an operator and a function of two variables.
;; However, in the string expression, the operator is clearly of type
;; infix, meaning that the notation is argument-1 operator argument-2.
;; An example is the √ operator. This means that the string "A√B" is
;; basically the operation A * sqrt(B).  Since it is defined as an
;; infix operator, √A by itself has no meaning, and should be written
;; as 1√A.  Instead, use the "sqrt" function.
;;
;; Note that √ as an operator was just added as an experiment.


(defvar *default-operator-precedence* '(("-" "+") ("/" "*") ("^" "**" "√"))
  "List of the operator classes according to operator precedence.
   Each class is a list of operators with the same precedence.")

(defun operator-string-p (string)
  "operator-string-p string => generalized-boolean

Test if a string represents an operator and return a generalized
boolean.  If the string represents an operator, the level of its
precedence is returned as an integer (0 means lowest precedence)."
  (loop for class in *default-operator-precedence*
     for precedence from 0

     when (operator-in-class string class)
     do (return precedence)))

(defun operator-in-class (operator-string class)
  "operator-in-class operator-string class => generalized-boolean

Test if an operator is in a given class."
  (member operator-string class :test #'string=))

;; List of right-associative operators.  Others are assumed to be
;; left-associative.
(defvar *default-right-associative-operators* '("^" "**" "√")
  "List of operators that are right-associative.")

(defun right-associative-p (operator)
  "right-associative-p operator-string => boolean

Predicate to test if the operator character is right-associative."
  (member operator *default-right-associative-operators* :test #'string=))

(defun operator< (operator-1 operator-2)
  "operator< operator-1 operator-2 => generalized-boolean

Comparison if operator-2 has a higher precedence than operator-1.
When operator-1 and operator-2 have the same precedence, and
operator-1 is right-associative, this also returns true."
  (cond
    ((null operator-1) t)
    ((null operator-2) nil)
    #+nil ((string= operator-1 operator-2)
     (right-associative-p operator-1))
    (t (loop for class in *default-operator-precedence*
	    
	 if (operator-in-class operator-2 class)
	 do (return (and (operator-in-class operator-1 class) (right-associative-p operator-1)))
	 else when (operator-in-class operator-1 class)
	 do (return t)))))

(defun read-operator (string &key (start 0) end sub-expression function-argument)
  "read-operator string &key (start 0) end sub-expression function-argument => operator-string, position

Read the operator from the given string expression, delimited by start
and end and return it, as well as the position in the string
expression where the operator ends.  Returns NIL when the end of the
string expression is reached.  When sub-expression or
function-argument is true, also returns NIL when a closing parenthesis
is reached.  When function-argument is true, also returns NIL when a
comma is reached."
  (unless end
    (setf end (length string)))
  (let ((position (parse-float::skip-whitespaces string :start start :end end)))
    (if (< position end)
      (let ((character (char string position)))
	(if (or (and (or sub-expression function-argument)
		     (char= character #\)))
		(and function-argument
		     (char= character #\,)))
	    (values nil (+ position 1))
	    (loop with operator = (make-array 0
					      :element-type 'character
					      :fill-pointer 0
					      :adjustable t)
	       for pos from position below end
	       do (progn
		    (vector-push-extend (char string pos) operator)
		    (unless (operator-string-p operator)
		      (vector-pop operator)
		      (decf pos)
		      (loop-finish)))
		 
	       finally
		 (return (values (if (zerop (length operator)) nil operator) (+ pos 1))))))
      (values nil end))))


;;; Components

;; These are parts of a string expression.  This can be a number, a
;; variable or a function call.

(defun parse-function-arguments (string &key (start 0) end)
  "parse-function-arguments string &key (start 0) end => arguments, position

This function assumes we are in the argument list of a function call,
e.g. 'f(x,y)' just after the opening parenthesis.  Returns a list of
expressions, each of which represents an argument.  Also returns the
position inside the string where parsing ended."
  (unless end
    (setf end (length string)))
  (loop with position = start
     collect (multiple-value-bind (expr pos)
		 (parse-expression string :start position :end end :function-argument t)
	       (setf position pos)
	       expr) into arguments
     until (char= (char string (- position 1)) #\))

     finally (return (values arguments position))))

(defun parse-function-or-variable (string &key (start 0) end)
  "parse-function-or-variable string &key (start 0) end => expression, position

Parses a function or variable expression from the given string.  The
difference between a function and a variable is, that immediately
after a function name, there is an open parenthesis.  Returns the
parsed expression and the position where parsing ended."
  (unless end
    (setf end (length string)))
  (loop for position from start below end
     for character = (char string position)
       
     while (alphanumericp character)

     finally
       (return
	 (if (char= #\( character)
	     (multiple-value-bind (args pos)
		 (parse-function-arguments string :start (+ position 1) :end end)
	       (values (list* :function (subseq string start position) args) pos))
	     (values (list :variable (subseq string start position)) position)))))

(defun parse-component (string &key (start 0) end)
  "parse-component string &key (start 0) end => expression, position

Parses a component of an expression, i.e. the thing before or after an
operator.  The component is returned as an expression.  Also returns
the position where parsing ended."
  (unless end
    (setf end (length string)))
  (let ((position (parse-float::skip-whitespaces string :start start :end end)))
    (when (< position end)
      (let ((character (char string position)))
	(cond
	  ((char= character #\()
	   (parse-expression string :start (+ position 1) :end end :sub-expression t))
	  ((or (digit-char-p character) (char= character #\.))
	   (parse-float string :start start :end end :junk-allowed t :type *arithmetic-float-format*))
	  ((alpha-char-p character)
	   (parse-function-or-variable string :start position :end end))
	  (t (error "Expected component.")))))))


;;; Parsing of an expression

(defun determine-precedence (components operators)
  "determine-precedence components-stack operators-stack => expression

This function is only used by parse-expression.  Takes two stacks, one
for the components and one for the operators, sorts the whole bunch
according to precedence, and returns the resulting expression."
  (labels ((determine-precedence* (component-stack operator-stack)
	     (loop while operator-stack
		  
		if (operator< (cadr operator-stack) (car operator-stack))
		do (setf component-stack (cons (list :function
						     (car operator-stack)
						     (cadr component-stack)
						     (car component-stack))
					       (cddr component-stack))
			 operator-stack (cdr operator-stack))
		else
		do (multiple-value-bind (c-stack o-stack)
		       (determine-precedence* (cdr component-stack)
					      (cdr operator-stack))
		     (setf component-stack (cons (car component-stack) c-stack)
			   operator-stack  (cons (car operator-stack) o-stack)))
		  
		finally
		  (return (values component-stack operator-stack)))))
    (car (determine-precedence* components operators))))

(defun parse-expression (string &key (start 0) end sub-expression function-argument)
  "parse-expression string &key (start 0) end sub-expression function-argument => expression, position

Parse the string expression, with bounds start and end, and return it,
and the position where parsing ended.  If sub-expression is true, it
is assumed that string starts inside parentheses, and parsing will end
when the closing parenthesis is found.  If function-argument is true,
it is assumed that string starts inside the argument list of a
function, and parsing will end when either a comma or a closing
parenthesis is reached."
  (unless end
    (setf end (length string)))
  (loop with components = nil
     with operators = nil
     with position = (parse-float::skip-whitespaces string :start start :end end)

     while (< position end)
       
     do (if (char= (char string position) #\-)
	    (setf components (cons -1 components)
		  operators (cons "*" operators)
		  position (+ position 1))
	    (progn
	      (multiple-value-bind (component pos)
		  (parse-component string :start position :end end)
		(setf position pos)
		(if component
		    (setf components (cons component components))
		    (error "Expected component")))
	      (multiple-value-bind (operator pos)
		  (read-operator string :start position :end end :sub-expression sub-expression :function-argument function-argument)
		(setf position pos)
		(if operator
		    (setf operators (cons operator operators))
		    (if (or (= pos end) sub-expression function-argument)
			(loop-finish)
			(error "Expected operator"))))))

     finally
       (return (values (determine-precedence components operators) position))))


(defun evaluate-expression (expression)
  "evaluate-expression expression => number

Evaluate an expression and return the result."
  (cond
    ((expression-number-p expression)
     expression)
    ((expression-function-p expression)
     (let ((func (arithmetic-function (expression-function-name expression)))
	   (arguments (expression-function-arguments expression)))
       (apply func (mapcar #'evaluate-expression arguments))))
    ((expression-variable-p expression)
     (arithmetic-variable (expression-variable-name expression)))
    (t (error "Unable to evaluate expression"))))


(defun compile-expression (expression &optional argument-names)
  "compile-expression expression &optional arguments => function

Takes an expression and, optionally, a list of variable names
representing the arguments, and returns a lambda.  If no list of
variables is provided, the returned lambda takes no arguments.  If
there is a list of variable names, the expression is modified such
that the variables with those names correspond to the argument list
of the returned lambda."
  (labels ((replace-arguments (expression)
	     (cond
	       ((numberp expression)
		expression)
	       ((expression-variable-p expression)
		(let ((pos (position (cadr expression) argument-names :test #'string=)))
		  (if pos
		      (list :function ":ARGUMENT" pos)
		      expression)))
	       ((expression-function-p expression)
		(list* :function (expression-function-name expression)
		       (mapcar #'replace-arguments (expression-function-arguments expression)))))))
    (let (func)
      (if argument-names
	  (let ((expression (replace-arguments expression)))
	    (setf func #'(lambda (&rest args)
			   (with-arithmetic-function (":ARGUMENT" #'(lambda (i) (nth i args)))
			     (evaluate-expression expression)))))
	  (setf func #'(lambda (&rest args) (declare (ignore args)) (evaluate-expression expression))))
      func)))

(defun parse-assignment (string &key (start 0) end)
  "parse-assignment string &key (start 0) end => object-expression, definition-expression

Parse an assignment string, i.e. a string that contains an assignment
character #\=, and return the expressions to the left and right of the
assignment character."
  (unless end
    (setf end (length string)))
  (let ((assignment-position (position #\= string :test #'char=)))
    (when assignment-position
      (values (parse-function-or-variable string :start start :end assignment-position)
	      (parse-expression string :start (+ assignment-position 1) :end end)))))

(defun evaluate-assignment (expression definition-expression)
  "evaluate-assignment object-expression definition-expression => result-object

Evaluates an assignment.  The object-expression must be either a
variable expression, or a function expression with all arguments
variable expressions.  If object-expression represents a variable, the
definition-expression must evaluate to a number, which is then
returned.  If object-expression is a function, the
definition-expression is compiled into a lambda and returned."
  (cond
    ((expression-variable-p expression)
     (let ((value (evaluate-expression definition-expression)))
       (if (numberp value)
	   (setf (arithmetic-variable (expression-variable-name expression)) value)
	   (error "Variable definition must be a numerical value."))))
    ((expression-function-p expression)
     (let ((args (expression-function-arguments expression)))
       (if (loop for arg in args
	      always (expression-variable-p arg))
	   (setf (arithmetic-function (expression-function-name expression))
		 (compile-expression definition-expression (mapcar #'expression-variable-name args)))
	   (error "Function arguments must be variables in assignment."))))
    (t (error "Unrecognized assignment statement."))))


(defun parse-and-evaluate (string &key (start 0) end)
  "parse-and-evaluate string &key (start 0) end => result-object

Parses and evaluates the given string expression.  If it contains an
assignment, the assignment is evaluated and the result is the assigned
value.  If it contains an expression, the result of its evaluation is
returned."
  (or (multiple-value-bind (object definition)
	  (parse-assignment string :start start :end end)
	(when object
	  (evaluate-assignment object definition)))
      (evaluate-expression (parse-expression string :start start :end end))))


;; This is only for the calculator
(defun set-option (string &key (start 0) end)
  (unless end
    (setf end (length string)))
  (let ((position (parse-float::skip-whitespaces string :start start :end end)))
    (when (and (< 4 (length string)) (string= (subseq string position (+ position 4)) "set "))
      (let* ((start (parse-float::skip-whitespaces string :start (+ position 4) :end end))
	     (end (parse-float::skip-whitespaces string :start start :end end :from-end t))
	     (option (subseq string start end)))
	(cond
	  ((string= option "radians") (setf *arithmetic-angle-unit* :radians))
	  ((string= option "degrees") (setf *arithmetic-angle-unit* :degrees))
	  ((string= option "gradians") (setf *arithmetic-angle-unit* :gradians))
	  ((string= option "single") (setf *arithmetic-float-format* 'single-float))
	  ((string= option "double") (setf *arithmetic-float-format* 'double-float))
	  (t (error "Unknown option ~A" option)))))))



(defun start-calculator (&optional (stream *standard-input*))
  (loop for string = (read-line stream nil nil)
     until (string= "quit" string)

     do (or (set-option string)
	    (multiple-value-bind (object definition)
		(parse-assignment string)
	      (when object
		(evaluate-assignment object definition)))
	    (format t "~A~%" (evaluate-expression (parse-expression string))))))

