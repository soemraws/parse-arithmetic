;;;; package.lisp

(defpackage #:parse-arithmetic
  (:use #:cl #:parse-float)
  (:export #:*arithmetic-float-format*
	   #:*arithmetic-angle-unit*
	   #:arithmetic-variable
	   #:arithmetic-function
	   
	   #:with-arithmetic-variable
	   #:with-arithmetic-function

	   #:parse-expression
	   #:evaluate-expression
	   #:compile-expression

	   #:parse-assignment
	   #:evaluate-assignment

	   #:parse-and-evaluate

	   #:start-calculator))
