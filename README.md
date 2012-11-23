parse-arithmetic
================

Parse-arithmetic is a library for parsing user-provided infix
arithmetic expressions, allowing a CL program to use that input in a
meaningful way.

Introduction
------------

The aim of this library is __not__ to change CL's syntax to use infix
operators, which is obviously a wrong and evil thing to do.  Rather,
the use case is for a program that requires the user to input an
arithmetic expression, which the program can use for further
calculations.  A good example would be a console calculator, where the
user will input the expression (a string) the way he or she is used to
from many other programs: with infix notation and without our beloved
parentheses.

Functions defined by the user in string expression form can be
funcall'ed, and variables can be accessed.

This was hacked together in a few hours, because I wanted to try my
hand at parsing user input to interact with a CL program, and without
using eval.  The result is quite rudimentary, not thoroughly tested,
and not optimized in any way, and the documentation isn't really
clear.

Examples
--------

We can evaluate numerical expressions, with proper operator precedence and associativity:

    (parse-and-evaluate "2+3*4") => 14.0

    (let ((*arithmetic-float-format* 'double-float))
      (parse-and-evaluate "2^3^2")) => 512.0d0

We can also use pre-defined functions:

    (parse-and-evaluate "cos(pi/4)") => 0.7071067811865476d0

    (let ((*arithmetic-angle-unit* :degrees))
      (parse-and-evaluate "cos(45)")) => 0.7071067811865476d0

Assign and access variables:

    (parse-and-evaluate "a=1") => 1.0

    (parse-and-evaluate "a+1") => 2.0

    ; access the variable in CL
    (+ (arithmetic-variable "a") 1) => 2.0

    ; set the variable in CL
    (setf (arithmetic-variable "a") 2.0) => 2.0

    (parse-and-evaluate "a*sin(pi/4)/sqrt(2)") => 1.0000000171142713d0

Assign and access functions:
    
    (parse-and-evaluate "f(x)=x^2+a") => #<CLOSURE (LAMBDA (&REST PARSE-ARITHMETIC::ARGS) :IN COMPILE-EXPRESSION) {1004F758BB}>

    (parse-and-evaluate "f(3)") => 11.0

    ; funcall the result in CL
    (funcall ** 3) => 11.0

    ; or find the function by name and funcall it
    (funcall (arithmetic-function "f") 3) => 11.0

CL Functions and variables
--------------------------

### Special variables

**\*ARITHMETIC-FLOAT-FORMAT\***  
The float type into which all numbers are parsed.  Default value is
**\*DEFAULT-READ-FLOAT-FORMAT\***.

**\*ARITHMETIC-ANGLE-UNIT\***  
The unit of angle which is used by trigonometric functions.  Possible
values are:

* _:radians_ or _:rad_ for radians (the default value),
* _:degrees_ or _:deg_ for degrees,
* _:gradians_, _:grades_, _:grads_ or _:grad_ for gradians,
* _:turns_, _:cycles_ or _:revolutions_ for turns,
* _:quadrants_ for quadrants.

### Number expressions

**expression-number-p** _expression_ => _boolean_  
Test if an expression object represents a number.  In fact, the object
is a number.

    (parse-expression "2.2") => 2.2

### Variable expressions

**expression-variable-p** _expression_ => _boolean_  
Test if an expression object represents a variable.  A variable
expression is a list two items: the keyword symbol _:variable_, and a
string that is the name of the variable.

    (parse-expression "x") => (:variable "x")

**expression-variable-name** _expression_ => _string_  
Returns the name of the variable described by _expression_, or **nil**
if _expression_ did not represent a variable.

**arithmetic-variable** _name_ => _value_  
(setf (**arithmetic-variable** _name_) _new-value_)  
Return or set the value of the variable with the given _name_.
When setf'ing, if the variable _name_ didn't exist, it is created.

### Function expressions

**expression-function-p** _expression_ => _boolean_  
Test if an expression object represents a call to a function.  A
function expression is a list multiple items: the keyword symbol
_:function_ and a string that is the name of the function, followed by
the function's arguments, each of which is an expression in itself.

    (parse-expression "f(x,3)") => (:function "f" (:variable "x") 3)

**expression-function-name** _expression_ => _string_  
Returns the name of the function described by _expression_, or **nil**
if _expression_ did not represent a function.

**expression-function-arguments** _expression_ => _expressions_  
Returns a list of expressions, each representing an argument to the
function described by _expression_, or **nil** if _expression_ did not
describe a function.

**arithmetic-function** _name_ => _lambda_  
(setf (**arithmetic-function** _name_) _new-lambda_)  
Return or set the lambda for an arithmetic function with the given _name_.
When setf'ing, if the function _name_ didn't exist, it is created.

### Parsing and evaluation

**parse-expression** _string_ &key _start_ _end_ _sub-expression_ _function-argument_ => _expression_, _pos_  
Parse a string expression into an expression object (as described
above), and return that.  Also returns the bounding index where
parsing stopped.  If _sub-expression_ is true, parsing is assumed to
start just inside a sub-expression, i.e. after an opening parenthesis
in _string_, and parsing will end when a closing parenthesis is
encountered.  If _function-argument_ is true, parsing is assumed to
start just inside a function's argument, and parsing will end when
either a comma or a closing parenthesis is encountered.

**evaluate-expression** _expression_ => _number_  
Evaluate _expression_, which results in a _number_.  If not all
functions and/or variables are defined, and thus _expression_ contains
unknowns, this causes an error.

**compile-expression** _expression_ &optional _argument-names_ => _lambda_  
Compile an expression into a lambda that can be funcalled.  This is
normally not needed.

**parse-assignment** _string_ &key _start_ _end_ => _object-expression_, _definition-expression_  
Parse an assignment in _string_, bounded by indices _start_ and _end_.
Returns the expression _object-expression_ before the assignment character
\#\=, and the expression _definition-expression_ after it.

**evaluate-assignment** _object-expression_ _definition-expression_ => _object_  
Evaluate an assignment, by assigning the result of _definition-expression_ to
_object-expression_.  _object-expression_ must either be a variable expression,
or a function expression of which all arguments are variable expressions.  If
a variable, _definition-expression_ must evaluate to a number, which is then
returned as _object_.  If a function, _definition-expression_ will be compiled
to a lambda, which is returned as _object_.

**parse-and-evaluate** _string_ &key _start_ _end_ => _object_  
Convenience function that parses and evaluates expressions and
assignments.

Authors
-------

Sumant S. R. Oemrawsingh
