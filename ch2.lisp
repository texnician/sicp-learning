;; Load chapter 1
(load "ch1-1-6")

(in-package :sicp)

;; *Exercise 2.1:* Define a better version of `make-rat' that handles both
;; positive and negative arguments.  `Make-rat' should normalize the sign so
;; that if the rational number is positive, both the numerator and denominator
;; are positive, and if the rational number is negative, only the numerator is
;; negative.
(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defun equal-ratp (x y)
  (= (* (number x) (denom y))
     (* (number y) (denom x))))

(defun make-rat (x y)
  (let ((d (gcd1 x y))
        (abs-x (abs x))
        (abs-y (abs y)))
      (cons (/ (* (/ x abs-x) (/ y abs-y) abs-x) d)
            (/ abs-y d))))

(defun numer (x)
  (car x))

(defun denom (x)
  (cdr x))

;; (make-rat 40 206)

;; (make-rat 40 -206)
;; (make-rat -40 206)
;; (make-rat -40 -206)

;; *Exercise 2.2:* Consider the problem of representing line segments in a
;; plane.  Each segment is represented as a pair of points: a starting point and
;; an ending point.  Define a constructor `make-segment' and selectors
;; `start-segment' and `end-segment' that define the representation of segments
;; in terms of points.  Furthermore, a point can be represented as a pair of
;; numbers: the x coordinate and the y coordinate.  Accordingly, specify a
;; constructor `make-point' and selectors `x-point' and `y-point' that define
;; this representation.  Finally, using your selectors and constructors, define
;; a procedure `midpoint-segment' that takes a line segment as argument and
;; returns its midpoint (the point whose coordinates are the average of the
;; coordinates of the endpoints).  To try your procedures, you'll need a way to
;; print points:

;;       (define (print-point p)
;;        (newline)
;;        (display "(")
;;        (display (x-point p))
;;        (display ",")
;;        (display (y-point p))
;;        (display ")"))
(defun make-segment (start end)
  (cons start end))

(defun start-segment (seg)
  (car seg))

(defun end-segment (seg)
  (cdr seg))

(defun make-point (x y)
  (cons x y))

(defun x-point (p)
  (car p))

(defun y-point (p)
  (cdr p))

(defun midpoint-segment (seg)
  (let ((pa (start-segment seg))
        (pb (end-segment seg)))
    (make-point (average (x-point pa) (x-point pb))
                (average (y-point pa) (y-point pb)))))

(defun print-point (p)
  (fresh-line)
  (format t "(~d, ~d)" (x-point p) (y-point p)))

;; (print-point (midpoint-segment (make-segment (make-point 1 -1)
;;                                              (make-point -1 1))))

;; *Exercise 2.3:* Implement a representation for rectangles in a plane.  (Hint:
;; You may want to make use of *note Exercise 2-2::.)  In terms of your
;; constructors and selectors, create procedures that compute the perimeter and
;; the area of a given rectangle.  Now implement a different representation for
;; rectangles.  Can you design your system with suitable abstraction barriers,
;; so that the same perimeter and area procedures will work using either
;; representation?
(defun make-rectangle (bottom-edge height)
  (cons bottom-edge height))

(defun point-distance (a b)
  (sqrt (sum-of-square (- (x-point a) (x-point b))
                       (- (y-point a) (y-point b)))))

(defun width-rec (rec)
  (let ((width-edge (car rec)))
    (point-distance (start-segment width-edge)
                    (end-segment width-edge))))

(defun height-rec (rec)
  (cdr rec))

(defun perimeter-rec (rec)
  (* 2 (+ (width-rec rec) (height-rec rec))))

(defun area-rec (rec)
  (* (width-rec rec) (height-rec rec)))

;; (perimeter-rec (make-rectangle (make-segment (make-point 0 0)
;;                                              (make-point 2 (* 2 (sqrt 3d0))))
;;                                3))

;; *Exercise 2.4:* Here is an alternative procedural representation of pairs.
;; For this representation, verify that `(car (cons x y))' yields `x' for any
;; objects `x' and `y'.

;;      (define (cons x y)
;;        (lambda (m) (m x y)))

;;      (define (car z)
;;        (z (lambda (p q) p)))

;; What is the corresponding definition of `cdr'? (Hint: To verify that this
;; works, make use of the substitution model of section *note 1-1-5::.)

(defun fcons (x y)
  (lambda (m) (funcall m x y)))

(defun fcar (z)
  (funcall z #'(lambda (p q) p)))

(defun fcdr (z)
  (funcall z #'(lambda (p q) q)))

;; (fcar (fcons 1 2))
;; (fcdr (fcons 1 2))

;; *Exercise 2.5:* Show that we can represent pairs of nonnegative integers
;; using only numbers and arithmetic operations if we represent the pair a and b
;; as the integer that is the product 2^a 3^b.  Give the corresponding
;; definitions of the procedures `cons', `car', and `cdr'.

(defun ncons (a b)
  (* (expt 2 a) (expt 3 b)))


(defun ncar (c)
  (labels ((iter (x acc)
             (if (> (mod x 2) 0)
                 acc
                 (iter (/ x 2) (1+ acc)))))
    (iter c 0)))

(defun ncdr (c)
  (labels ((iter (x acc)
             (if (> (mod x 3) 0)
                 acc
                 (iter (/ x 3) (1+ acc)))))
    (iter c 0)))

;; (ncar (ncons 7 11))
;; (ncdr (ncons 29 13))

;; *Exercise 2.6:* In case representing pairs as procedures wasn't mind-boggling

;; enough, consider that, in a language that can manipulate procedures, we can
;; get by without numbers (at least insofar as nonnegative integers are
;; concerned) by implementing 0 and the operation of adding 1 as

;;      (define zero (lambda (f) (lambda (x) x)))

;;      (define (add-1 n)
;;        (lambda (f) (lambda (x) (f ((n f) x)))))

;; This representation is known as "Church numerals", after its inventor, Alonzo
;; Church, the logician who invented the [lambda] calculus.

;; Define `one' and `two' directly (not in terms of `zero' and `add-1').  (Hint:
;; Use substitution to evaluate `(add-1 zero)').  Give a direct definition of
;; the addition procedure `+' (not in terms of repeated application of `add-1').
(defparameter *zero* (lambda (f) (lambda (x) x)))

(defun add-1 (n)
  (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))

(defparameter *one* (lambda (f) (lambda (x) (funcall f x))))
(defparameter *two* (lambda (f) (lambda (x) (funcall f (funcall f x)))))
(defparameter *five*
  (lambda (f)
    (lambda (x)
      (funcall f (funcall f (funcall f (funcall f (funcall f x))))))))


(defun fadd (a b)
  (lambda (f) (lambda (x) (funcall (funcall a f) (funcall (funcall b f) x)))))

;; (funcall (funcall *zero* #'1+) 0)
;; (funcall (funcall (add-1 *two*) #'1+) 0)
;; (funcall (funcall *one* #'1+) 0)
;; (funcall (funcall *two* #'1+) 0)
;; (funcall (funcall *five* #'1+) 0)
;; (funcall (funcall (fadd *five* *two*) #'1+) 0)

;; *Exercise 2.7:* Alyssa's program is incomplete because she has not specified
;; the implementation of the interval abstraction.  Here is a definition of the
;; interval constructor:

;;      (define (make-interval a b) (cons a b))

;; Define selectors `upper-bound' and `lower-bound' to complete the
;; implementation.
(defun make-interval (a b) (cons a b))

(defun upper-bound (z) (cdr z))

(defun lower-bound (z) (car z))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x (make-interval (/ 1L0 (upper-bound y))
                                 (/ 1L0 (lower-bound y)))))

;; *Exercise 2.8:* Using reasoning analogous to Alyssa's, describe how the
;; difference of two intervals may be computed.  Define a corresponding
;; subtraction procedure, called `sub-interval'.
(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; (setq inv1 (make-interval 9 11))
;; (setq inv2 (make-interval -1 1))
;; (add-interval inv1 inv2)
;; (mul-interval inv1 inv2)
;; (div-interval inv1 inv2)
;; (sub-interval inv1 inv2)

;; *Exercise 2.9:* The "width" of an interval is half of the difference between
;; its upper and lower bounds.  The width is a measure of the uncertainty of the
;; number specified by the interval.  For some arithmetic operations the width
;; of the result of combining two intervals is a function only of the widths of
;; the argument intervals, whereas for others the width of the combination is
;; not a function of the widths of the argument intervals.  Show that the width
;; of the sum (or difference) of two intervals is a function only of the widths
;; of the intervals being added (or subtracted).  Give examples to show that
;; this is not true for multiplication or division.
(defun width-interval (z)
  (/ (- (upper-bound z) (lower-bound z)) 2))

;; (setq inv1 (make-interval 9 11))
;; (setq inv2 (make-interval 3 11))
;; (setq inv3 (make-interval 14 16))
;; (setq inv4 (make-interval 15 23))
;; (assert (= (width-interval inv1) (width-interval inv3)))
;; (assert (= (width-interval inv2) (width-interval inv4)))
;; (assert (= (width-interval (add-interval inv1 inv2))
;;            (width-interval (add-interval inv3 inv4))))
;; (assert (= (width-interval (sub-interval inv1 inv2))
;;            (width-interval (sub-interval inv3 inv4))))
;; (assert (not (= (width-interval (mul-interval inv1 inv2))
;;                 (width-interval (mul-interval inv3 inv4)))))
;; (assert (not (= (width-interval (div-interval inv1 inv2))
;;                 (width-interval (div-interval inv3 inv4)))))

;; *Exercise 2.10:* Ben Bitdiddle, an expert systems programmer, looks over
;; Alyssa's shoulder and comments that it is not clear what it means to divide
;; by an interval that spans zero.  Modify Alyssa's code to check for this
;; condition and to signal an error if it occurs.
(fmakunbound 'div-interval)

(defun div-interval (x y)
  (assert (> (* (upper-bound y) (lower-bound y)) 0))
  (mul-interval x (make-interval (/ 1L0 (upper-bound y))
                                 (/ 1L0 (lower-bound y)))))

;; *Exercise 2.12:* Define a constructor `make-center-percent' that takes a
;; center and a percentage tolerance and produces the desired interval.  You
;; must also define a selector `percent' that produces the percentage tolerance
;; for a given interval.  The `center' selector is the same as the one shown
;; above.
(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))

(defun center-interval (z)
  (/ (+ (lower-bound z) (upper-bound z)) 2))

(defun width-interval (z)
  (/ (- (upper-bound z) (lower-bound z)) 2))

(defun make-center-percent (c p)
  (make-interval (* c (- 1 (/ p 100)))
                 (* c (+ 1 (/ p 100)))))

(defun percent-interval (z)
  (* 100.0d0 (/ (width-interval z) (abs (center-interval z)))))

;(center-interval (make-center-percent 100 1))
;(percent-interval (make-center-percent 100 1))

;; *Exercise 2.14:* Demonstrate that Lem is right.  Investigate the behavior of
;; the system on a variety of arithmetic expressions.  Make some intervals A and
;; B, and use them in computing the expressions A/A and A/B.  You will get the
;; most insight by using intervals whose width is a small percentage of the
;; center value.  Examine the results of the computation in center-percent form
;; (see *note Exercise 2-12::).
(defun par1 (r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))


;; (setq inv1 (make-center-percent 100 1))
;; (setq inv2 (make-center-percent 150 1))

;; (percent-interval (add-interval inv1 inv1))
;; (percent-interval (sub-interval inv1 inv2))
;; (percent-interval (div-interval inv2 inv2))
;; (center-interval (div-interval inv1 inv2))
;; (percent-interval (div-interval inv1 inv2))
;; (percent-interval (par1 inv1 inv2))
;; (percent-interval (par2 inv1 inv2))
;; (/ 9 23d0)
;; (/ 11 15d0)

;; *Exercise 2.17:* Define a procedure `last-pair' that returns the list that
;; contains only the last element of a given (nonempty) list:

;;      (last-pair (list 23 72 149 34))
;;      (34)
(defun last-pair (l)
  (if (null (cdr l))
      l
      (last-pair (cdr l))))

;; *Exercise 2.18:* Define a procedure `reverse' that takes a list as argument
;; and returns a list of the same elements in reverse order:

;;      (reverse (list 1 4 9 16 25))
;;      (25 16 9 4 1)

(defun sicp-reverse1 (lst)
  (labels ((reverse-iter (acc l)
             (if (null l)
                 acc
                 (reverse-iter (cons (car l) acc) (cdr l)))))
    (reverse-iter nil lst)))

;(sicp-reverse1 (list 1 4 9 16 25))

;; *Exercise 2.19:* Consider the change-counting program of section *1-2-2::.
;; It would be nice to be able to easily change the currency used by the
;; program, so that we could compute the number of ways to change a British
;; pound, for example.  As the program is written, the knowledge of the currency
;; is distributed partly into the procedure `first-denomination' and partly into
;; the procedure `count-change' (which knows that there are five kinds of U.S.
;; coins).  It would be nicer to be able to supply a list of coins to be used
;; for making change.

;; We want to rewrite the procedure `cc' so that its second argument is a list
;; of the values of the coins to use rather than an integer specifying which
;; coins to use.  We could then have lists that defined each kind of currency:

;;      (define us-coins (list 50 25 10 5 1))

;;      (define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; We could then call `cc' as follows:

;;      (cc 100 us-coins)
;;      292

;; To do this will require changing the program `cc' somewhat.  It will still
;; have the same form, but it will access its second argument differently, as
;; follows:

;;      (define (cc amount coin-values)
;;        (cond ((= amount 0) 1)
;;              ((or (< amount 0) (no-more? coin-values)) 0)
;;              (else
;;               (+ (cc amount
;;                      (except-first-denomination coin-values))
;;                  (cc (- amount
;;                         (first-denomination coin-values))
;;                      coin-values)))))

;; Define the procedures `first-denomination', `except-first-denomination', and
;; `no-more?' in terms of primitive operations on list structures.  Does the
;; order of the list `coin-values' affect the answer produced by `cc'?  Why or
;; why not?

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-morep coin-values)) 0)
        (t (+ (cc amount
                  (except-first-denomination coin-values))
              (cc (- amount
                     (first-denomination coin-values))
                  coin-values)))))

(defun first-denomination (coin-values)
  (car coin-values))

(defun except-first-denomination (coin-values)
  (cdr coin-values))

(defun no-morep (coin-values)
  (null coin-values))

(defparameter *us-coins* (list 50 25 10 5 1))
(defparameter *uk-coins* (list 100 50 20 10 5 2 1 0.5))
(defparameter *cn-coins* (list 100 50 20 10 5 2 1))

;(time (cc 100 *cn-coins*))

;; *Exercise 2.20:* The procedures `+', `*', and `list' take arbitrary numbers
;; of arguments. One way to define such procedures is to use `define' with
;; notation "dotted-tail notation".  In a procedure definition, a parameter list
;; that has a dot before the last parameter name indicates that, when the
;; procedure is called, the initial parameters (if any) will have as values the
;; initial arguments, as usual, but the final parameter's value will be a "list"
;; of any remaining arguments.  For instance, given the definition

;;      (define (f x y . z) <BODY>)

;; the procedure `f' can be called with two or more arguments.  If we evaluate

;;      (f 1 2 3 4 5 6)

;; then in the body of `f', `x' will be 1, `y' will be 2, and `z' will be the
;; list `(3 4 5 6)'.  Given the definition

;;      (define (g . w) <BODY>)

;; the procedure `g' can be called with zero or more arguments.  If we evaluate

;;      (g 1 2 3 4 5 6)

;; then in the body of `g', `w' will be the list `(1 2 3 4 5 6)'.(4)

;; Use this notation to write a procedure `same-parity' that takes one or more
;; integers and returns a list of all the arguments that have the same even-odd
;; parity as the first argument.  For example,

;;      (same-parity 1 2 3 4 5 6 7)
;;      (1 3 5 7)

;;      (same-parity 2 3 4 5 6 7)
;;      (2 4 6)
(defun same-parity (first &rest others)
  (labels ((same-parity-iter (acc lst)
             (if (null lst)
                 acc
                 (if (= (mod first 2) (mod (car lst) 2))
                     (same-parity-iter (append acc (list (car lst))) (cdr lst))
                     (same-parity-iter acc (cdr lst))))))
    (cons first (same-parity-iter nil others))))

(defun same-parity-full-recursive (first &rest others)
  (labels ((same-parity-iter (lst)
             (if (null lst)
                 nil
                 (if (= (mod first 2) (mod (car lst) 2))
                     (append (list (car lst)) (same-parity-iter (cdr lst)))
                     (same-parity-iter (cdr lst))))))
    (cons first (same-parity-iter others))))

;; Use generalized filter pattern
(defun same-parity-general (first &rest others)
  (cons first (filtered-accumulate-tail-recursive
               #'(lambda (term acc)
                   (if (null term)
                       acc
                       (append acc (list term))))
               #'(lambda (x)
                   (= (mod first 2) (mod (nth x others) 2)))
               nil
               #'(lambda (x)
                   (nth x others))
               0
               #'1+
               (1- (length others)))))


;(same-parity-general 2 3 4 5 6 7)
;(map 'list #'+ '(1 2 3) '(40 50 60) '(700 800 900))

;; *Exercise 2.21:* The procedure `square-list' takes a list of numbers as
;; argument and returns a list of the squares of those numbers.

;;      (square-list (list 1 2 3 4))
;;      (1 4 9 16)

;; Here are two different definitions of `square-list'.  Complete both of them
;; by filling in the missing expressions:

;;      (define (square-list items)
;;        (if (null? items)
;;            nil
;;            (cons <??> <??>)))

;;      (define (square-list items)
;;        (map <??> <??>))

(defun square-list1 (items)
  (if (null items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))

(defun square-list2 (items)
  (map 'list #'square items))

; (square-list2 (square-list1 '(1 2 3 4)))

;; *Exercise 2.22:* Louis Reasoner tries to rewrite the first `square-list'
;; procedure of *note Exercise 2-21:: so that it evolves an iterative process:

;;      (define (square-list items)
;;        (define (iter things answer)
;;          (if (null? things)
;;              answer
;;              (iter (cdr things)
;;                    (cons (square (car things))
;;                          answer))))
;;        (iter items nil))

;; Unfortunately, defining `square-list' this way produces the answer list in
;; the reverse order of the one desired.  Why?

;; Louis then tries to fix his bug by interchanging the arguments to `cons':

;;      (define (square-list items)
;;        (define (iter things answer)
;;          (if (null? things)
;;              answer
;;              (iter (cdr things)
;;                    (cons answer
;;                          (square (car things))))))
;;        (iter items nil))

;; This doesn't work either.  Explain.
(defun square-list3 (items)
  (labels ((iter (acc lst)
             (if (null lst)
                 acc
                 (iter (append acc (list (square (car lst)))) (cdr lst)))))
    (iter nil items)))

; (square-list3 '(1 2 3 4))

;; *Exercise 2.23:* The procedure `for-each' is similar to `map'.  It takes as
;; arguments a procedure and a list of elements.  However, rather than forming a
;; list of the results, `for-each' just applies the procedure to each of the
;; elements in turn, from left to right.  The values returned by applying the
;; procedure to the elements are not used at all--`for-each' is used with
;; procedures that perform an action, such as printing.  For example,

;;      (for-each (lambda (x) (newline) (display x))
;;                (list 57 321 88))
;;      57
;;      321
;;      88

;; The value returned by the call to `for-each' (not illustrated above) can be
;; something arbitrary, such as true.  Give an implementation of `for-each'.
(defun for-each (f items)
  (labels ((iter (action lst)
             (if (null lst)
                 action
                 (iter (funcall f (car lst)) (cdr lst)))))
    (iter nil items)))

;; (for-each #'(lambda (x) (fresh-line) (prin1 x))
;;           '(57 321 88))

;; *Exercise 2.26:* Suppose we define `x' and `y' to be two lists:

;;      (define x (list 1 2 3))

;;      (define y (list 4 5 6))

;; What result is printed by the interpreter in response to evaluating each of
;; the following expressions:

;;      (append x y)

;;      (cons x y)

;;      (list x y)

(assert (equal (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6)))
(assert (equal (cons '(1 2 3) '(4 5 6)) '((1 2 3) . (4 5 6))))
(assert (equal (list '(1 2 3) '(4 5 6)) '((1 2 3) (4 5 6))))

;; *Exercise 2.27:* Modify your `reverse' procedure of *note Exercise 2-18:: to
;; produce a `deep-reverse' procedure that takes a list as argument and returns
;; as its value the list with its elements reversed and with all sublists
;; deep-reversed as well.  For example,

;;      (define x (list (list 1 2) (list 3 4)))

;;      x
;;      ((1 2) (3 4))

;;      (reverse x)
;;      ((3 4) (1 2))

;;      (deep-reverse x)
;;      ((4 3) (2 1))
(defun deep-reverse (items)
  (if (null items)
      nil
      (append (deep-reverse (cdr items))
              (if (consp (car items))
                  (list (deep-reverse (car items)))
                  (list (car items))))))

(defun deep-reverse-tail-recursive (items)
  (labels ((iter (acc lst)
             (if (null lst)
                 acc
                 (iter (cons (if (consp (car lst))
                                 (iter nil (car lst))
                                 (car lst)) acc)
                       (cdr lst)))))
    (iter nil items)))
                                  
; (deep-reverse '((1 2) 3 4 (5 (6 (7)) 8)))
; (deep-reverse-tail-recursive '((1 2) 3 4 (5 (6 (7)) 8)))

;; *Exercise 2.28:* Write a procedure `fringe' that takes as argument a tree
;; (represented as a list) and returns a list whose elements are all the leaves
;; of the tree arranged in left-to-right order.  For example,

;;      (define x (list (list 1 2) (list 3 4)))

;;      (fringe x)
;;      (1 2 3 4)

;;      (fringe (list x x))
;;      (1 2 3 4 1 2 3 4)
(defun fringe (tree)
  (cond ((null tree) nil)
        ((consp (car tree))
         (append (fringe (car tree)) (fringe (cdr tree))))
        (t (cons (car tree) (fringe (cdr tree))))))

(defun fringe-tail-recursive (tree)
  (labels ((iter (accl lst)
             (cond ((null lst) accl)
                   ((consp (car lst))
                    (iter (append (iter accl (car lst)))
                          (cdr lst)))
                   (t (iter (append accl (list (car lst))) (cdr lst))))))
    (iter nil tree)))
                    
; (defparameter *tmp* '((1 2) (3 4)))

; (fringe-tail-recursive (list *tmp* *tmp*))

;; *Exercise 2.29:* A binary mobile consists of two branches, a left branch and
;; a right branch.  Each branch is a rod of a certain length, from which hangs
;; either a weight or another binary mobile.  We can represent a binary mobile
;; using compound data by constructing it from two branches (for example, using
;; `list'):

;;      (define (make-mobile left right)
;;        (list left right))

;; A branch is constructed from a `length' (which must be a number) together
;; with a `structure', which may be either a number (representing a simple
;; weight) or another mobile:

;;      (define (make-branch length structure)
;;        (list length structure))

;;   a. Write the corresponding selectors `left-branch' and `right-branch',
;;      which return the branches of a mobile, and `branch-length' and
;;      `branch-structure', which return the components of a branch.
(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (cadr mobile))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (cadr branch))

(defun mobilep (structure)
  (consp structure))

(defparameter *w1* 1)
(defparameter *b1* (make-branch 1 *w1*))
(defparameter *w2* 3)
(defparameter *b2* (make-branch 1 *w2*))
(defparameter *m1-2* (make-mobile *b1* *b2*))
(defparameter *bm1-2* (make-branch 1 *m1-2*))
(defparameter *w3* 5)
(defparameter *b3* (make-branch 1 *w3*))
(defparameter *m1-2-3* (make-mobile *bm1-2* *b3*))
(defparameter *w4* 9)
(defparameter *b4* (make-branch 1 *w4*))
(defparameter *w5* 7)
(defparameter *b5* (make-branch 1 *w5*))
(defparameter *m4-5* (make-mobile *b4* *b5*))
(defparameter *bm4-5* (make-branch 1 *m4-5*))
(defparameter *w6* 8)
(defparameter *b6* (make-branch 1 *w6*))
(defparameter *m4-5-6* (make-mobile *bm4-5* *b6*))
(defparameter *bm1-2-3* (make-branch 1 *m1-2-3*))
(defparameter *bm4-5-6* (make-branch 1 *m4-5-6*))
(defparameter *m* (make-mobile *bm1-2-3* *bm4-5-6*))

;;   b. Using your selectors, define a procedure `total-weight' that returns the
;;      total weight of a mobile.
(defun total-weight (mobile)
  (let ((lbranch (left-branch mobile))
        (rbranch (right-branch mobile)))
    (labels ((iter (br)
               (let ((st (branch-structure br)))
                 (if (mobilep st)
                     (+ (iter (left-branch st))
                        (iter (right-branch st)))
                     st))))
      (+ (iter lbranch)
         (iter rbranch)))))

(total-weight *m*)
;;   c. A mobile is said to be "balanced" if the torque applied by its top-left
;;      branch is equal to that applied by its top-right branch (that is, if the
;;      length of the left rod multiplied by the weight hanging from that rod is
;;      equal to the corresponding product for the right side) and if each of
;;      the submobiles hanging off its branches is balanced. Design a predicate
;;      that tests whether a binary mobile is balanced.
(defun mobile-balancep (mobile)
  (labels ((weight-weight-balancep (weight-length1 weight1 weight-length2 weight2)
             (= (* weight-length1 weight1) (* weight-length2 weight2)))
           (mobile-weight-balancep (weight-length weight mobi-length mobi)
             (and (= (* weight-length weight) (* mobi-length (total-weight mobi)))
                  (mobile-balancep (mobi))))
           (balance-iter (lbr rbr)
             (let ((lst (branch-structure lbr))
                   (ll (branch-length lbr))
                   (rst (branch-structure rbr))
                   (rl (branch-length rbr)))
               (cond ((and (not (mobilep lst)) (not (mobilep rst)))
                      ;; weight weight
                      (weight-weight-balancep ll lst rl rst))
                     ((and (not (mobilep lst)) (mobilep rst))
                      ;; weight mobile
                      (mobile-weight-balancep ll lst rl rst))
                     ((and (mobilep lst) (not (mobilep rst)))
                      ;; mobile weight
                      (mobile-weight-balancep #'balance-iter rl rst
                                              ll lst))
                     (t
                      ;; mobile mobile
                      (and (= (* ll (total-weight lst)) (* rl (total-weight rst)))
                           (and (mobile-balancep lst) (mobile-balancep rst))))))))
    (balance-iter (left-branch mobile) (right-branch mobile))))

; (mobile-balancep *m*)
; (mobile-balancep '((3 ((1 3) (3 1))) (4 ((2 1) (1 2)))))
;;   d. Suppose we change the representation of mobiles so that the constructors
;;      are

;;           (define (make-mobile left right)
;;             (cons left right))

;;           (define (make-branch length structure)
;;             (cons length structure))

;;      How much do you need to change your programs to convert to the new
;;      representation?
'(Only selectors need change)

;; *Exercise 2.30:* Define a procedure `square-tree' analogous to the
;; `square-list' procedure of *note Exercise 2-21::.  That is, `square-list'
;; should behave as follows:

;;      (square-tree
;;       (list 1
;;             (list 2 (list 3 4) 5)
;;             (list 6 7)))
;;      (1 (4 (9 16) 25) (36 49))

;; Define `square-tree' both directly (i.e., without using any higher-order
;; procedures) and also by using `map' and recursion.
(defun square-tree (tree)
    (cond ((null tree)
           nil)
          ((consp (car tree))
           (cons (square-tree (car tree))
                 (square-tree (cdr tree))))
          (t (cons (square (car tree))
                   (square-tree (cdr tree))))))

(defun map-square-tree (tree)
  (map 'list #'(lambda (x)
                 (if (consp x)
                     (map-square-tree x)
                     (square x)))
       tree))

;; (square-tree '(1 (2 (3 4) 5) (6 7)))
;; (map-square-tree '(1 (2 (3 4) 5) (6 7)))

;; *Exercise 2.31:* Abstract your answer to *note Exercise 2-30:: to produce a
;; procedure `tree-map' with the property that `square-tree' could be defined as

;;      (define (square-tree tree) (tree-map square tree))
(defun tree-map (f tree)
  (map 'list #'(lambda (x)
                 (if (consp x)
                     (tree-map f x)
                     (funcall f x)))
       tree))

; ((lambda (x) (tree-map #'square x)) '(1 (2 (3 4) 5) (6 7)))

;; *Exercise 2.32:* We can represent a set as a list of distinct elements, and
;; we can represent the set of all subsets of the set as a list of lists.  For
;; example, if the set is `(1 2 3)', then the set of all subsets is `(() (3) (2)
;; (2 3) (1) (1 3) (1 2) (1 2 3))'.  Complete the following definition of a
;; procedure that generates the set of subsets of a set and give a clear
;; explanation of why it works:

;;      (define (subsets s)
;;        (if (null? s)
;;            (list nil)
;;            (let ((rest (subsets (cdr s))))
;;              (append rest (map <??> rest)))))
(defun subsets (s)
  (if (null s)
      '(nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map 'list #'(lambda (x)
                                    (cons (car s) x))
                          rest)))))

;; *Exercise 2.33:* Fill in the missing expressions to complete the
;; following definitions of some basic list-manipulation operations
;; as accumulations:

;;      (define (map p sequence)
;;        (accumulate (lambda (x y) <??>) nil sequence))

;;      (define (append seq1 seq2)
;;        (accumulate cons <??> <??>))

;;      (define (length sequence)
;;        (accumulate <??> 0 sequence))
(defun sicp-accumulate (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence) (sicp-accumulate op initial (cdr sequence)))))

;; (defun fold-right (op initial sequence)
;;   (labels ((iter (acc lst)
;;              (if (null lst)
;;                  acc
;;                  (iter (funcall op (car lst) initial) (cdr lst)))))
;;     (iter initial sequence)))
      
(defun sicp-map (p sequence)
  (sicp-accumulate #'(lambda (x y) (cons (funcall p x) y)) nil sequence))

(sicp-map #'(lambda (x)
              (* x x)) '(1 2 3 4 5))

(defun sicp-append (seq1 seq2)
  (sicp-accumulate #'cons seq2 seq1))

(defun sicp-length (sequence)
  (sicp-accumulate #'(lambda (x y) (+ 1 y)) 0 sequence))

; (sicp-length '(1 2 3 4 5 6 7))

;; *Exercise 2.34:* Evaluating a polynomial in x at a given value of x can be
;; formulated as an accumulation.  We evaluate the polynomial

;;      a_n r^n | a_(n-1) r^(n-1) + ... + a_1 r + a_0

;; using a well-known algorithm called "Horner's rule", which structures the
;; computation as

;;      (... (a_n r + a_(n-1)) r + ... + a_1) r + a_0

;; In other words, we start with a_n, multiply by x, add a_(n-1), multiply by x,
;; and so on, until we reach a_0.(3)

;; Fill in the following template to produce a procedure that evaluates a
;; polynomial using Horner's rule.  Assume that the coefficients of the
;; polynomial are arranged in a sequence, from a_0 through a_n.

;;      (define (horner-eval x coefficient-sequence)
;;        (accumulate (lambda (this-coeff higher-terms) <??>)
;;                         0
;;                         coefficient-sequence))

;; For example, to compute 1 + 3x + 5x^3 + x^(5) at x = 2 you would evaluate

;;      (horner-eval 2 (list 1 3 0 5 0 1))
(defun horner-eval (x sequence)
  (sicp-accumulate #'(lambda (this-coeff higher-terms)
                       (+ this-coeff (* x higher-terms)))
                   0
                   sequence))

;; *Exercise 2.35:* Redefine `count-leaves' from section *note 2-2-2:: as an
;; accumulation:

;;      (define (count-leaves t)
;;        (accumulate <??> <??> (map <??> <??>)))
(defun count-leaves (tree)
  (sicp-accumulate #'+ 0
                   (sicp-map #'(lambda (x)
                                 (cond ((null x) 0)
                                       ((consp x) (count-leaves x))
                                       (t 1)))
                             tree)))

;; *Exercise 2.36:* The procedure `accumulate-n' is similar to `accumulate'
;; except that it takes as its third argument a sequence of sequences, which are
;; all assumed to have the same number of elements.  It applies the designated
;; accumulation procedure to combine all the first elements of the sequences,
;; all the second elements of the sequences, and so on, and returns a sequence
;; of the results.  For instance, if `s' is a sequence containing four
;; sequences, `((1 2 3) (4 5 6) (7 8 9) (10 11 12)),' then the value of
;; `(accumulate-n + 0 s)' should be the sequence `(22 26 30)'.  Fill in the
;; missing expressions in the following definition of `accumulate-n':

;;      (define (accumulate-n op init seqs)
;;        (if (null? (car seqs))
;;            nil
;;            (cons (accumulate op init <??>)
;;                  (accumulate-n op init <??>))))
(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
      (cons (sicp-accumulate op init (map 'list #'(lambda (x)
                                                    (car x)) seqs))
            (accumulate-n op init (map 'list #'(lambda (x)
                                                 (cdr x)) seqs)))))

;(accumulate-n #'+ 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;; Exercise 2.37 Suppose we represent vectors v = (v_i) as sequences of numbers,
;; and matrices m = (m_(ij)) as sequences of vectors (the rows of the matrix).
;; For example, the matrix

;;      +-         -+
;;      |  1 2 3 4  |
;;      |  4 5 6 6  |
;;      |  6 7 8 9  |
;;      +-         -+

;; is represented as the sequence `((1 2 3 4) (4 5 6 6) (6 7 8 9))'.  With this
;; representation, we can use sequence operations to concisely express the basic
;; matrix and vector operations.  These operations (which are described in any
;; book on matrix algebra) are the following:

;;                                             __
;;   (dot-product v w)      returns the sum >_i v_i w_i

;;   (matrix-*-vector m v)  returns the vector t,
;;                                         __
;;                          where t_i = >_j m_(ij) v_j

;;   (matrix-*-matrix m n)  returns the matrix p,
;;                                            __
;;                          where p_(ij) = >_k m_(ik) n_(kj)

;;   (transpose m)          returns the matrix n,
;;                          where n_(ij) = m_(ji)

;; We can define the dot product as(4)

;;   (define (dot-product v w)
;;     (accumulate + 0 (map * v w)))

(defparameter *tmp-matrix* '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(defparameter *tmp-vec* '(3 2 1 0))
(defun dot-product (v w)
  (sicp-accumulate #'+ 0 (map 'list #'* v w)))

; (dot-product '(1 2 3 4) '(1 2 3 4))
;; Fill in the missing expressions in the following procedures for computing the
;; other matrix operations.  (The procedure `accumulate-n' is defined in
;; *noteExercise 2-36

;;   (define (matrix-*-vector m v)
;;     (map <??> m))

;;   (define (transpose mat)
;;     (accumulate-n <??> <??> mat))

;;   (define (matrix-*-matrix m n)
;;     (let ((cols (transpose n)))
;;       (map <??> m)))
(defun matrix-*-vector (m v)
  (map 'list #'(lambda (x)
                 (dot-product x v)) m))

(defun transpose (mat)
  (accumulate-n #'cons nil mat))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (map 'list #'(lambda (x)
                   (matrix-*-vector cols x))
                   m)))

(defparameter *tmp-matrix-2* '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)))
; (matrix-*-matrix *tmp-matrix* *tmp-matrix-2*)

;; *Exercise 2.38:* The `accumulate' procedure is also known as
;; `fold-right', because it combines the first element of the
;; sequence with the result of combining all the elements to the
;; right.  There is also a `fold-left', which is similar to
;; `fold-right', except that it combines elements working in the
;; opposite direction:

;;      (define (fold-left op initial sequence)
;;        (define (iter result rest)
;;          (if (null? rest)
;;              result
;;              (iter (op result (car rest))
;;                    (cdr rest))))
;;        (iter initial sequence))

;; What are the values of

;;      (fold-right / 1 (list 1 2 3))

;;      (fold-left / 1 (list 1 2 3))

;;      (fold-right list nil (list 1 2 3))

;;      (fold-left list nil (list 1 2 3))

;; Give a property that `op' should satisfy to guarantee that
;; `fold-right' and `fold-left' will produce the same values for any
;; sequence.
(defun fold-left (op initial sequence)
  (labels ((iter (result rest)
             (if (null rest)
                 result
                 (iter (funcall op result (car rest))
                       (cdr rest)))))
    (iter initial sequence)))

(defun fold-right (&rest args)
  (apply #'sicp-accumulate args))



;; (fold-right #'/ 1 (list 1 2 3))
;; => (iter (/ 1 '1) (2 3))
;; => (iter (/ 2 1) (3))
;; => (iter (/ 3 2) nil)
;; => 3/2

;; (fold-left #'/ 1 (list 1 2 3))
;; => (iter (/ '1 1) (2 3))
;; => (iter (/ 1 2) (3))
;; => (iter (/ 1/2 3) nil)
;; => 1/6


;(fold-right #'list nil (list 1 2 3))
;; (fold-right #'list nil (list 1 2 3))
;; => (list 3 nil)
;; => (list 2 '(3 nil))
;; => (list 1 '(2 (3 nil))) 
;; => '(1 (2 (3 nil)))

;; (fold-left #'list nil (list 1 2 3))
;; => (list nil 3)
;; => (list '(nil 3) 2)
;; => (list '((nil 3) 2) 1)
;; => '(((nil 3) 2) 1)
;; '(((nil 1) 2 3))
;; op should be a Associative operation, (equal (op c (a b)) (op a (b c))

;; *Exercise 2.39:* Complete the following definitions of `reverse'
;; (*note Exercise 2-18::) in terms of `fold-right' and `fold-left'
;; from *note Exercise 2-38:::

;;      (define (reverse sequence)
;;        (fold-right (lambda (x y) <??>) nil sequence))

;;      (define (reverse sequence)
;;        (fold-left (lambda (x y) <??>) nil sequence))
(defun reverse-by-fold-right (sequence)
  (fold-right #'(lambda (x y) (append y (list x))) nil sequence))

(defun reverse-by-fold-left (sequence)
  (fold-left #'(lambda (x y) (cons y x)) nil sequence))

;(reverse-by-fold-left '(1 2 3 4 5))
;(reverse-by-fold-right '(1 2 3 4 5))

;; *Exercise 2.40:* Define a procedure `unique-pairs' that, given an integer n,
;; generates the sequence of pairs (i,j) with 1 <= j< i <= n.  Use
;; `unique-pairs' to simplify the definition of `prime-sum-pairs' given above.
(defun enumerate-interval (beg end)
  (accumulate #'cons nil #'(lambda (x) x) beg #'1+ end))

(defun unique-pairs (n)
  (sicp-accumulate #'append nil
                   (map 'list #'(lambda (i)
                                  (map 'list #'(lambda (j)
                                                 (list i j))
                                       (enumerate-interval 1 (1- i))))
                        (enumerate-interval 1 n))))

; (unique-pairs 6)

(defun filter (p sequence)
  (if (null sequence)
      nil
      (if (funcall p (car sequence))
          (cons (car sequence) (filter p (cdr sequence)))
          (filter p (cdr sequence)))))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-sump (pair)
  (primep (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
  (map 'list #'make-pair-sum
       (filter #'prime-sump
               (unique-pairs n))))

; (prime-sum-pairs 6)

(defun unique-triples (n)
  (sicp-accumulate #'append nil
                   (map 'list #'(lambda (x)
                                  (map 'list #'(lambda (pair)
                                                 (cons x pair))
                                       (unique-pairs (1- x))))
                        (enumerate-interval 1 n))))


(defun ordered-triples-with-sum (n s)
  (filter #'(lambda (x)
              (= (apply #'+ x) s))
          (unique-triples n)))

;; *Figure 2.8:* A solution to the eight-queens puzzle.

;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   |   | Q |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   | Q |   |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      | Q |   |   |   |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   |   |   | Q |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   | Q |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   |   |   |   | Q |
;;      +---+---+---+---+---+---+---+---+
;;      |   | Q |   |   |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   | Q |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+

;; *Exercise 2.42:* The "eight-queens puzzle" asks how to place eight queens on
;; a chessboard so that no queen is in check from any other (i.e., no two queens
;; are in the same row, column, or diagonal).  One possible solution is shown in
;; *note Figure 2-8::.  One way to solve the puzzle is to work across the board,
;; placing a queen in each column.  Once we have placed k - 1 queens, we must
;; place the kth queen in a position where it does not check any of the queens
;; already on the board.  We can formulate this approach recursively: Assume
;; that we have already generated the sequence of all possible ways to place k -
;; 1 queens in the first k - 1 columns of the board.  For each of these ways,
;; generate an extended set of positions by placing a queen in each row of the
;; kth column.  Now filter these, keeping only the positions for which the queen
;; in the kth column is safe with respect to the other queens.  This produces
;; the sequence of all ways to place k queens in the first k columns.  By
;; continuing this process, we will produce not only one solution, but all
;; solutions to the puzzle.

;; We implement this solution as a procedure `queens', which returns a sequence
;; of all solutions to the problem of placing n queens on an n*n chessboard.
;; `Queens' has an internal procedure `queen-cols' that returns the sequence of
;; all ways to place queens in the first k columns of the board.

;;      (define (queens board-size)
;;        (define (queen-cols k)
;;          (if (= k 0)
;;              (list empty-board)
;;              (filter
;;               (lambda (positions) (safe? k positions))
;;               (flatmap
;;                (lambda (rest-of-queens)
;;                  (map (lambda (new-row)
;;                         (adjoin-position new-row k rest-of-queens))
;;                       (enumerate-interval 1 board-size)))
;;                (queen-cols (- k 1))))))
;;        (queen-cols board-size))

;; In this procedure `rest-of-queens' is a way to place k - 1 queens in the
;; first k - 1 columns, and `new-row' is a proposed row in which to place the
;; queen for the kth column.  Complete the program by implementing the
;; representation for sets of board positions, including the procedure
;; `adjoin-position', which adjoins a new row-column position to a set of
;; positions, and `empty-board', which represents an empty set of positions.
;; You must also write the procedure `safe?', which determines for a set of
;; positions, whether the queen in the kth column is safe with respect to the
;; others.  (Note that we need only check whether the new queen is safe--the
;; other queens are already guaranteed safe with respect to each other.)
(defun queens (board-size)
  (labels ((adjoin-position (row col rest)
             (cons (list row col) rest))
           (safep (col pos-seq)
             (let ((row (car (car pos-seq))))
               (and (queen-no-same-rowp row (cdr pos-seq))
                    (queen-no-same-diagonalp row col (cdr pos-seq)))))
           (queen-cols (k)
             (if (= k 0)
                 (list nil)
                 (filter
                  #'(lambda (positions) (safep k positions))
                  (mapcan
                   #'(lambda (rest-of-queens)
                       (map 'list #'(lambda (new-row)
                                      (adjoin-position new-row k rest-of-queens))
                            (enumerate-interval 1 board-size)))
                   (queen-cols (1- k)))))))
    (queen-cols board-size)))

(defun queen-no-same-rowp (row rest-queens)
  (if (null rest-queens)
      t
      (and (not (= row (car (car rest-queens))))
           (queen-no-same-rowp row (cdr rest-queens)))))

(defun queen-no-same-diagonalp (row col rest-queens)
  (labels ((iter (acc q)
             (if (or (null acc) (null q))
                 acc
                 (iter (and (not (= (abs (- row (car (car q))))
                                    (- col (cadr (car q)))))
                                 acc)
                       (cdr q)))))
           (iter t rest-queens)))

;(time (queens 8))

;; Exercise 2.43.  Louis Reasoner is having a terrible time doing exercise
;; 2.42. His queens procedure seems to work, but it runs extremely
;; slowly. (Louis never does manage to wait long enough for it to solve even the
;; 6¡Á 6 case.) When Louis asks Eva Lu Ator for help, she points out that he has
;; interchanged the order of the nested mappings in the flatmap, writing it as

;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;           (adjoin-position new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run slowly. Estimate how long
;; it will take Louis's program to solve the eight-queens puzzle, assuming that
;; the program in exercise 2.42 solves the puzzle in time T.

;; louis's program compute each `queen-cols' N times. thus the N step recursive
;; call will be N^(N-1) times slower than T.

;; *Exercise 2.44:* Define the procedure `up-split' used by `corner-split'.  It
;; is similar to `right-split', except that it switches the roles of `below' and
;; `beside'.
(defun flipped-pairs (painter)
  (let ((painter2 (beside painer (flip-vert painter))))
    (below painter2 painter2)))

(defun right-split (painter n)
  (if (= 0 n)
      painter
      (let ((smaller (right-split painter (1- n))))
        (beside painter (below smaller smaller)))))

(defun corner-split (painter n)
  (if (= 0 n)
      painter
      (let* ((up (up-split painter (1- n)))
             (right (right-split painter (1- n)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (top-right (corner-split painter (1- n))))
        (beside (below painter top-left)
                (below bottom-right top-right)))))

(defun square-limit (painter n)
  (let* ((corner (corner-split painter n))
         (half (below (flip-vert corner) corner)))
    (beside (flip-horiz half) half)))

(defun up-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (1- n))))
        (below painter (beside smaller smaller)))))

;; *Exercise 2.45:* `Right-split' and `up-split' can be expressed as instances
;; of a general splitting operation.  Define a procedure `split' with the
;; property that evaluating

;;      (define right-split (split beside below))
;;      (define up-split (split below beside))

;; produces procedures `right-split' and `up-split' with the same behaviors as
;; the ones already defined.
(defun painter-split (first-split second-split)
  (lambda (painter n)
    (if (= 0 n)
        painter
        (let ((smaller (funcall (painter-split first-split second-split) painter (1- n))))
          (funcall first-split (funcall second-split smaller smaller))))))

;; *Exercise 2.46:* A two-dimensional vector v running from the origin to a
;; point can be represented as a pair consisting of an x-coordinate and a
;; y-coordinate.  Implement a data abstraction for vectors by giving a
;; constructor `make-vect' and corresponding selectors `xcor-vect' and
;; `ycor-vect'.  In terms of your selectors and constructor, implement
;; procedures `add-vect', `sub-vect', and `scale-vect' that perform the
;; operations vector addition, vector subtraction, and multiplying a vector by a
;; scalar:

;;      (x_1, y_1) + (x_2, y_2) = (x_1 + x_2, y_1 + y_2)
;;      (x_1, y_1) - (x_2, y_2) = (x_1 - x_2, y_1 - y_2)
;;                   s * (x, y) = (sx, sy)
(defun make-vect (a b)
  (list a b))

(defun xcor-vect (v)
  (car v))

(defun ycor-vect (v)
  (cadr v))

(defun add-vect (v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(defun sub-vect (v1 v2)
  (add-vect v1 (scale-vect v2 -1)))

(defun scale-vect (v s)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;; *Exercise 2.47:* Here are two possible constructors for frames:

;;      (define (make-frame origin edge1 edge2)
;;        (list origin edge1 edge2))

;;      (define (make-frame origin edge1 edge2)
;;        (cons origin (cons edge1 edge2)))

;; For each constructor supply the appropriate selectors to produce an
;; implementation for frames.
(defun make-frame (origin edge1 edge2)
  (list origin edge1 edge2))

(defun origin-frame (f)
  (car f))

(defun edge1-frame (f)
  (cadr f))

(defun edge2-frame (f)
  (caddr f))

(defun frame-coord-map (frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (edge1-frame frame) (xcor-vect v))
               (scale-vect (edge2-frame frame) (ycor-vect v))))))

(defun segments->painter (line-pen segment-list)
    (lambda (frame)
      (for-each
       #'(lambda (segment)
           (funcall line-pen (funcall (frame-coord-map frame) (start-segment segment))
                    (funcall (frame-coord-map frame) (end-segment segment))))
       segment-list)))

;; *Exercise 2.48:* A directed line segment in the plane can be represented as a
;; pair of vectors--the vector running from the origin to the start-point of the
;; segment, and the vector running from the origin to the end-point of the
;; segment.  Use your vector representation from *note Exercise 2-46:: to define
;; a representation for segments with a constructor `make-segment' and selectors
;; `start-segment' and `end-segment'.
(defun make-segment (start end)
  (list start end))

(defun start-segment (segment)
  (car segment))

(defun end-segment (segment)
  (cadr segment))

;; *Exercise 2.49:* Use `segments->painter' to define the following primitive
;; painters:

;;   a. The painter that draws the outline of the designated frame.

;;   b. The painter that draws an "X" by connecting opposite corners of the
;;      frame.

;;   c. The painter that draws a diamond shape by connecting the midpoints of
;;      the sides of the frame.

;;   d. The `wave' painter.
(defparameter *frame-outline*
  (mapcar #'(lambda (x)
              (make-segment (make-vect (caar x) (cdar x))
                            (make-vect (caadr x) (cdadr x))))
          '(((0.0 . 0.0) (1.0 . 0.0))
            ((1.0 . 0.0) (1.0 . 1.0))
            ((1.0 . 1.0) (0.0 . 1.0))
            ((0.0 . 1.0) (0.0 . 0.0)))))

;; *Exercise 2.50:* Define the transformation `flip-horiz', which flips painters
;; horizontally, and transformations that rotate painters counterclockwise by
;; 180 degrees and 270 degrees.
(defun transform-painter (painter origin corner1 corner2)
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
           (new-origin (funcall m origin)))
      (funcall painter (make-frame new-origin
                                   (sub-vect (funcall m corner1) new-origin)
                                   (sub-vect (funcall m corner2) new-origin))))))

(defun flip-vert (painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defun rotate90 (painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defun squash-inwards (painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(defun beside (painter1 painter2)
  (let* ((split-point (make-vect 0.5 0))
         (paint-left (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        split-point
                                        (make-vect 0.0 1.0)))
         (paint-right (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.0)
                                         (make-vect (xcor-vect split-point) 1.0))))
    (lambda (frame)
      (funcall paint-left frame)
      (funcall paint-right frame))))
      
(defun flip-horiz (painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(defun rotate180 (painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(defun rotate270 (painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; *Exercise 2.51:* Define the `below' operation for painters.  `Below' takes
;; two painters as arguments.  The resulting painter, given a frame, draws with
;; the first painter in the bottom of the frame and with the second painter in
;; the top.  Define `below' in two different ways--first by writing a procedure
;; that is analogous to the `beside' procedure given above, and again in terms
;; of `beside' and suitable rotation operations (from *note Exercise 2-50::).
(defun below (painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(defparameter *identity-frame*
  (make-frame (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              (make-vect 0.0 1.0)))

;; *Exercise 2.53:* What would the interpreter print in response to evaluating
;; each of the following expressions?

;;      (list 'a 'b 'c)

;;      (list (list 'george))

;;      (cdr '((x1 x2) (y1 y2)))

;;      (cadr '((x1 x2) (y1 y2)))

;;      (consp (car '(a short list)))

;;      (member 'red '((red shoes) (blue socks)))

;;      (member 'red '(red shoes blue socks))

;; *Exercise 2.54:* Two lists are said to be `equal?' if they contain equal
;; elements arranged in the same order.  For example,

;;      (equal? '(this is a list) '(this is a list))

;; is true, but

;;      (equal? '(this is a list) '(this (is a) list))

;; is false.  To be more precise, we can define `equal?'  recursively in terms
;; of the basic `eq?' equality of symbols by saying that `a' and `b' are
;; `equal?' if they are both symbols and the symbols are `eq?', or if they are
;; both lists such that `(car a)' is `equal?'  to `(car b)' and `(cdr a)' is
;; `equal?' to `(cdr b)'.  Using this idea, implement `equal?' as a
;; procedure.(5)
(defun equal? (lst1 lst2)
  (let ((a (car lst1))
        (b (car lst2)))
    (cond ((or (null lst1) (null lst2))
           (and (null lst1) (null lst2)))
          ((and (consp a) (consp b))
           (and (equal? a b) (equal? (cdr lst1) (cdr lst2))))
          (t (and (eql a b) (equal? (cdr lst1) (cdr lst2)))))))

;; *Exercise 2.55:* Eva Lu Ator types to the interpreter the expression

;;      (car ''abracadabra)

;; To her surprise, the interpreter prints back `quote'.  Explain.
(car (quote (quote abracadabra)))

;; *Exercise 2.56:* Show how to extend the basic differentiator to handle more
;; kinds of expressions.  For instance, implement the differentiation rule

;;      n_1   n_2
;;      --- = ---  if and only if n_1 d_2 = n_2 d_1
;;      d_1   d_2

;; by adding a new clause to the `deriv' program and defining appropriate
;; procedures `exponentiation?', `base', `exponent', and `make-exponentiation'.
;; (You may use the symbol `**' to denote exponentiation.)  Build in the rules
;; that anything raised to the power 0 is 1 and anything raised to the power 1
;; is the thing itself.
(defun deriv (exp var)
  (fresh-line)
  (format t "~a" exp)
  (cond ((numberp exp) 0)
        ((variablep exp)
         (if (same-variablep exp var) 1 0))
        ((sump exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((productp exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiationp exp)
         (make-product 
          (make-product (exp-exponent exp)
                        (make-exponentiation (exp-base exp)
                                             (make-sum (exp-exponent exp) -1)))
          (deriv (exp-base exp) var)))
        (t (error "unknown expression type -- DERIV ~a:" exp))))

(defun variablep (x)
  (symbolp x))

(defun same-variablep (v1 v2)
  (and (variablep v1) (variablep v2) (eq v1 v2)))

(defun =numberp (exp num)
  (and (numberp exp) (= exp num)))

(defun make-sum (a1 a2)
  (cond ((=numberp a1 0) a2)
        ((=numberp a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list '+ a1 a2))))

(defun make-product (m1 m2)
  (cond ((or (=numberp m1 0) (=numberp m2 0)) 0)
        ((=numberp m1 1) m2)
        ((=numberp m2 1) m1)
        ((and (numberp m1) (numberp m2)) (* m1 m2))
        (t (list '* m1 m2))))

(defun sump (x)
  (and (consp x) (eq (car x) '+)))

(defun addend (s)
  (cadr s))

(defun augend (s)
  (if (null (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(defun productp (s)
  (and (consp s) (eq (car s) '*)))

(defun multiplier (p) (cadr p))

(defun multiplicand (p)
  (if (null (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(defun exponentiation (base exponent)
  (list '^ base exponent))

(defun exponentiationp (exp)
  (and (consp exp) (eq (car exp) '^)))

(defun exp-base (exp)
  (cadr exp))

(defun exp-exponent (exp)
  (caddr exp))

(defun make-exponentiation (base expo)
  (cond ((=numberp base 0) 0)
        ((=numberp base 1) 1)
        ((=numberp expo 0) 1)
        ((=numberp expo 1) base)
        ((and (numberp base) (numberp expo)) (expt base expo))
        (t (list '^ base expo))))

;; *Exercise 2.57:* Extend the differ (cons '* (cddr p))))entiation program to
;; handle sums and products of arbitrary numbers of (two or more) terms.  Then
;; the last example above could be expressed as

;;      (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and products,
;; without changing the `deriv' procedure at all.  For example, the `addend' of
;; a sum would be the first term, and the `augend' would be the sum of the rest
;; of the terms.
;; Try to do this by changing only the representation for sums and products,
;; without changing the `deriv' procedure at all.  For example, the `addend' of
;; a sum would be the first term, and the `augend' would be the sum of the rest
;; of the terms.
(defun augend (s)
  (if (null (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(defun multiplicand (p)
  (if (null (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;; *Exercise 2.58:* Suppose we want to modify the differentiation program so
;; that it works with ordinary mathematical notation, in which `+' and `*' are
;; infix rather than prefix operators.  Since the differentiation program is
;; defined in terms of abstract data, we can modify it to work with different
;; representations of expressions solely by changing the predicates, selectors,
;; and constructors that define the representation of the algebraic expressions
;; on which the differentiator is to operate.

;;   a. Show how to do this in order to differentiate algebraic expressions
;;      presented in infix form, such as `(x + (3 * (x + (y + 2))))'.  To
;;      simplify the task, assume that `+' and `*' always take two arguments and
;;      that expressions are fully parenthesized.
(defun make-sum (a1 a2)
  (cond ((=numberp a1 0) a2)
        ((=numberp a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list a1 '+ a2))))

(defun make-product (m1 m2)
  (cond ((or (=numberp m1 0) (=numberp m2 0)) 0)
        ((=numberp m1 1) m2)
        ((=numberp m2 1) m1)
        ((and (numberp m1) (numberp m2)) (* m1 m2))
        (t (list m1 '* m2))))

(defun sump (x)
  (and (consp x) (eq (cadr x) '+)))

(defun addend (s)
  (car s))
 
(defun augend (s)
  (caddr s))

(defun productp (s)
  (and (consp s) (eq (cadr s) '*)))

(defun multiplier (p) (car p))

(defun multiplicand (p)
  (caddr p))

;;   b. The problem becomes substantially harder if we allow standard algebraic
;;      notation, such as `(x + 3 * (x + y + 2))', which drops unnecessary
;;      parentheses and assumes that multiplication is done before addition.
;;      Can you design appropriate predicates, selectors, and constructors for
;;      this notation such that our derivative program still works?
(defun all->list (x)
  (if (or (listp x) (null x))
      x
      (list x)))

(defun make-sum (a1 a2)
  (cond ((=numberp a1 0) a2)
        ((=numberp a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (append (all->list a1) (cons '+ (all->list a2))))))

(defun make-product (m1 m2)
  (cond ((or (=numberp m1 0) (=numberp m2 0)) 0)
        ((=numberp m1 1) m2)
        ((=numberp m2 1) m1)
        ((and (numberp m1) (numberp m2)) (* m1 m2))
        (t (list m1 '* m2))))

;; sum <- atom | product + atom | product | sum
;; product <- atom  * atom | product
;; atom <- number | symbol | list
;; 
(defun before-symbol (lst sym)
  (labels ((iter (acc x)
             (cond ((null x) nil)
                   ((eq (car x) sym)
                    acc)
                   (t (iter (cons (car x) acc) (cdr x))))))
    (iter nil lst)))

(defun after-symbol (lst sym)
  (if (or (null lst) (eq (car lst) sym))
      (cdr lst)
      (after-symbol (cdr lst) sym)))

(defun atomp (x)
  (labels ((p (s)
             (or (numberp x)
                 (variablep x)
                 (listp x))))
  (cond ((null x) nil)
        ((consp x) (and (p (car x)) (null (cdr x))))
        (t (p x)))))

(defun productp (s)
  (let ((left (before-symbol s '*))
        (right (after-symbol s '*)))
    (if (null s)
        nil
        (and (atomp left) (or (productp right) (atomp right))))))

(defun multiplier (p)
  (let ((left (before-symbol p '*)))
    (if (null left)
        nil
        (car left))))

(defun multiplicand (p)
  (let ((right (after-symbol p '*)))
    (if (productp right)
        right
        (car right))))

(defun sump (x)
  (let ((left (before-symbol x '+))
        (right (after-symbol x '+)))
    (and (or (productp left) (atomp left))
         (or (productp right) (atomp right) (sump right)))))

(defun addend (s)
  (let ((left (before-symbol s '+)))
    (if (productp left)
        left
        (car left))))
 
(defun augend (s)
  (let ((right (after-symbol s '+)))
    (if (or (productp right) (sump right))
        right
        (car right))))

;(defparameter *tmp* '(x + 3 * (x + y + 2)))

;(deriv *tmp* 'x)

;; *Exercise 2.59:* Implement the `union-set' operation for the unordered-list
;; representation of sets.
(defun element-of-unordered-setp (x set)
  (cond ((null set) nil)
        ((eq (car set) x) t)
        (t (element-of-unordered-setp x (cdr set)))))

(defun adjoin-unordered-set (x set)
  (if (element-of-unordered-setp x set)
      set
      (cons x set)))

(defun intersection-unordered-set (set1 set2)
  (cond ((or (null set1) (null set2)) nil)
        ((element-of-unordered-setp (car set1) set2)
         (cons (car set1) (intersection-unordered-set (cdr set1) set2)))
        (t (intersection-unordered-set (cdr set1) set2))))

(defun union-unordered-set (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        ((element-of-unordered-setp (car set1) set2)
         (union-unordered-set (cdr set1) set2))
        (t (cons (car set1) (union-unordered-set (cdr set1) set2)))))

;; *Exercise 2.61:* Give an implementation of `adjoin-set' using the ordered
;; representation.  By analogy with `element-of-set?' show how to take advantage
;; of the ordering to produce a procedure that requires on the average about
;; half as many steps as with the unordered representation.
(defun adjoin-ordered-set (x set)
  (if (null set)
      (list x)
      (let ((x1 (car set)))
        (cond ((= x x1) set)
              ((< x x1) (cons x set))
              (t (cons x1 (adjoin-ordered-set x (cdr set))))))))

;; *Exercise 2.62:* Give a [theta](n) implementation of `union-set' for sets
;; represented as ordered lists.
(defun union-ordered-set (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        (t (let ((x1 (car set1))
                 (x2 (car set2)))
             (cond ((= x1 x2) (cons x1 (union-ordered-set (cdr set1) (cdr set2))))
                   ((< x1 x2) (cons x1 (union-ordered-set (cdr set1) set2)))
                   ((< x2 x1) (cons x2 (union-ordered-set set1 (cdr set2)))))))))

;; *Exercise 2.63:* Each of the following two procedures converts a
;; binary tree to a list.

;;      (define (tree->list-1 tree)
;;        (if (null? tree)
;;            '()
;;            (append (tree->list-1 (left-branch tree))
;;                    (cons (entry tree)
;;                          (tree->list-1 (right-branch tree))))))

;;      (define (tree->list-2 tree)
;;        (define (copy-to-list tree result-list)
;;          (if (null? tree)
;;              result-list
;;              (copy-to-list (left-branch tree)
;;                            (cons (entry tree)
;;                                  (copy-to-list (right-branch tree)
;;                                                result-list)))))
;;        (copy-to-list tree '()))

;;   a. Do the two procedures produce the same result for every tree?
;;      If not, how do the results differ?  What lists do the two
;;      procedures produce for the trees in *note Figure 2-16::?
(defun entry (tree)
  (car tree))

(defun left-branch (tree)
  (cadr tree))

(defun right-branch (tree)
  (caddr tree))

(defun tree->list-1 (tree)
  (if (null tree)
      nil
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(defun tree->list-2 (tree)
  (labels ((copy-to-list (tree acc)
             (if (null tree)
                 acc
                 (copy-to-list (left-branch tree)
                               (cons (entry tree)
                                     (copy-to-list (right-branch tree) acc))))))
    (copy-to-list tree nil)))
;; '(1 5 3 7 9 11)

;;   b. Do the two procedures have the same order of growth in the
;;      number of steps required to convert a balanced tree with n
;;      elements to a list?  If not, which one grows more slowly?

;; Yes, both O(n)

;; *Exercise 2.64:* The following procedure `list->tree' converts an ordered
;; list to a balanced binary tree.  The helper procedure `partial-tree' takes as
;; arguments an integer n and list of at least n elements and constructs a
;; balanced tree containing the first n elements of the list.  The result
;; returned by `partial-tree' is a pair (formed with `cons') whose `car' is the
;; constructed tree and whose `cdr' is the list of elements not included in the
;; tree.

;;      (define (list->tree elements)
;;        (car (partial-tree elements (length elements))))

;;      (define (partial-tree elts n)
;;        (if (= n 0)
;;            (cons '() elts)
;;            (let ((left-size (quotient (- n 1) 2)))
;;              (let ((left-result (partial-tree elts left-size)))
;;                (let ((left-tree (car left-result))
;;                      (non-left-elts (cdr left-result))
;;                      (right-size (- n (+ left-size 1))))
;;                  (let ((this-entry (car non-left-elts))
;;                        (right-result (partial-tree (cdr non-left-elts)
;;                                                    right-size)))
;;                    (let ((right-tree (car right-result))
;;                          (remaining-elts (cdr right-result)))
;;                      (cons (make-tree this-entry left-tree right-tree)
;;                            remaining-elts))))))))

;;   a. Write a short paragraph explaining as clearly as you can how
;;      `partial-tree' works.  Draw the tree produced by `list->tree' for the
;;      list `(1 3 5 7 9 11)'.
(defun partial-tree (elts n)
  (if (= 0 n)
      (cons nil elts)
      (let* ((left-size (floor (/ (1- n) 2)))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (rest-elts (cdr left-result))
             (right-size (- n left-size 1))
             (this-entry (car rest-elts))
             (right-result (partial-tree (cdr rest-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (list this-entry left-tree right-tree)
              remaining-elts))))

(defun list->tree (lst)
  (car (partial-tree lst (length lst))))
;(list->tree '(1 3 5 7 9 11))

;;   b. What is the order of growth in the number of steps required by
;;      `list->tree' to convert a list of n elements?
;; O(n)

;; *Exercise 2.65:* Use the results of Exercise 2-63:: and Exercise 2-64:: to
;; *give [theta](n) implementations of `union-set' and `intersection-set' for
;; *sets implemented as (balanced) binary trees.(5)
(defun intersection-ordered-set (set1 set2)
  (if (or (null set1) (null set2))
      nil
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-ordered-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-ordered-set (cdr set1) set2))
              (t (intersection-ordered-set set1 (cdr set2)))))))


(defparameter *tree1* '(5 (2 (1 nil nil) (3 nil nil)) (13 (8 nil nil) (21 nil nil))))
(defparameter *tree2* '(5 (1 nil (3 nil nil)) (9 (7 nil nil) (11 nil nil))))

(defun union-binary-tree-set (set1 set2)
  (list->tree (union-ordered-set (tree->list-1 set1) (tree->list-2 set2))))

(defun intersection-binary-tree-set (set1 set2)
  (list->tree (intersection-ordered-set (tree->list-2 set1) (tree->list-1 set2))))

;(tree->list-1 (union-binary-tree-set *tree1* *tree2*))
;(tree->list-2 (intersection-binary-tree-set *tree1* *tree2*))

(defun make-huffman-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun huffman-leafp (object)
  (eq (car object) 'leaf))

(defun symbol-huffman-leaf (x)
  (cadr x))

(defun weight-huffman-leaf (x)
  (caddr x))

(defun make-code-tree (left right)
  (list left right
        (append (huffman-symbols left) (huffman-symbols right))
        (+ (huffman-weight left) (huffman-weight right))))

(defun code-tree-left-branch (tree)
  (car tree))

(defun code-tree-right-branch (tree)
  (cadr tree))

(defun huffman-symbols (tree)
  (if (huffman-leafp tree)
      (list (symbol-huffman-leaf tree))
      (caddr tree)))

(defun huffman-weight (tree)
  (if (huffman-leafp tree)
      (weight-huffman-leaf tree)
      (cadddr tree)))

(defun huffman-decode (bits tree)
  (labels ((decode-1 (bits current-branch)
             (if (null bits)
                 nil
                 (let ((next-branch (select-huffman-branch (car bits) current-branch)))
                   (if (huffman-leafp next-branch)
                       (cons (symbol-huffman-leaf next-branch)
                             (decode-1 (cdr bits) tree))
                       (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

(defun select-huffman-branch (bit tree)
  (cond ((= 0 bit) (code-tree-left-branch tree))
        ((= 1 bit) (code-tree-right-branch tree))
        (t (error "bat bits -- CHOOSE BRANCH ~a" bit))))

(defun adjoin-huffman-set (x set)
  (cond ((null set) (list x))
        ((< (huffman-weight x) (huffman-weight (car set)))
         (cons x set))
        (t (cons (car set) (adjoin-huffman-set x (cdr set))))))

(defun make-huffman-leaf-set (pairs)
  (if (null pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-huffman-set (make-huffman-leaf (car pair) (cadr pair))
                            (make-huffman-leaf-set (cdr pairs))))))

;; *Exercise 2.67:* Define an encoding tree and a sample message:

;;      (define sample-tree
;;        (make-code-tree (make-leaf 'A 4)
;;                        (make-code-tree
;;                         (make-leaf 'B 2)
;;                         (make-code-tree (make-leaf 'D 1)
;;                                         (make-leaf 'C 1)))))

;;      (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; Use the `decode' procedure to decode the message, and give the result.
(defparameter *sample-huffman-tree*
    (make-code-tree (make-huffman-leaf 'A 4)
                    (make-code-tree
                     (make-huffman-leaf 'B 2)
                     (make-code-tree (make-huffman-leaf 'D 1)
                                     (make-huffman-leaf 'C 1)))))

(defparameter *sample-huffman-message* '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(huffman-decode *sample-huffman-message* *sample-huffman-tree*)

;; *Exercise 2.68:* The `encode' procedure takes as arguments a message and a
;; tree and produces the list of bits that gives the encoded message.

;;      (define (encode message tree)
;;        (if (null? message)
;;            '()
;;            (append (encode-symbol (car message) tree)
;;                    (encode (cdr message) tree))))

;; `Encode-symbol' is a procedure, which you must write, that returns the list
;; of bits that encodes a given symbol according to a given tree.  You should
;; design `encode-symbol' so that it signals an error if the symbol is not in
;; the tree at all.  Test your procedure by encoding the result you obtained in
;; *note Exercise 2-67:: with the sample tree and seeing whether it is the same
;; as the original sample message.
(defun huffman-encode (msg tree)
  (if (null msg)
      nil
      (append (huffman-encode-symbol (car msg) tree)
              (huffman-encode (cdr msg) tree))))

(defun huffman-encode-symbol (symbol tree)
  (cond ((null tree)
         (error "NULL HUFFMAN TREE"))
        ((huffman-leafp tree)
         (if (eq symbol (symbol-huffman-leaf tree))
             nil
             (error "-- INVALID SYMBOL ~a" symbol)))
        (t (if (member symbol (huffman-symbols (code-tree-left-branch tree)))
               (cons 0 (huffman-encode-symbol symbol (code-tree-left-branch tree)))
               (cons 1 (huffman-encode-symbol symbol (code-tree-right-branch tree)))))))

;(huffman-encode '(A D A B B C A) *sample-huffman-tree*)

;; *Exercise 2.69:* The following procedure takes as its argument a list of
;; symbol-frequency pairs (where no symbol appears in more than one pair) and
;; generates a Huffman encoding tree according to the Huffman algorithm.

;;      (define (generate-huffman-tree pairs)
;;        (successive-merge (make-leaf-set pairs)))

;; `Make-leaf-set' is the procedure given above that transforms the list of
;; pairs into an ordered set of leaves.  `Successive-merge' is the procedure you
;; must write, using `make-code-tree' to successively merge the smallest-weight
;; elements of the set until there is only one element left, which is the
;; desired Huffman tree.  (This procedure is slightly tricky, but not really
;; complicated.  If you find yourself designing a complex procedure, then you
;; are almost certainly doing something wrong.  You can take significant
;; advantage of the fact that we are using an ordered set representation.)
(defun generate-huffman-tree (pairs)
  (successive-merge (make-huffman-leaf-set pairs)))

(defun successive-merge (leaf-sets)
  (let ((leaf1 (car leaf-sets))
        (leaf2 (cadr leaf-sets)))
    (if (or (null leaf1) (null leaf2))
        (car leaf-sets)
        (successive-merge (adjoin-huffman-set (make-code-tree leaf1 leaf2)
                                              (cddr leaf-sets))))))

;(defparameter *leaf-pairs* '((C 8) (D 6) (E 15) (F 2) (G 5) (A 7) (B 1)))

;; *Exercise 2.70:* The following eight-symbol alphabet with associated relative
;; frequencies was designed to efficiently encode the lyrics of 1950s rock
;; songs.  (Note that the "symbols" of an "alphabet" need not be individual
;; letters.)

;;      A     2 NA   16
;;      BOOM  1 SHA  3
;;      GET   2 YIP  9
;;      JOB   2 WAH  1

;; Use `generate-huffman-tree' (*note Exercise 2-69::) to generate a
;; corresponding Huffman tree, and use `encode' (*note Exercise 2-68::) to
;; encode the following message:

;;      Get a job

;;      Sha na na na na na na na na

;;      Get a job

;;      Sha na na na na na na na na

;;      Wah yip yip yip yip yip yip yip yip yip

;;      Sha boom

;; How many bits are required for the encoding?  What is the smallest number of
;; bits that would be needed to encode this song if we used a fixed-length code
;; for the eight-symbol alphabet?

(defparameter *lynrics-of-rock* '((A 2) (NA 16) (BOOM 1) (SHA  3)
                                  (GET 2) (YIP 9) (JOB 2) (WAH  1)))

(generate-huffman-tree *lynrics-of-rock*)

(huffman-encode '(Get a job                           ; (* 2 7) => 14
                  Sha na na na na na na na na         ; (* 2 (+ 16 3)) => 38
                  Get a job
                  Sha na na na na na na na na
                  Wah yip yip yip yip yip yip yip yip yip ; 27
                  Sha boom)                               ; 7
                (generate-huffman-tree *lynrics-of-rock*))
;; (/ 84 (* 8 (+ 14 38 27 7))) 21/172

;; *Exercise 2.71:* Suppose we have a Huffman tree for an alphabet of n symbols,
;; and that the relative frequencies of the symbols are 1, 2, 4, ..., 2^(n-1).
;; Sketch the tree for n=5 for n=10.  In such a tree (for general n) how may
;; bits are required to encode the most frequent symbol?  the least frequent
;; symbol?
(defun test-huffman-freq (n)
  (let ((tree
         (generate-huffman-tree (accumulate #'cons nil #'(lambda (x)
                                                           (let ((e (expt 2 x)))
                                                             (list x e)))
                                            0 #'1+ (1- n)))))
    (list (length (huffman-encode (list (1- n)) tree))
          (length (huffman-encode '(0) tree)))))

; (test-huffman-freq 5) => (1 4)
; (test-huffman-freq 10) => (1 9)
