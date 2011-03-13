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
                 (reverse-iter (append (list (car l)) acc) (cdr l)))))
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