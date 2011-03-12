;; Load chapter 1
(load "ch1-1-6")

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