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