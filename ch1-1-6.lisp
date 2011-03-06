;; Exercise 1.1

10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(setf *a* 3)

(setf *b* (+ *a* 1))

(+ *a* *b* (* *a* *b*))

(eq *a* *b*)

(if (and (> *b* *a*) (< *b* (* *a* *b*)))
    *b*
    *a*)

(cond ((eq *a* 4) 6)
      ((eq *b* 4) (+ 6 7 *a*))
      (t 25))

(+ 2 (if (> *b* *a*)
         *b* *a*))

(* (cond ((> *a* *b*) *a*)
         ((> *b* *a*) *b*)
         (t -1))
   (+ *a* 1))

;; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3

(defun square (x)
  (* x x))

(defun max1 (x y)
  (if (> x y)
      x
      y))

(defun min1 (x y)
  (if (< x y)
      x
      y))

(defun sum-of-square (x y)
  (+ (square x) (square y)))

(defun sum-of-two-larger-square (x y z)
  (cond ((< x (min1 y z)) (sum-of-square y z))
        (t (sum-of-square x (max1 y z)))))

(sum-of-two-larger-square 1 2 3)
(sum-of-two-larger-square 1 3 2)
(sum-of-two-larger-square 2 1 3)
(sum-of-two-larger-square 2 3 1)
(sum-of-two-larger-square 3 1 2)
(sum-of-two-larger-square 3 2 1)

;; Exercise 1.4
(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) #'+ #'-) a b))

(a-plus-abs-b 1 2)
(a-plus-abs-b 1 -2)

(/ 3 2)

;; Exercise 1.7
(defun sqrt-iter (guess x)
  (format t "~f " guess)
  (if (good-enoughp guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enoughp (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun sqrt1 (x)
  (sqrt-iter 1.0 x))

(defun good-enough2p (diff)
  (< (abs diff) 0.001))


(defun sqrt-iter2 (guess x)
  (format t "~f " guess)
  (if (good-enough2p (- (improve guess x) guess))
      guess
      (sqrt-iter2 (improve guess x)
                  x)))

(defun sqrt2 (x)
  (sqrt-iter2 1.0 x))

;(sqrt1 100000)

;(sqrt2 10000)
