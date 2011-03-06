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