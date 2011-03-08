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

;; Exercise 1.9
(defun add1 (a b)
  (if (eq a 0)
      b
      (1+ (add1 (1- a) b))))

(add1 4 5)
(1+ (add1 3 5))
(1+ (1+ (add1 2 5)))
(1+ (1+ (1+ (add1 1 5))))
(1+ (1+ (1+ (1+ (add1 0 5)))))
(1+ (1+ (1+ (1+ 5))))
(1+ (1+ (1+ 6)))
(1+ (1+ 7))
(1+ 8)
9

(defun add2 (a b)
  (if (eq a 0)
      b
      (add2 (1- a) (1+ b))))
(add2 4 5)
(add2 3 6)
(add2 2 7)
(add2 1 8)
(add2 0 9)
9

;; Exercise 1.10
(defun A (x y)
  (cond ((eq y 0) 0)
        ((eq x 0) (* 2 y))
        ((eq y 1) 2)
        (t (A (1- x)
              (A x (1- y))))))

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 (A 0 (A 0 8192)))
(A 0 (A 0 16384))
(A 0 32768)
65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 4)

;; 2n
(defun A-f (n)
  (A 0 n))                              

;; 2^n
(defun A-g (n)
  (A 1 n))

;; 2_1^2_2^2_3..2_n
(defun A-h (n)
  (A 2 n))

;; Exercise 1.11
(defun fb3 (n)
  (if (< n 3)
      n
      (+ (fb3 (- n 1))
         (* 2 (fb3 (- n 2)))
         (* 3 (fb3 (- n 3))))))
(t
(defun fb3-2 (n)
  (fb3-iter 0 1 2 n))

(defun fb3-iter (a b c count)
  (if (eq count 0)
       a
       (fb3-iter b c (+ c (* 2 b) (* 3 a)) (1- count))))

;; Exercise 1.12
(defun pascal-triangle (n k)
  (cond ((eq k 1) 1)
        ((eq k n) 1)
        (t (+ (pascal-triangle (1- n) (1- k))
              (pascal-triangle (1- n) k)))))

;; Exercise 1.16
(defun expr-fast (b n)
  (expr-fast-iter 1 b n))

(defun expr-fast-iter (a b n)
  (if (eq n 0)
      a
      (let ((acc (if (evenp n)
                     a
                     (* a b)))
            (h (if (evenp n)
                   (/ n 2)
                   (/ (1- n) 2))))
        (expr-fast-iter acc (square b) h))))
