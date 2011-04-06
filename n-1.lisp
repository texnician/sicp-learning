(defparameter *raidx* 10)

(defun n-e (n)
  (if (< n *raidx*)
      0
      (1+ (n-e (floor (/ n *raidx*))))))

(defun left-most (n)
  (floor (/ n (expt *raidx* (n-e n)))))

(defun n-1 (n)
  (let* ((e (n-e n))
         (lm (left-most n))
         (k (expt *raidx* e))
         (rmd (- n (* lm k))))
    (cond ((= n 0) 0)
          ((< n *raidx*) 1)
          ((= lm 1)
           (+ (1+ rmd) (n-1 rmd) (n-1 (1- k))))
          (t (+ k (* lm (n-1 (1- k)))
                (n-1 rmd))))))

(defun max-n-1-n (n)
  (labels ((iter (low high)
             (if (> low high)
                 0
                 (let ((middle (floor (/ (+ low high) 2))))
                   (if (= (n-1 middle) middle)
                       (max middle (iter (1+ middle) high))
                       (let ((x (iter (1+ middle) high)))
                         (if (> x 0)
                             x
                             (iter low (1- middle)))))))))
    (iter 1 n)))

(max-n-1-n (expt 2 20))

;(max-n-1-n)
