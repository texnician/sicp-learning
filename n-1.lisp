(defparameter *radix* 10)

(defun n-e (n)
  (if (< n *radix*)
      0
      (1+ (n-e (floor (/ n *radix*))))))

(defun left-most (n)
  (floor (/ n (expt *radix* (n-e n)))))


(defun n-1 (n)
  (let* ((e (n-e n))
         (lm (left-most n))
         (k (expt *radix* e))
         (rmd (mod n k)))
    (cond ((= n 0) 0)
          ((< n *radix*) 1)
          ((= (1- (expt *radix* (1+ e))) n)
           (* (1+ e) (expt *radix* e)))
          ((= lm 1)
           (+ (1+ rmd) (n-1 (1- k)) (n-1 rmd)))
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

(defun min-n-1-n ()
  (labels ((iter (n)
             (if (= (n-1 n) n)
                 n
                 (iter (1+ n)))))
    (iter 2)))

(defun max-n-1-n-2 (n)
  (if (= (n-1 n) n)
      n
      (max-n-1-n-2 (1- n))))

;; (max-n-1-n-2 (expt 2 32))
;; (max-n-1-n 100)
;; (min-n-1-n)
;; (untrace n-1)
;; (- (n-1 1234) (n-1 9))
;; (- (n-1 1998) (n-1 999))
;; (trace n-1)
;; (step (n-1 199983))
;; (time (n-1 (expt 2 32)))
;; (> (n-1 2) 2)
;; (n-1 199999)
;; (> (n-1 199983) 199983)
;; (> (n-1 199991) 199991)
;; (> (n-1 199992) 199992)
;; (> (n-1 200001) 200001)
;; (> (expt 2 21) (n-1 (expt 2 21)))
;; ;(max-n-1-n (expt 2 20))
;; (error "error")
;(min-n-1-n)
(expt 2 32)
(n-1 1111111110)
(n-1 (expt 2 31))
