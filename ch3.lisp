(load "ch2.lisp")

(in-package :sicp)

;; *Exercise 3.1:* An "accumulator" is a procedure that is called repeatedly
;; with a single numeric argument and accumulates its arguments into a sum.
;; Each time it is called, it returns the currently accumulated sum.  Write a
;; procedure `make-accumulator' that generates accumulators, each maintaining an
;; independent sum.  The input to `make-accumulator' should specify the initial
;; value of the sum; for example

;;      (define A (make-accumulator 5))

;;      (A 10)
;;      15

;;      (A 10)
;;      25

(defun make-accumulator (base)
  (lambda (x)
    (setf base (+ x base))
    base))

(defparameter *acc-a* (make-accumulator 5))

;; *Exercise 3.2:* In software-testing applications, it is useful to be able to
;; count the number of times a given procedure is called during the course of a
;; computation.  Write a procedure `make-monitored' that takes as input a
;; procedure, `f', that itself takes one input.  The result returned by
;; `make-monitored' is a third procedure, say `mf', that keeps track of the
;; number of times it has been called by maintaining an internal counter.  If
;; the input to `mf' is the special symbol `how-many-calls?', then `mf' returns
;; the value of the counter.  If the input is the special symbol `reset-count',
;; then `mf' resets the counter to zero.  For any other input, `mf' returns the
;; result of calling `f' on that input and increments the counter.  For
;; instance, we could make a monitored version of the `sqrt' procedure:

;;      (define s (make-monitored sqrt))

;;      (s 100)
;;      10

;;      (s 'how-many-calls?)
;;      1

(defun make-monitored (f)
  (let ((count 0))
    (lambda (&rest args)
      (cond ((eq (car args) 'how-many-calls?) count)
            ((eq (car args) 'reset-count) (setf count 0) count)
            (t (setf count (1+ count))
               (apply f args))))))

(defparameter *mf* (make-monitored #'sqrt))
(funcall *mf* 100)
(funcall *mf* 'how-many-calls?)
(funcall *mf* 'reset-count)

;; *Exercise 3.3:* Modify the `make-account' procedure so that it creates
;; password-protected accounts.  That is, `make-account' should take a symbol as
;; an additional argument, as in

;;      (define acc (make-account 100 'secret-password))

;; The resulting account object should process a request only if it is
;; accompanied by the password with which the account was created, and should
;; otherwise return a complaint:

;;      ((acc 'secret-password 'withdraw) 40)
;;      60

;;      ((acc 'some-other-password 'deposit) 50)
;;      "Incorrect password"
(defun make-account (balance password)
  (lambda (pwd op)
    (cond ((not (eq pwd password))
           (error "Incorrect password"))
          ((eq op 'make-joint) t)
          (t 
           (let ((f (cond ((eq op 'withdraw) #'-)
                          ((eq op 'deposit) #'+))))
             (lambda (x)
               (setf balance (funcall f balance x))
               balance))))))

(defparameter *test-account* (make-account 100 'not-secret-password))
(funcall (funcall *test-account* 'not-secret-password 'withdraw) 40)
(funcall (funcall *test-account* 'not-secret-password 'deposit) 50)

;; *Exercise 3.4:* Modify the `make-account' procedure of *note Exercise 3-3::
;; by adding another local state variable so that, if an account is accessed
;; more than seven consecutive times with an incorrect password, it invokes the
;; procedure `call-the-cops'.

(defun make-account-2 (balance password)
  (let ((password-count 0))
    (lambda (pwd op)
      (if (not (eq pwd password))
          (progn (setf password-count (1+ password-count))
                 (if (> password-count 7)
                     (error "Call the cops")
                     (error "Incorrect password")))
          (progn (setf password-count 0)
                 (let ((f (cond ((eq op 'withdraw) #'-)
                                ((eq op 'deposit) #'+))))
                   (lambda (x)
                     (setf balance (funcall f balance x))
                     balance)))))))

(defparameter *test-account-2* (make-account-2 100 'not-secret-password))
(funcall (funcall *test-account-2* 'not-secret-password 'withdraw) 50)

;; *Exercise 3.5:* "Monte Carlo integration" is a method of estimating definite
;; integrals by means of Monte Carlo simulation.  Consider computing the area of
;; a region of space described by a predicate P(x, y) that is true for points
;; (x, y) in the region and false for points not in the region.  For example,
;; the region contained within a circle of radius 3 centered at (5, 7) is
;; described by the predicate that tests whether (x - 5)^2 + (y - 7)^2 <= 3^2.
;; To estimate the area of the region described by such a predicate, begin by
;; choosing a rectangle that contains the region.  For example, a rectangle with
;; diagonally opposite corners at (2, 4) and (8, 10) contains the circle above.
;; The desired integral is the area of that portion of the rectangle that lies
;; in the region.  We can estimate the integral by picking, at random, points
;; (x,y) that lie in the rectangle, and testing P(x, y) for each point to
;; determine whether the point lies in the region.  If we try this with many
;; points, then the fraction of points that fall in the region should give an
;; estimate of the proportion of the rectangle that lies in the region.  Hence,
;; multiplying this fraction by the area of the entire rectangle should produce
;; an estimate of the integral.

;; Implement Monte Carlo integration as a procedure `estimate-integral' that
;; takes as arguments a predicate `P', upper and lower bounds `x1', `x2', `y1',
;; and `y2' for the rectangle, and the number of trials to perform in order to
;; produce the estimate.  Your procedure should use the same `monte-carlo'
;; procedure that was used above to estimate [pi].  Use your `estimate-integral'
;; to produce an estimate of [pi] by measuring the area of a unit circle.

;; You will find it useful to have a procedure that returns a number chosen at
;; random from a given range.  The following `random-in-range' procedure
;; implements this in terms of the `random' procedure used in section see 1-2-6,
;; which returns a nonnegative number less than its input.(3)

;;      (define (random-in-range low high)
;;        (let ((range (- high low)))
;;          (+ low (random range))))

(defun random-in-range (low high)
  (let ((random-range 100000l0)
        (range (- high low)))
    (+ low (* range (/ (random random-range) (1- random-range))))))

(defun mote-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
             (cond ((= trials-remaining 0)
                    (* 1.0l0 (/ trials-passed trials)))
                   ((funcall experiment)
                    (iter (1- trials-remaining) (1+ trials-passed)))
                   (t (iter (1- trials-remaining) trials-passed)))))
    (iter trials 0)))

(defun estimate-integral (pred x1 x2 y1 y2)
    (lambda (trials)
      (mote-carlo trials #'(lambda ()
                             (funcall pred (random-in-range x1 x2) (random-in-range y1 y2))))))

(defun mote-carlo-pi (trials)
  (let* ((ox 0.0l0)
         (oy 0.0l0)
         (r 3.0l0)
         (expt-r (expt r 2))
         (x1 (- ox r))
         (x2 (+ ox r))
         (y1 (- oy r))
         (y2 (+ oy r)))
    (/ (* (* (- x2 x1) (- y2 y1))
          (funcall (estimate-integral #'(lambda (x y)
                                          (<= (+ (expt (- x ox) 2) (expt (- y oy) 2)) expt-r))
                                      x1 x2 y1 y2) trials))
       expt-r)))

;(time (mote-carlo-pi 50000000))

;; *Exercise 3.7:* Consider the bank account objects created by `make-account',
;; with the password modification described in *note Exercise 3-3::.  Suppose
;; that our banking system requires the ability to make joint accounts.  Define
;; a procedure `make-joint' that accomplishes this.  `Make-joint' should take
;; three arguments.  The first is a password-protected account.  The second
;; argument must match the password with which the account was defined in order
;; for the `make-joint' operation to proceed.  The third argument is a new
;; password.  `Make-joint' is to create an additional access to the original
;; account using the new password.  For example, if `peter-acc' is a bank
;; account with password `open-sesame', then

;;      (define paul-acc
;;        (make-joint peter-acc 'open-sesame 'rosebud))

;; will allow one to make transactions on `peter-acc' using the name `paul-acc'
;; and the password `rosebud'.  You may wish to modify your solution to see
;; Exercise 3-3 to accommodate this new feature

(defun make-joint (account password other-password)
  (if (funcall account password 'make-joint)
      (lambda (pwd op)
        (if (eq pwd other-password)
            (funcall account password op)
            (error "Incorrect joint account password")))
      nil))

(defparameter *paul-acc*
  (make-joint *test-account* 'not-secret-password 'rosebud))

;(funcall (funcall *test-account* 'rosebud 'withdraw) 10)

;(funcall (funcall *paul-acc* 'rosebud 'deposit) 200)

;; *Exercise 3.8:* When we defined the evaluation model in section *note 1-1-3,
;; we said that the first step in evaluating an expression is to evaluate its
;; subexpressions.  But we never specified the order in which the subexpressions
;; should be evaluated (e.g., left to right or right to left).  When we
;; introduce assignment, the order in which the arguments to a procedure are
;; evaluated can make a difference to the result. Define a simple procedure `f'
;; such that evaluating `(+ (f 0) (f 1))' will return 0 if the arguments to `+'
;; are evaluated from left to right but will return 1 if the arguments are
;; evaluated from right to left.
(defun make-simple-f ()
  (let ((state 0))
    (lambda (x)
      (let ((prev state))
        (if (not (= state x))
            (progn (setf state x)
                   prev)
            0)))))

(let ((f (make-simple-f)))
  (assert (= (+ (funcall f 0) (funcall f 1)) 0))
  (assert (= (+ (funcall f 1) (funcall f 0)) 1)))
