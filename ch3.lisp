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

;(time (mote-carlo-pi 5000000))

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

;(step (test-f '(1 2 3 4 5)))
(defun nappend (x y)
  (setf (cdr (last-pair x)) y)
  x)

(defun last-pair (x)
  (if (null (cdr x))
      x
      (last-pair (cdr x))))

(defparameter *nx* '(a b))
(defparameter *ny* '(c d))
(defparameter *nz* (append *nx* *ny*))
(defparameter *nw* (nappend *nx* *ny*))

;; *Exercise 3.13:* Consider the following `make-cycle' procedure, which uses
;; the `last-pair' procedure defined in *note Exercise 3-12:::

;;      (define (make-cycle x)
;;        (set-cdr! (last-pair x) x)
;;        x)

;; Draw a box-and-pointer diagram that shows the structure `z' created by

;;      (define z (make-cycle (list 'a 'b 'c)))

;; What happens if we try to compute `(last-pair z)'?
(defun make-cycle (x)
  (setf (cdr (last-pair x)) x)
  x)

;(defparameter *nz* (make-cycle '(a b c)))

;; *Exercise 3.14:* The following procedure is quite useful, although obscure:

;;      (define (mystery x)
;;        (define (loop x y)
;;          (if (null? x)
;;              y
;;              (let ((temp (cdr x)))
;;                (set-cdr! x y)
;;                (loop temp x))))
;;        (loop x '()))

;; `Loop' uses the "temporary" variable `temp' to hold the old value of the
;; `cdr' of `x', since the `set-cdr!'  on the next line destroys the `cdr'.
;; Explain what `mystery' does in general.  Suppose `v' is defined by `(define v
;; (list 'a 'b 'c 'd))'. Draw the box-and-pointer diagram that represents the
;; list to which `v' is bound.  Suppose that we now evaluate `(define w (mystery
;; v))'. Draw box-and-pointer diagrams that show the structures `v' and `w'
;; after evaluating this expression.  What would be printed as the values of `v'
;; and `w'?
(defun mystery (x)
  (labels ((iter (x y)
             (if (null x)
                 y
                 (let ((temp (cdr x)))
                   (setf (cdr x) y)
                   (iter temp x)))))
    (iter x nil)))

;(defparameter *v* '(a b c d))
;(defparameter *w* (mystery *v*))

(defparameter *x* '(a b))
(defparameter *z1* (cons *x* *x*))
(defparameter *z2* (cons '(a b) '(a b)))

;; *Exercise 3.16:* Ben Bitdiddle decides to write a procedure to count the
;; number of pairs in any list structure.  "It's easy," he reasons.  "The number
;; of pairs in any structure is the number in the `car' plus the number in the
;; `cdr' plus one more to count the current pair."  So Ben writes the following
;; procedure:

;;      (define (count-pairs x)
;;        (if (not (pair? x))
;;            0
;;            (+ (count-pairs (car x))
;;               (count-pairs (cdr x))
;;               1)))

;; Show that this procedure is not correct.  In particular, draw box-and-pointer
;; diagrams representing list structures made up of exactly three pairs for
;; which Ben's procedure would return 3; return 4; return 7; never return at
;; all.
(defun count-pairs (x)
  (if (not (consp x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs *x*)
(count-pairs *z2*)
(count-pairs *z1*)

;; *Exercise 3.17:* Devise a correct version of the `count-pairs' procedure of
;; *note Exercise 3-16:: that returns the number of distinct pairs in any
;; structure.  (Hint: Traverse the structure, maintaining an auxiliary data
;; structure that is used to keep track of which pairs have already been
;; counted.)
(defun count-pairs-1 (x)
  (let ((marks nil))
    (labels ((iter (x)
               (if (not (consp x))
                   0
                   (+ (iter (car x))
                      (iter (cdr x))
                      (if (member x marks)
                          0
                          (progn (setf marks (cons x marks))
                                 1))))))
      (iter x))))

(defun count-pairs-2 (x)
  (labels ((iter (acc x)
             (if (not (consp x))
                 acc
                 (if (member x acc)
                     (iter (iter acc (car x)) (cdr x))
                     (iter (iter (cons x acc) (car x)) (cdr x))))))
    (length (iter nil x))))


(count-pairs-2 *x*)
(count-pairs-2 *z2*)
(count-pairs-2 *z1*)

;; *Exercise 3.18:* Write a procedure that examines a list and determines
;; whether it contains a cycle, that is, whether a program that tried to find
;; the end of the list by taking successive `cdr's would go into an infinite
;; loop.  *note Exercise 3-13:: constructed such lists.
(defun cyclep (lst)
  (labels ((iter (acc x)
             (cond ((null x) nil)
                   ((member (car x) acc) t)
                   (t (iter (cons (car x) acc) (cdr x))))))
    (iter nil lst)))

(defun make-p-cycle (handler cycle)
  (setf (cdr (last-pair handler)) (make-cycle cycle))
  handler)

(cyclep (make-p-cycle '(f g h) '(a b c d e)))

;; *Exercise 3.19:* Redo *note Exercise 3-18:: using an algorithm that takes
;; only a constant amount of space.  (This requires a very clever idea.)
;(defun smart-cyclep (lst)
(defun smart-cyclep (lst)
  (labels ((iter (slow fast)
             (cond ((or (null slow) (null fast) (null (cdr fast)))
                    nil)
                   ((eq slow fast) (car slow))
                   (t (iter (cdr slow) (cddr fast))))))
    (iter lst (cddr lst))))

;(smart-cyclep (make-p-cycle '(f g h i j k) '(1 2 3 a b c d f l)))
;(smart-cyclep (make-cycle '(a)))

;; *Exercise 3.21:* Ben Bitdiddle decides to test the queue implementation
;; described above.  He types in the procedures to the Lisp interpreter and
;; proceeds to try them out:

;;      (define q1 (make-queue))

;;      (insert-queue! q1 'a)
;;      ((a) a)

;;      (insert-queue! q1 'b)
;;      ((a b) b)

;;      (delete-queue! q1)
;;      ((b) b)

;;      (delete-queue! q1)
;;      (() b)

;; "It's all wrong!" he complains.  "The interpreter's response shows that the
;; last item is inserted into the queue twice.  And when I delete both items,
;; the second `b' is still there, so the queue isn't empty, even though it's
;; supposed to be."  Eva Lu Ator suggests that Ben has misunderstood what is
;; happening.  "It's not that the items are going into the queue twice," she
;; explains.  "It's just that the standard Lisp printer doesn't know how to make
;; sense of the queue representation.  If you want to see the queue printed
;; correctly, you'll have to define your own print procedure for queues."
;; Explain what Eva Lu is talking about.  In particular, show why Ben's examples
;; produce the printed results that they do.  Define a procedure `print-queue'
;; that takes a queue as input and prints the sequence of items in the queue.
(defmacro set-car! (form value)
  `(setf (car ,form) ,value))

(defmacro set-cdr! (form value)
  `(setf (cdr ,form) ,value))

(defun make-queue ()
  (cons '() '()))

(defun front-ptr (queue)
  (car queue))

(defun rear-ptr (queue)
  (cdr queue))

(defun set-front-ptr! (queue item)
  (set-car! queue item))

(defun set-rear-ptr! (queue item)
  (set-cdr! queue item))

(defun empty-queuep (queue)
  (null (front-ptr queue)))

(defun front-queue (queue)
  (if (empty-queuep queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(defun insert-queue! (queue item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-queuep queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair))
          (t (set-cdr! (rear-ptr queue) new-pair)
             (set-rear-ptr! queue new-pair)))
    queue))

(defun delete-queue! (queue)
  (cond ((empty-queuep queue)
         (error "DELETE! called with an empty queue" queue))
        (t (set-front-ptr! queue (cdr (front-ptr queue)))
           queue)))

(defun print-queue (queue)
  (cond ((empty-queuep queue)
         (print nil))
        (t (print (front-ptr queue)))))

(defparameter *q1* (make-queue))
(insert-queue! *q1* 'a)
(insert-queue! *q1* 'b)
(delete-queue! *q1*)
(delete-queue! *q1*)

(print-queue *q1*)
