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

;; *Exercise 3.22:* Instead of representing a queue as a pair of pointers, we
;; can build a queue as a procedure with local state.  The local state will
;; consist of pointers to the beginning and the end of an ordinary list.  Thus,
;; the `make-queue' procedure will have the form

;;      (define (make-queue)
;;        (let ((front-ptr ... )
;;              (rear-ptr ... ))
;;          <DEFINITIONS OF INTERNAL PROCEDURES>
;;          (define (dispatch m) ...)
;;          dispatch))

;; Complete the definition of `make-queue' and provide implementations of the
;; queue operations using this representation.
(defun make-queue-2 ()
  (let ((front-ptr '())
        (rear-ptr '()))
    (labels ((emptyp ()
               (null front-ptr))
             (front ()
               (if (emptyp)
                   (error "FRONT called with an empty queue")
                   (car front-ptr)))
             (insert! (item)
               (let ((new-pair `(,item)))
                 (cond ((emptyp)
                        (setf front-ptr new-pair)
                        (setf rear-ptr new-pair))
                       (t (set-cdr! rear-ptr new-pair)
                          (setf rear-ptr new-pair)))))
             (delete! ()
               (cond ((emptyp)
                      (error "DELETE! called with an emtpy queue"))
                     (t (setf front-ptr (cdr front-ptr)))))
             (print-queue ()
               (print front-ptr))
             (dispatch (m)
               (cond ((eq m 'emptyp) #'emptyp)
                     ((eq m 'front) #'front)
                     ((eq m 'insert!) #'insert!)
                     ((eq m 'delete!) #'delete!)
                     ((eq m 'print) #'print-queue)
                     (t (error "Unkown message" m)))))
      #'dispatch)))

(defmacro message-passing (object msg &body body)
    `(funcall (funcall ,object ',msg) ,@body))

(defparameter *q2* (make-queue-2))
(message-passing *q2* emptyp)
(message-passing *q2* insert! 'a)
(message-passing *q2* insert! 'b)
(message-passing *q2* insert! 'c)
(message-passing *q2* front)
(message-passing *q2* delete!)
(message-passing *q2* delete!)
(message-passing *q2* delete!)
(message-passing *q2* print)

;; *Exercise 3.23:* A "deque" ("double-ended queue") is a sequence in which
;; items can be inserted and deleted at either the front or the rear.
;; Operations on deques are the constructor `make-deque', the predicate
;; `empty-deque?', selectors `front-deque' and `rear-deque', and mutators
;; `front-insert-deque!', `rear-insert-deque!', `front-delete-deque!', and
;; `rear-delete-deque!'.  Show how to represent deques using pairs, and give
;; implementations of the operations.(2) All operations should be accomplished
;; in [theta](1) steps.
(defun make-deque-node (item)
  (cons item (cons nil nil)))

(defun get-deque-node-item (node)
  (car node))

(defun deque-node-next (node)
  (cddr node))

(defun deque-node-prev (node)
  (cadr node))

(defun set-deque-node-next! (node next-node)
  (setf (cddr node) next-node)
  (get-deque-node-item node))

(defun set-deque-node-prev! (node prev-node)
  (setf (cadr node) prev-node)
  (get-deque-node-item node))

(defun link-deque-node (node1 node2)
  (set-deque-node-next! node1 node2)
  (set-deque-node-prev! node2 node1))

(defun unlink-deque-node (node1 node2)
  (set-deque-node-next! node1 nil)
  (set-deque-node-prev! node2 nil))

(defun deque2lst (head)
  (let ((lst nil))
    (if (null head)
        lst
        (cons (get-deque-node-item head) (deque2lst (deque-node-next head))))))

(defun deque2lst-reverse (head)
  (let ((lst nil))
    (if (null head)
        lst
        (cons (get-deque-node-item head) (deque2lst-reverse (deque-node-prev head))))))

(defun make-deque ()
  (cons nil nil))

(defun empty-dequep (deque)
  (null (front-ptr deque)))

(defun front-deque (deque)
  (get-deque-node-item (front-ptr deque)))

(defun rear-deque (deque)
  (get-deque-node-item (rear-ptr deque)))

(defun front-insert-deque! (deque item)
  (let ((new-pair (make-deque-node item)))
    (cond ((empty-dequep deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (t (link-deque-node new-pair (front-ptr deque))
             (set-front-ptr! deque new-pair)))
    (deque2lst (front-ptr deque))))

(defun rear-insert-deque! (deque item)
  (let ((new-pair (make-deque-node item)))
    (cond ((empty-dequep deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (t (link-deque-node (rear-ptr deque) new-pair)
             (set-rear-ptr! deque new-pair)))
    (deque2lst (front-ptr deque))))

(defun front-delete-deque! (deque)
  (cond ((empty-dequep deque)
         (error "FRONT-DELETE-DEQUE called with empty deque" deque))
        (t (let ((new-head (deque-node-next (front-ptr deque))))
             (if (null new-head)
                 (set-rear-ptr! deque nil)
                 (unlink-deque-node (front-ptr deque) new-head))
             (set-front-ptr! deque new-head)
             (deque2lst (front-ptr deque))))))

(defun rear-delete-deque! (deque)
  (cond ((empty-dequep deque)
         (error "REAR-DELETE-DEQUE called with empty deque" deque))
        (t (let ((new-tail (deque-node-prev (rear-ptr deque))))
             (if (null new-tail)
                 (set-front-ptr! deque nil)
                 (unlink-deque-node new-tail (rear-ptr deque)))
             (set-rear-ptr! deque new-tail)
             (deque2lst (front-ptr deque))))))

(defun print-deque (deque)
  (print (deque2lst (front-ptr deque))))

(defun print-deque-reverse (deque)
  (print (deque2lst-reverse (rear-ptr deque))))

(defparameter *dq1*  (make-deque))
(front-insert-deque! *dq1* 'a)
(front-insert-deque! *dq1* 'b)
(front-insert-deque! *dq1* 'c)
(rear-insert-deque! *dq1* '3)
(rear-insert-deque! *dq1* '2)
(rear-insert-deque! *dq1* '1)
(front-delete-deque! *dq1*)
(rear-delete-deque! *dq1*)
(print-deque *dq1*)
(print-deque-reverse *dq1*)

;; *Exercise 3.28:* Define an or-gate as a primitive function box. Your
;; *`or-gate' constructor should be similar to `and-gate'.

(defun inverter (input output)
  (labels ((invert-input ()
             (let ((new-value (logical-not (get-signal input))))
               (after-delay *inverter-delay* #'(lambda ()
                                                 (set-signal! output new-value))))))
    (add-action! input #'invert-input)
    'ok))

(defun logical-not (sig)
  (cond ((= sig 0) 1)
        ((= sig 1) 0)
        (t (error "Invalid signal" sig))))

(defun and-gate (a1 a2 output)
  (labels ((and-signal-handler ()
             (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
               (after-delay *and-gate-delay* #'(lambda ()
                                                 (set-signal! output new-value))))))
    (add-action! a1 #'and-signal-handler)
    (add-action! a2 #'and-signal-handler)
    'ok))

(defun logical-and (sig1 sig2)
  (cond ((and (= 1 sig1) (= 1 sig2)) 1)
        ((and (= 1 sig1) (= 0 sig2)) 0)
        ((and (= 0 sig1) (= 1 sig2)) 0)
        ((and (= 0 sig1) (= 0 sig2)) 0)
        (t (error "Invalid signal" sig1 sig2))))

(defun or-gate (a1 a2 output)
  (labels ((or-signal-handler ()
             (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
               (after-delay *or-gate-delay* #'(lambda ()
                                                (set-signal! output new-value))))))
    (add-action! a1 #'or-signal-handler)
    (add-action! a2 #'or-signal-handler)
    'ok))

(defun logical-or (sig1 sig2)
  (cond ((and (= 1 sig1) (= 1 sig2)) 1)
        ((and (= 1 sig1) (= 0 sig2)) 1)
        ((and (= 0 sig1) (= 1 sig2)) 1)
        ((and (= 0 sig1) (= 0 sig2)) 0)
        (t (error "Invalid signal" sig1 sig2))))

;; *Exercise 3.29:* Another way to construct an or-gate is as a compound digital
;; logic device, built from and-gates and inverters.  Define a procedure
;; `or-gate' that accomplishes this.  What is the delay time of the or-gate in
;; terms of `and-gate-delay' and `inverter-delay'?

;;; A*B + A*~B + ~A*B
(defun or-gate2 (a1 a2 output)
  (let ((c (make-wire))
        (d (make-wire)))
    (and-gate a1 a2 output)
    (inverter b c)
    (and-gate a c output)
    (inverter a d)
    (and-gate d b output)
    'ok))
;;delay =  *and-gate-delay* + *inverter-delay*
 
;; *Exercise 3.30:* *note Figure 3-27:: shows a "ripple-carry adder" formed by
;; stringing together n full-adders.  This is the simplest form of parallel
;; adder for adding two n-bit binary numbers.  The inputs A_1, A_2, A_3, ...,
;; A_n and B_1, B_2, B_3, ..., B_n are the two binary numbers to be added (each
;; A_k and B_k is a 0 or a 1).  The circuit generates S_1, S_2, S_3, ..., S_n,
;; the n bits of the sum, and C, the carry from the addition.  Write a procedure
;; `ripple-carry-adder' that generates this circuit.  The procedure should take
;; as arguments three lists of n wires each--the A_k, the B_k, and the S_k--and
;; also another wire C.  The major drawback of the ripple-carry adder is the
;; need to wait for the carry signals to propagate.  What is the delay needed to
;; obtain the complete output from an n-bit ripple-carry adder, expressed in
;; terms of the delays for and-gates, or-gates, and inverters?

;; *Figure 3.27:* A ripple-carry adder for n-bit numbers.

;;         :                                              :   :
;;         : A_1 B_1   C_1   A_2 B_2   C_2   A_3 B_3   C_3:   : A_n B_n C_n=0
;;         :  |   |   +---+   |   |   +---+   |   |   +-----  :  |   |   +-
;;         |  |   |   |   |   |   |   |   |   |   |   |   :   :  |   |   |
;;         : ++---+---++  |  ++---+---++  |  ++---+---++  :   : ++---+---++
;;         : |   FA    |  |  |   FA    |  |  |   FA    |  :   : |   FA    |
;;         : +--+---+--+  |  +--+---+--+  |  +--+---+--+  :   : +--+---+--+
;;         :    |   |     |     |   |     |     |   |     :   :    |   |
;;      C ------+   |     +-----+   |     +-----+   |     :  ------+   |
;;         :        |       C_1     |       C_2     |     :   : C_(n-1)|
;;         :        |               |               |     :   :        |
;;                 S_1             S_2             S_3                S_n
(defun half-adder (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(defun ripple-carry-adder (ak bk c sk)
  (labels ((iter-builder (a-in b-in c-in s-out)
             (if (null a-in)
                 'ok
                 (let ((a (car a-in))
                       (b (car b-in))
                       (sum (car s-out))
                       (carry-out (make-wire)))
                   (full-adder a b c-in sum carry-out)
                   (iter-builder (cdr a-in) (cdr b-in) carry-out (cdr s-out))))))
    (if (and (= (length ak) (length bk))
              (= (length bk) (length sk)))
        (iter-builder (ak bk c sk))
        (error "Invalid input" ak bk c sk))))

;; *Exercise 3.31:* The internal procedure `accept-action-procedure!'  defined
;; in `make-wire' specifies that when a new action procedure is added to a wire,
;; the procedure is immediately run.  Explain why this initialization is
;; necessary.  In particular, trace through the half-adder example in the
;; paragraphs above and say how the system's response would differ if we had
;; defined `accept-action-procedure!' as

;;      (define (accept-action-procedure! proc)
;;        (set! action-procedures (cons proc action-procedures)))

(defun make-wire ()
  (let ((signal-value 0)
        (action-procedures '()))
    (labels ((set-my-signal! (new-value)
               (if (not (= signal-value new-value))
                   (progn (setf signal-value new-value)
                          (call-each action-procedures)))
               'done)
             (accept-action-procedure! (proc)
               (setf action-procedures (cons proc action-procedures))
               (funcall proc))
             (dispatch (m)
               (cond ((eq m 'get-signal) signal-value)
                     ((eq m 'set-signal!) #'set-my-signal!)
                     ((eq m 'add-action!) #'accept-action-procedure!)
                     (t (error "Unknown operation -- WIRE" m)))))
      #'dispatch)))

(defun call-each (procedures)
  (if (null procedures)
      'done
      (progn (funcall (car procedures))
             (call-each (cdr procedures)))))

(defun get-signal (wire)
  (funcall wire 'get-signal))

(defun set-signal! (wire new-value)
  (funcall (funcall wire 'set-signal!) new-value))

(defun add-action! (wire action-procedure)
  (funcall (funcall wire 'add-action!) action-procedure))

(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-time *the-agenda*))
                  action
                  *the-agenda*))

(defun propagate ()
  (if (empty-agendap *the-agenda*)
      'done
      (let ((first-item (first-agenda-item *the-agenda*)))
        (funcall first-item)
        (remove-first-agenda-item! *the-agenda*)
        (propagate))))

(defun make-time-segment (time queue)
  (cons time queue))

(defun segment-time (s) (car s))

(defun segment-queue (s) (cdr s))

(defun make-agenda () '(0))

(defun current-time (agenda)
  (car agenda))

(defun set-current-time! (agenda time)
  (set-car! agenda time))

(defun segments (agenda) (cdr agenda))

(defun set-segments! (agenda segments)
  (set-cdr! agenda segments))

(defun first-segment (agenda) (car (segments agenda)))

(defun rest-segments (agenda) (cdr (segments agenda)))

(defun empty-agendap (agenda)
  (null (segments agenda)))

(defun add-to-agenda! (time action agenda)
  (labels ((belongs-beforep (segments)
             (or (null segments)
                 (< time (segment-time (car segments)))))
           (make-new-time-segment (time action)
             (let ((q (make-queue)))
               (insert-queue! q action)
               (make-time-segment time q)))
           (add-to-segments! (segments)
             (if (= (segment-time (car segments)) time)
                 (insert-queue! (segment-queue (car segments))
                                action)
                 (let ((rest (cdr segments)))
                   (if (belongs-beforep rest)
                       (set-cdr! segments
                                 (cons (make-new-time-segment time action)
                                       (cdr segments)))
                       (add-to-segments! rest))))))
    (let ((segments (segments agenda)))
      (if (belongs-beforep segments)
          (set-segments! agenda (cons (make-new-time-segment time action)
                                      segments))
          (add-to-segments! segments)))))

(defun remove-first-agenda-item! (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-dequep q)
        (set-segments! agenda (rest-segments agenda)))))

(defun first-agenda-item (agenda)
  (if (empty-agendap agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(defun probe (name wire)
  (add-action! wire #'(lambda ()
                        (fresh-line)
                        (format t "~a ~a New-value -> ~a"
                                name (current-time *the-agenda*) (get-signal wire)))))

(defparameter *inverter-delay* nil)
(defparameter *and-gate-delay* nil)
(defparameter *or-gate-delay* nil)
(defparameter *the-agenda* nil)
(defparameter *input-1* nil)
(defparameter *input-2* nil)
(defparameter *sum-wire* nil)
(defparameter *carry-out* nil)

(setf *inverter-delay* 2)
(setf *and-gate-delay* 3)
(setf *or-gate-delay* 5)
(setf *the-agenda* (make-agenda))
(setf *input-1* (make-wire))
(setf *input-2* (make-wire))
(setf *sum-wire* (make-wire))
(setf *carry-out* (make-wire))

(probe 'sum *sum-wire*)
(probe 'carry-out *carry-out*)
(half-adder *input-1* *input-2* *sum-wire* *carry-out*)
(set-signal! *input-1* 1)
(propagate)
(set-signal! *input-2* 1)