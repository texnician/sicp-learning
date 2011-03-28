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
    (if (not (eq pwd password))
        (error "Incorrect password")
        (let ((f (cond ((eq op 'withdraw) #'-)
                       ((eq op 'deposit) #'+))))
          (lambda (x)
            (setf balance (funcall f balance x))
             balance)))))

(defparameter *test-account* (make-account 100 'not-secret-password))
(funcall (funcall *test-account* 'not-secret-password 'withdraw) 40)
(funcall (funcall *test-account* 'not-secret-password 'deposit) 50)