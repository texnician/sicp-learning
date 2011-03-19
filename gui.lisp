(asdf:operate 'asdf:load-op :cl-gtk2-cairo)
(defpackage :sicp-gui
  (:shadowing-import-from #:cl-cairo2 #:scale)
  (:use :cl #:gtk #:cl-cairo2 #:cl-gtk2-cairo #:iter :sicp))

(in-package :sicp-gui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass cairo-w (drawing-area)
    ((draw-fn :initform 'draw-pic :accessor cairo-w-draw-fn))
    (:metaclass gobject:gobject-class)))

(defmethod initialize-instance :after ((w cairo-w) &rest initargs)
  (declare (ignore initargs))
  (gobject:connect-signal w "configure-event" (lambda (widget event)
                                                (declare (ignore event))
                                                (widget-queue-draw widget)))
  (gobject:connect-signal w "expose-event" (lambda (widget event)
                                             (declare (ignore event))
                                             (cc-expose widget))))

(defmethod (setf cairo-w-draw-fn) :after (new-value (w cairo-w))
  (declare (ignore new-value))
  (widget-queue-draw w))

(defun cc-expose (widget)
  (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
    (with-gdk-context (ctx (widget-window widget))
      (with-context (ctx)
        (funcall (cairo-w-draw-fn widget) w h)
        nil))))


(defun tmp (w h)
  (move-to 0 (/ h 2.0))
  (line-to (/ w 2) 0)
  (line-to w (/ h 2))
  (line-to (/ w 2) h)
  (line-to 0 (/ h 2.0))
  (stroke))

(defun tmp-chunk (f)
  (funcall f 100 100))

(defun draw-diamond (w h)
  (set-source-rgb 1 1 1)
  (fill-preserve)
  (set-source-rgb 0 0 0)
  (set-line-width 0.5)
  (move-to 0 (/ h 2.0))
  (line-to (/ w 2) 0)
  (line-to w (/ h 2))
  (line-to (/ w 2) h)
  (line-to 0 (/ h 2.0))
  (stroke))

(defun draw-clock-face (w h)
  "Draw a clock face"
  (set-line-width 1)
  (translate (/ w 2) (/ h 2))
  (setf w (- w 2) h (- h 2))
  (scale (* 0.99 (/ (min w h) 2)) (* 0.99 (/ (min w h) 2)))
  (set-line-width 0.01)

  ;; Circle
  (arc 0 0 1 0 (* 2 pi))
  (set-source-rgb 1 1 1)
  (fill-preserve)
  (set-source-rgb 0 0 0)
  (stroke)
        
  ;; Ticks
  (iter (for i from 0 below 12)
        (for angle = (/ (* i pi) 6))
        (for cos = (cos angle))
        (for sin = (sin angle))
        (save)
        (if (zerop (mod i 3))
            (progn (set-line-width 0.02)
                   (move-to (* 0.8 cos) (* 0.8 sin)))
            (move-to (* 0.9 cos) (* 0.9 sin)))
        (line-to cos sin)
        (set-source-rgb 0 0 0)
        (stroke)
        (restore)))

(defun run ()
  (within-main-loop
    (let-ui (gtk-window
             :var w
             :default-width 400
             :default-height 400
             :type :toplevel
             :title "SICP picture"
             (cairo-w :var cw))
      (widget-show w))))

(defun cairo-pen (v1 v2)
  (set-source-rgb 0 0 0)
  (set-line-width 0.5)
  (move-to (sicp::xcor-vect v1) (sicp::ycor-vect v1))
  (line-to (sicp::xcor-vect v2) (sicp::ycor-vect v2))
  (stroke))

(defun ->cairo (painter w h)
  (sicp::transform-painter (sicp::flip-vert painter)
                           (sicp::make-vect 0.0 0.0)
                           (sicp::make-vect w 0.0)
                           (sicp::make-vect 0.0 h)))

(defun draw-pic (w h)
  (let ((painter (sicp::segments->painter #'cairo-pen
                                          sicp::*frame-outline*)))
     (funcall (->cairo (sicp::square-limit painter 8) w h) sicp::*identity-frame*)))

;(run)

(defun debug-pen (v1 v2)
  (fresh-line)
  (format t "v1:~a  v2~a" v1 v2))

;; (let ((painter (sicp::segments->painter #'debug-pen sicp::*frame-outline*)))
;;   (funcall (->cairo (sicp::up-split painter 4) 400 400) sicp::*identity-frame*))