(in-package :cl-user)
(defpackage :bigbrain
  (:use :common-lisp))
(in-package :bigbrain)

(defparameter *instructions* "dsafa")
(defparameter *instruction-pos* 0)
(defparameter *data* (make-array 30000 :initial-element 0))
(defparameter *data-pos* 0)
(defparameter *loop-stack* (make-array 0 :adjustable t))

(defun read-instruction ()
  (when (< *instruction-pos* (length *instructions*))
    (let ((x (elt *instructions* *instruction-pos*)))
      (incf *instruction-pos*)
      x)))

(defun load-instructions (filename)
  (format t "loading program ~A~%" filename))

(defun execute-instruction (ins)
  (cond
    ((eql ins #\>) (incr-pointer))
    ((eql ins #\<) (decr-pointer))
    ((eql ins #\+) (incr-data))
    ((eql ins #\-) (decr-data))
    ((eql ins #\.) (output-data))
    ((eql ins #\,) (input-data))
    ((eql ins #\[) (loop-begin))
    ((eql ins #\]) (loop-end))
    (t (format t "ignoring instruction ~A~%" ins))))

(defun incr-pointer ()
  (if (>= *data-pos* (length *data*))
      (error "incremented data pointer out of bounds")
      (incf *data-pos*)))

(defun decr-pointer ()
  (if (<= *data-pos* 0)
      (error "decremented data pointer out of bounds")
      (decf *data-pos*)))

(defun incr-data ()
  (assert (< *data-pos* (length *data*)))
  (incf (elt *data* *data-pos*)))

(defun decr-data ()
  (assert (< *data-pos* (length *data*)))
  (decf (elt *data* *data-pos*)))

(defun output-data ()
  (assert (< *data-pos* (length *data*)))
  (print (elt *data* *data-pos*)))

(defun input-data ()
  (assert (< *data-pos* (length *data*)))
  (setf (elt *data* *data-pos*) (read-char)))

(defun loop-begin ()
  (assert (< *data-pos* (length *data*)))
  (assert (< *instruction-pos* (length *instructions*)))
  )

(defun loop-end ()
  (assert (< *data-pos* (length *data*)))
  (when (eql (length *loop-stack*) 0)
    (error "encountered loop end without a corresponding loop begin"))
  (if (eql (elt *data* *data-pos*) 0)
      (vector-pop *loop-stack*)
      (setf *instruction-pos* (elt *loop-stack* (- (length *loop-stack*) 1)))))

(defun run-program (filename)
  (setf *instruction-pos* 0
        *data-pos* 0)
  (load-instructions filename)
  ;; How do I do this properly?
  (do ((ins (read-instruction) (read-instruction)))
      ((not ins))
    (execute-instruction ins)))
