(in-package :cl-user)
(defpackage :bigbrain
  (:use :common-lisp)
  (:export :run-program))
(in-package :bigbrain)

(defvar *instructions*)
(defparameter *instruction-pos* 0)
(defparameter *data* (make-array 30000 :initial-element 0))
(defparameter *data-pos* 0)
(defparameter *loop-stack* (make-array 0 :fill-pointer 0 :adjustable t))

(defun read-instruction ()
  (when (< *instruction-pos* (length *instructions*))
    (elt *instructions* *instruction-pos*)))

(defun load-instructions (filename)
  (with-open-file (in filename :direction :input :if-does-not-exist nil)
    (when in
      (setf *instructions* (make-string (file-length in)))
      (read-sequence *instructions* in))))

(defun execute-instruction (ins)
  (cond
    ((eql ins #\>) (incr-pointer))
    ((eql ins #\<) (decr-pointer))
    ((eql ins #\+) (incr-data))
    ((eql ins #\-) (decr-data))
    ((eql ins #\.) (output-data))
    ((eql ins #\,) (input-data))
    ((eql ins #\[) (loop-begin))
    ((eql ins #\]) (loop-end))))

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
  (format t "~a" (code-char (elt *data* *data-pos*))))

(defun input-data ()
  (assert (< *data-pos* (length *data*)))
  (setf (elt *data* *data-pos*) (char-code (read-char t nil (code-char 0)))))

(defun loop-begin ()
  (assert (< *data-pos* (length *data*)))
  (assert (< *instruction-pos* (length *instructions*)))
  (if (not (eq (elt *data* *data-pos*) 0))
      (vector-push-extend *instruction-pos* *loop-stack*)
      ;; Keep track of nested loops.
      (progn
        (incf *instruction-pos*)
        (do ((stack-size 0)
             (cur-ins (read-instruction) (read-instruction)))
            ((or (not cur-ins)
                 (and (eql stack-size 0)
                      (eql cur-ins #\]))))
          (when (eql cur-ins #\[)
            (incf stack-size))
          (when (eql cur-ins #\])
            (decf stack-size))
          (incf *instruction-pos*)))))

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
    (execute-instruction ins)
    (incf *instruction-pos*)))
