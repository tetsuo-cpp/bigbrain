(in-package :cl-user)
(defpackage :bigbrain
  (:use :cl)
  (:export :run-program :interpreter-error))
(in-package :bigbrain)

(defconstant +data-size+ 30000)
(defvar *instructions*)
(defparameter *instruction-pos* 0)
(defparameter *data* (make-array +data-size+ :initial-element 0))
(defparameter *data-pos* 0)
(defparameter *loop-stack* (make-array 0 :fill-pointer 0 :adjustable t))

(define-condition interpreter-error (error) ())

(defun read-instruction ()
  (when (< *instruction-pos* (length *instructions*))
    (aref *instructions* *instruction-pos*)))

(defun load-instructions (filename)
  (with-open-file (file filename :direction :input)
    (setf *instructions* (make-string (file-length file)))
    (read-sequence *instructions* file)))

(defun incr-pointer ()
  (if (>= *data-pos* (length *data*))
      (error 'interpreter-error "Incremented data pointer out of bounds")
      (incf *data-pos*)))

(defun decr-pointer ()
  (if (<= *data-pos* 0)
      (error 'interpreter-error "Decremented data pointer out of bounds")
      (decf *data-pos*)))

(defun incr-data ()
  (assert (< *data-pos* (length *data*)))
  (incf (aref *data* *data-pos*)))

(defun decr-data ()
  (assert (< *data-pos* (length *data*)))
  (decf (aref *data* *data-pos*)))

(defun output-data ()
  (assert (< *data-pos* (length *data*)))
  (format t "~a" (code-char (aref *data* *data-pos*))))

(defun input-data ()
  (assert (< *data-pos* (length *data*)))
  (let* ((zero-char (code-char 0))
         (input-char (char-code (read-char t nil zero-char))))
    (setf (aref *data* *data-pos*) input-char)))

(defun loop-begin ()
  (assert (< *data-pos* (length *data*)))
  (assert (< *instruction-pos* (length *instructions*)))
  (if (not (eql (aref *data* *data-pos*) 0))
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
    (error 'interpreter-error "Encountered loop end without a corresponding loop begin"))
  (if (eql (aref *data* *data-pos*) 0)
      (vector-pop *loop-stack*)
      (setf *instruction-pos* (aref *loop-stack* (- (length *loop-stack*) 1)))))

(defun execute-instruction (ins)
  (case ins
    (#\> (incr-pointer))
    (#\< (decr-pointer))
    (#\+ (incr-data))
    (#\- (decr-data))
    (#\. (output-data))
    (#\, (input-data))
    (#\[ (loop-begin))
    (#\] (loop-end))))

(defun run-program (filename)
  (setf *instruction-pos* 0
        *data-pos* 0)
  (load-instructions filename)
  (let ((ins))
    (loop while (setf ins (read-instruction)) do
         (execute-instruction ins)
         (incf *instruction-pos*))))
