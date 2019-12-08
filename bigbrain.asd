(defpackage :bigbrain-asd
  (:use :cl :asdf))
(in-package :bigbrain-asd)

(asdf:defsystem :bigbrain
  :version      "0.1.0"
  :description  "A Brainfuck interpreter written in Common Lisp"
  :author       "Alex Cameron <ascottcameron@gmail.com>"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "bigbrain")))
