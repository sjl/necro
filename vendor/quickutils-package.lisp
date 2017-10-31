(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "NECRO.QUICKUTILS")
    (defpackage "NECRO.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use :cl))))

(in-package "NECRO.QUICKUTILS")

;; need to define this here so sbcl will shut the hell up about it being
;; undefined when compiling quickutils.lisp.  computers are trash.
(defparameter *utilities* nil)

