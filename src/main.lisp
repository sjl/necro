(in-package :necro)


;;;; Main ---------------------------------------------------------------------
(defun run ()
  (print "Game goes here."))


;;;; Entry --------------------------------------------------------------------
(defun main ()
  (sb-ext:disable-debugger)
  (setf *random-state* (make-random-state t))
  (run)
  (sb-ext:exit :code 0))


;;;; Scratch ------------------------------------------------------------------
