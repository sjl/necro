(in-package :necro)

;;;; State --------------------------------------------------------------------
(defvar *width* 1)
(defvar *height* 1)

;;;; Input --------------------------------------------------------------------
(defun update-window-size ()
  (setf (values *width* *height*)
        (charms:window-dimensions t)))

;;;; Drawing ------------------------------------------------------------------
(defun redraw ()
  (charms:update))


;;;; Main ---------------------------------------------------------------------
(defun run ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-extra-keys t)
    (charms/ll:start-color)
    (charms/ll:curs-set 0)
    (charms:clear-window t)
    (update-window-size)
    (charms:write-string-at-point t "Necro!" 0 0)
    (charms:get-char t)))


;;;; Entry --------------------------------------------------------------------
(defun main ()
  (sb-ext:disable-debugger)
  (setf *random-state* (make-random-state t))
  (run)
  (sb-ext:exit :code 0))



;;;; Scratch ------------------------------------------------------------------
