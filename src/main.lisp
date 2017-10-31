(in-package :necro)

;;;; State --------------------------------------------------------------------
(defvar *running* nil)
(defvar *width* 1)
(defvar *height* 1)

(defvar *pages-read* nil)

;;;; Input --------------------------------------------------------------------
(defun update-window-size ()
  (setf (values *width* *height*)
        (charms:window-dimensions t)))


;;;; Drawing ------------------------------------------------------------------
(defun p (x y format-string &rest args)
  (charms:write-string-at-point t (apply #'format nil format-string args) x y))

(defun draw-pages-read ()
  (p 0 0 "Pages read: ~D" *pages-read*))

(defun draw-help ()
  (p 0 (1- *height*) "[Q]uit"))

(defun draw-screen ()
  (charms:clear-window t)
  (draw-pages-read)
  (draw-help)
  (charms:update))


;;;; Main ---------------------------------------------------------------------
(defun initialize ()
  (setf *pages-read* 0
        *running* t))

(defun handle-event (event)
  (case event
    (#\Q (setf *running* nil))
    (:resize (update-window-size))))


(defun handle-events ()
  (iterate
    (for event = (charms:get-char t :ignore-error t))
    (while event)
    (handle-event event)))


(defun game-loop ()
  (iterate (while *running*)
           (handle-events)
           (draw-screen)
           (sleep 1/30)))


(defun run ()
  (initialize)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode t)
    (charms:enable-extra-keys t)
    (charms/ll:start-color)
    (charms/ll:curs-set 0)
    (charms:clear-window t)
    (update-window-size)
    (game-loop)))


;;;; Entry --------------------------------------------------------------------
(defun main ()
  (sb-ext:disable-debugger)
  (setf *random-state* (make-random-state t))
  (run)
  (sb-ext:exit :code 0))



;;;; Scratch ------------------------------------------------------------------
