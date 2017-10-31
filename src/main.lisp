(in-package :necro)

;;;; State --------------------------------------------------------------------
(defvar *running* nil)
(defvar *width* 1)
(defvar *height* 1)

(defvar *pages* nil)
(defvar *unlocked* nil)

;;;; Utilities ----------------------------------------------------------------
(defun required ()
  (error "Required."))

(defun p (x y format-string &rest args)
  (charms:write-string-at-point t (apply #'format nil format-string args) x y))


;;;; Input --------------------------------------------------------------------
(defun update-window-size ()
  (setf (values *width* *height*)
        (charms:window-dimensions t)))


;;;; Components ---------------------------------------------------------------
(defclass* component ()
  ((draw :type (function (integer integer)) :initform (required))
   (key :type (or null character) :initform nil)
   (action :type (or null function) :initform nil)))

;;;; Pages --------------------------------------------------------------------
(defun make-pages-read ()
  (make-instance 'component
    :draw (lambda (x y)
            (p x y "Pages read: ~D" *pages*))))

(defun make-read-page! ()
  (make-instance 'component
    :draw (lambda (x y)
            (p x y "[ (R)ead page ]"))
    :key #\r
    :action (lambda ()
              (incf *pages*))))


;;;; Drawing ------------------------------------------------------------------
(defun draw-component (c x y)
  (funcall (component-draw c) x y))

(defun draw-help ()
  (p 0 (1- *height*) "[Q]uit"))

(defun draw-screen ()
  (charms:clear-window t)
  (iterate (for component :in *unlocked*)
           (for y :from 0)
           (draw-component component 0 y))
  (draw-help)
  (charms:update))


;;;; Main ---------------------------------------------------------------------
(defun initialize ()
  (setf *pages* 0
        *running* t
        *unlocked* (list (make-pages-read)
                         (make-read-page!))))

(defun handle-event (event)
  (case event
    (#\Q (setf *running* nil))
    (:resize (update-window-size))
    (t (when-let ((component (find event *unlocked* :key #'component-key)))
         (funcall (component-action component))))))


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
