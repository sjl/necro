(in-package :necro)

;;;; Parameters ---------------------------------------------------------------
(defparameter *pages-per-book* 50)
(defparameter *initial-books* 5)


;;;; State --------------------------------------------------------------------
(defvar *running* nil)
(defvar *width* 1)
(defvar *height* 1)

(defvar *unlocked* nil)
(defvar *locked* nil)

(defvar *pages* nil)
(defvar *books* nil)
(defvar *current-book* nil)

;;;; Utilities ----------------------------------------------------------------
(defun required ()
  (error "Required."))

(defun p (x y format-string &rest args)
  (charms:write-string-at-point t (apply #'format nil format-string args) x y))


;;;; Book Titles --------------------------------------------------------------
(chancery:define-string color
  "red"
  "black"
  "brown"
  "green")

(chancery:define-string noun
  "plague"
  "feast"
  "skeleton"
  "arts")

(chancery:define-string book
  ("the" color noun))

(defun update-current-book ()
  (setf *current-book* (format nil "~:(~A~)"
                               (if (plusp *books*)
                                 (book)
                                 nil))))

;;;; Components ---------------------------------------------------------------
(defclass* component ()
  ((draw :type (function (integer integer)) :initform (required))
   (key :type (or null character) :initform nil)
   (action :type (or null function) :initform nil)))


(defun make-books ()
  (make-instance 'component
    :draw (lambda (x y)
            (p x y "Library: ~D spellbook~:P" (ceiling *books*))
            (p x (1+ y) "Currently reading: ~A"
               (if (null *current-book*)
                 "nothing"
                 (format nil "~A (~D page~:P left)"
                         *current-book*
                         (let ((pages (* *pages-per-book* (rem *books* 1))))
                           (if (zerop pages)
                             *pages-per-book*
                             pages)))))
            2)))

(defun make-pages ()
  (make-instance 'component
    :draw (lambda (x y)
            (p x y "Pages read: ~D" *pages*)
            1)))

(defun make-read-page! ()
  (make-instance 'component
    :draw (lambda (x y)
            (p x y "[ (R)ead page ]")
            1)
    :key #\r
    :action (lambda ()
              (when (plusp *books*)
                (incf *pages*)
                (decf *books* (/ *pages-per-book*))
                (when (integerp *books*)
                  (update-current-book))))))


;;;; Drawing ------------------------------------------------------------------
(defun draw-component (c x y)
  (funcall (component-draw c) x y))

(defun draw-help ()
  (p 0 (1- *height*) "[Q]uit"))

(defun draw-screen ()
  (charms:clear-window t)
  (iterate (with y = 0)
           (for component :in *unlocked*)
           (incf y (1+ (draw-component component 0 y))))
  (draw-help)
  (charms:update))


;;;; Main ---------------------------------------------------------------------
(defun initialize ()
  (setf *pages* 0
        *books* *initial-books*
        *running* t
        *unlocked* (list (make-books)
                         (make-pages)
                         (make-read-page!)))
  (update-current-book))

(defun update-window-size ()
  (setf (values *width* *height*)
        (charms:window-dimensions t)))

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
