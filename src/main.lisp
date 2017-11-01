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
(defvar *spells* nil)
(defvar *skeletons* nil)


;;;; Utilities ----------------------------------------------------------------
(defun required ()
  (error "Required."))

(defmacro bold (&body body)
  `(unwind-protect
     (progn
       (charms/ll:attron charms/ll:a_bold)
       ,@body)
     (charms/ll:attroff charms/ll:a_bold)))

(defmacro bold-when (condition &body body)
  `(unwind-protect
     (progn
       (when ,condition (charms/ll:attron charms/ll:a_bold))
       ,@body)
     (charms/ll:attroff charms/ll:a_bold)))

(defun p (x y format-string &rest args)
  (charms:write-string-at-point t (apply #'format nil format-string args) x y))


;;;; Book Titles --------------------------------------------------------------
(define-string color
  "red"
  "black"
  "brown"
  "green")

(define-string noun
  "plague"
  "prince"
  "castle"
  "feast"
  "rite"
  "winter"
  "skeleton"
  "bloodlust"
  "art")

(define-string verb
  "raise"
  "animate"
  "conjure"
  "ensorcell"
  "enchant")

(define-string adjective
  "dark"
  "enchantment"
  "rotting"
  "cadaverous")

(define-string book
  ("the" color noun)
  ("how to" verb #(noun a))
  (adjective #(noun s)))

(defun update-current-book ()
  (setf *current-book* (if (plusp *books*)
                         (format nil "~:(~A~)" (book))
                         nil)))


;;;; Unlockables --------------------------------------------------------------
(defun make-unlockable (id unlock-p do-unlock)
  (list id unlock-p do-unlock))

(defun unlockablep (u)
  (funcall (second u)))

(defun unlock (u)
  (funcall (third u)))


;;;; Resources ----------------------------------------------------------------
(defun learn-spell ()
  (incf *spells*))


(defun can-read-page-p ()
  (plusp *books*))

(defun read-page ()
  (when (can-read-page-p)
    (incf *pages*)
    (decf *books* (/ *pages-per-book*))
    (when (dividesp *pages* 20)
      (learn-spell))
    (when (integerp *books*)
      (update-current-book))))


;;;; Spells -------------------------------------------------------------------
(defun can-summon-skeleton-p ()
  (plusp *spells*))

(defun summon-skeleton ()
  (when (can-summon-skeleton-p)
    (incf *skeletons* 1)
    (decf *spells* 1)))


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

(defun make-spells ()
  (make-instance 'component
    :draw (lambda (x y)
            (p x y "Spells memorized: ~D" *spells*)
            1)))

(defun make-servants ()
  (make-instance 'component
    :draw (lambda (x y)
            (p x y "SERVANTS:")
            (p x (1+ y) "~D skeleton~:P" *skeletons*)
            2)))

(defun make-read-page! ()
  (make-instance 'component
    :draw (lambda (x y)
            (bold-when (can-read-page-p)
                       (p x y "[ (R)ead page ]"))
            1)
    :key #\r
    :action #'read-page))

(defun make-summon-skeleton! ()
  (make-instance 'component
    :draw (lambda (x y)
            (bold-when (can-summon-skeleton-p)
                       (p x y "[ Summon (S)keleton ]"))
            1)
    :key #\s
    :action #'summon-skeleton))

(defun make-unlockable-spells ()
  (make-unlockable :spells
                   (lambda () (plusp *spells*))
                   (lambda ()
                     (list (make-spells)
                           (make-summon-skeleton!)))))

(defun make-unlockable-servants ()
  (make-unlockable :servants
                   (lambda () (plusp *skeletons*))
                   (lambda () (list (make-servants)))))


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
        *spells* 0
        *skeletons* 0
        *books* *initial-books*
        *running* t
        *unlocked* (list (make-books)
                         (make-pages)
                         (make-read-page!))
        *locked* (list (make-unlockable-spells)
                       (make-unlockable-servants)))
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


(defun check-unlockables ()
  (zapf *locked*
        (iterate (for u :in %)
                 (if (unlockablep u)
                   (setf *unlocked* (append *unlocked* (unlock u)))
                   (collect u)))))


(defun game-loop ()
  (iterate (while *running*)
           (handle-events)
           (check-unlockables)
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
