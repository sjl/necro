(in-package :necro)

;;;; Parameters ---------------------------------------------------------------
(defparameter *pages-per-book* 50)
(defparameter *initial-books* 5)
(defparameter *reading-glasses-cost* 10)


;;;; State --------------------------------------------------------------------
(defvar *running* nil)
(defvar *width* 1)
(defvar *height* 1)

(defvar *unlocked* nil)
(defvar *locked* nil)

(defvar *pages* nil)
(defvar *books* nil)
(defvar *current-book* nil)
(defvar *current-book-pages* nil)
(defvar *spells* nil)
(defvar *previous-spell-page* nil)
(defvar *skeletons* nil)
(defvar *gold* nil)
(defvar *message* nil)
(defvar *has-glasses* nil)


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

(defun message (m &rest args)
  (setf *message* (apply #'format nil m args)))


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
  "shatter"
  "break"
  "ruin"
  "animate"
  "conjure"
  "ensorcell"
  "enchant")

(define-string adjective
  "dark"
  "enchantment"
  "bone"
  "rotting"
  "cadaverous")

(define-string book
  ("the" color noun)
  ("how to" verb #(noun a))
  (adjective #(noun s)))

(defun update-current-book ()
  (setf *current-book* (format nil "~:(~A~)" (book))))


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

(defun learn-spells ()
  (let* ((since (- *pages* *previous-spell-page*))
         (learned (truncate since 20)))
    (when (plusp learned)
      (incf *spells* learned)
      (incf *previous-spell-page* (* 20 learned)))))

(defun pop-book ()
  (if (plusp *books*)
    (progn (decf *books*)
           (setf *current-book-pages* *pages-per-book*)
           (update-current-book))
    (setf *current-book* nil
          *current-book-pages* 0)))

(defun add-books (n)
  (when (plusp n)
    (incf *books* n)
    (when (null *current-book*)
      (pop-book))))


(defun can-read-page-p ()
  (plusp *current-book-pages*))

(defun read-pages (&optional (pages 1))
  (let ((actually-read (min pages *current-book-pages*)))
    (when (plusp actually-read)
      (incf *pages* actually-read)
      (decf *current-book-pages* actually-read)
      (learn-spells)
      (when (zerop *current-book-pages*)
        (pop-book)))))


;;;; Spells -------------------------------------------------------------------
(defun can-summon-skeleton-p ()
  (plusp *spells*))

(defun summon-skeleton ()
  (when (can-summon-skeleton-p)
    (incf *skeletons* 1)
    (decf *spells* 1)))


;;;; Attacking ----------------------------------------------------------------
(defun can-attack-town-p ()
  (plusp *skeletons*))

(defun attack-town ()
  (when (can-attack-town-p)
    (let* ((power (+ 1 *skeletons*))
           (skeletons-lost (ceiling (/ *skeletons* 2)))
           (books-gained (1+ (random (1+ (truncate power 10)))))
           (gold-gained (random power)))
      (decf *skeletons* skeletons-lost)
      (add-books books-gained)
      (incf *gold* gold-gained)
      (message "~D book~:P looted, ~D gold stolen, ~D skeleton~:P destroyed"
               books-gained
               gold-gained
               skeletons-lost))))


;;;; Items --------------------------------------------------------------------
(defun can-buy-glasses-p ()
  (>= *gold* *reading-glasses-cost*))

(defun buy-glasses ()
  (when (can-buy-glasses-p)
    (decf *gold* *reading-glasses-cost*)
    (setf *has-glasses* t)
    (zapf *unlocked* (remove :buy-glasses *unlocked* :key 'component-id))
    (append *unlocked* (list (make-glasses)))))


;;;; Components ---------------------------------------------------------------
(defclass* component ()
  ((id :initform nil)
   (draw :type (function (integer integer)) :initform (required))
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
                         *current-book-pages*)))
            2)))

(defun make-pages ()
  (make-instance 'component
    :draw (lambda (x y)
            (p x y "Pages read: ~D" (truncate *pages*))
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

(defun make-gold ()
  (make-instance 'component
    :draw (lambda (x y)
            (p x y "Treasury: ~D gold" *gold*)
            1)))


(defun make-read-page! ()
  (make-instance 'component
    :draw (lambda (x y)
            (bold-when (can-read-page-p)
                       (p x y "[ (R)ead page ]"))
            1)
    :key #\r
    :action 'read-pages))

(defun make-summon-skeleton! ()
  (make-instance 'component
    :draw (lambda (x y)
            (bold-when (can-summon-skeleton-p)
                       (p x y "[ Summon (S)keleton ]"))
            1)
    :key #\s
    :action 'summon-skeleton))

(defun make-attack-town! ()
  (make-instance 'component
    :draw (lambda (x y)
            (bold-when (can-attack-town-p)
                       (p x y "[ (A)ttack Town ]"))
            1)
    :key #\a
    :action 'attack-town))

(defun make-buy-glasses ()
  (make-instance 'component
    :id :buy-glasses
    :draw (lambda (x y)
            (bold-when (can-buy-glasses-p)
                       (p x y "[ (B)uy Reading Glasses (~Dg) ]"
                          *reading-glasses-cost*))
            1)
    :key #\b
    :action 'buy-glasses))

(defun make-glasses ()
  (make-instance 'component
    :id :buy-glasses
    :draw (lambda (x y)
            (p x y "Reading Glasses (automatically read 1 page/second)")
            1)))


(defun make-unlockable-spells ()
  (make-unlockable :spells
                   (lambda () (plusp *spells*))
                   (lambda ()
                     (list (make-spells)
                           (make-summon-skeleton!)))))

(defun make-unlockable-servants ()
  (make-unlockable :servants
                   (lambda () (plusp *skeletons*))
                   (lambda () (list (make-servants)
                                    (make-attack-town!)))))

(defun make-unlockable-gold ()
  (make-unlockable :spells
                   (lambda () (plusp *gold*))
                   (lambda ()
                     (list (make-gold)
                           (make-buy-glasses)))))


;;;; Drawing ------------------------------------------------------------------
(defun draw-component (c x y)
  (funcall (component-draw c) x y))


(defun draw-message ()
  (p 0 (1- *height*) *message*))

(defun draw-screen ()
  (charms:clear-window t)
  (iterate (with y = 0)
           (for component :in *unlocked*)
           (incf y (1+ (draw-component component 0 y))))
  (draw-message)
  (charms:update))


;;;; Main ---------------------------------------------------------------------
(defun initialize ()
  (setf *pages* 0
        *spells* 0
        *previous-spell-page* 0
        *skeletons* 0
        *gold* 0
        *books* *initial-books*
        *has-glasses* nil
        *running* t
        *current-book-pages* *pages-per-book*
        *message* "Press [q] to quit."
        *unlocked* (list (make-books)
                         (make-pages)
                         (make-read-page!))
        *locked* (list (make-unlockable-spells)
                       (make-unlockable-servants)
                       (make-unlockable-gold)))
  (update-current-book))


(defun update-window-size ()
  (setf (values *width* *height*)
        (charms:window-dimensions t)))


(defun handle-event (event)
  (case event
    (#\q (setf *running* nil))
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


(defun tick (delta-time)
  (when *has-glasses*
    (read-pages (* 1.0 delta-time))))

(defun game-loop ()
  (iterate (while *running*)
           (handle-events)
           (check-unlockables)
           (timing :real-time :per-iteration-into delta-time)
           (tick (/ delta-time internal-time-units-per-second))
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
