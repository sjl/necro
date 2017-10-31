(ql:quickload '(:deploy :necro))

;; (deploy:define-library blt:bearlibterminal
;;   :system :cl-blt)

;; (deploy:define-resource-directory assets "assets/")

(sb-ext:gc :full t)
(asdf:operate :build-op :necro)
