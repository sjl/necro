(ql:quickload '(:deploy :necro))

(deploy:define-library charms/ll::libcurses
  :system :cl-charms)

;; (deploy:define-resource-directory assets "assets/")

(sb-ext:gc :full t)
(asdf:operate :build-op :necro)
