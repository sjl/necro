(defpackage :necro
  (:use
    :cl
    :iterate
    :losh
    :necro.quickutils)
  (:import-from :chancery
    :define-string
    :s
    :a)
  (:export
    :run
    :main))
