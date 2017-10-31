(asdf:defsystem :necro
  :description "Clicker game."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"

  :depends-on (:deploy
               :iterate
               :losh
               :cl-charms
               )

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "necro"
  :entry-point "necro:main"

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "main")))))

