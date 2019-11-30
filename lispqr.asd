(defpackage #:lispqr-asd
  (:use :cl :asdf))

(in-package :lispqr-asd)

(defsystem "lispqr"
  :name "LispQR"
  :version "1.0.0"
  :author "mare5x.dev@gmail.com"
  :description "QR code encoding."
  :serial t
  :depends-on ("zpng")
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "utils")
                             (:file "image")
                             (:file "galois")
                             (:file "matrix")
                             (:file "encode")))))
