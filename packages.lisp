(in-package :cl-user)

(defpackage :mare5x.lispqr.utils
  (:use :common-lisp)
  (:export
   :loop-index-value
   :get-elt
   :floor-div
   :clamp
   :swap
   :print-2d-array
   :splice-list
   :split-list
   :decimal->binary
   :binary->decimal
   :shift-array
   :decimal->n-bit
   :decimal->8-bit
   :with-remainder))

(defpackage :mare5x.lispqr.galois
  (:use :common-lisp)
  (:import-from :mare5x.lispqr.utils
   :binary->decimal
   :decimal->8-bit
   :loop-index-value)
  (:export :generate-ec-codewords))

(defpackage :mare5x.lispqr.matrix
  (:use :common-lisp
        :mare5x.lispqr.utils)
  )

(defpackage :mare5x.lispqr.encode
  (:use
   :common-lisp
   :mare5x.lispqr.utils
   :mare5x.lispqr.galois
   :mare5x.lispqr.matrix))

