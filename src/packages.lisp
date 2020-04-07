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
   :print-bits
   :splice-list
   :split-list
   :decimal->binary
   :binary->decimal
   :shift-array
   :decimal->n-bit
   :decimal->8-bit
   :with-remainder
   :string+
   :string-split
   :list-lpad
   :list-rpad
   :list-ltrim
   :list-xor
   :sequence->list))

(defpackage :mare5x.lispqr.galois
  (:use :common-lisp)
  (:import-from :mare5x.lispqr.utils
   :binary->decimal
   :decimal->8-bit
   :loop-index-value)
  (:export :generate-ec-codewords))

(defpackage :mare5x.lispqr.image
  (:use
   :common-lisp
   :zpng
   :mare5x.lispqr.utils)
  (:export
   :write-qr-matrix
   :write-qr-text))

(defpackage :mare5x.lispqr.matrix
  (:use :common-lisp
   :mare5x.lispqr.utils)
  (:import-from :mare5x.lispqr.image
                :write-qr-matrix)
  (:export
   :loop-submatrix
   :version-size
   :init-matrix
   :copy-matrix
   :mask-pattern
   :mask-pattern-test-fn
   :make-qr-matrix))

(defpackage :mare5x.lispqr.encode
  (:nicknames :lispqr)
  (:use
   :common-lisp
   :mare5x.lispqr.utils
   :mare5x.lispqr.galois
   :mare5x.lispqr.matrix
   :mare5x.lispqr.image)
  (:export
   :encode->matrix
   :encode->image))

