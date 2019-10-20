(defparameter *src-dir* "d:/Mare5/dev/projects/LispQR/")

(defmacro load-file (relative)
  `(load (concatenate 'string ,*src-dir* ,relative)))

(load-file "packages.lisp")
(load-file "utils.lisp")
(load-file "galois.lisp")
(load-file "matrix.lisp")
(load-file "encode.lisp")

