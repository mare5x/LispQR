(defparameter *src-dir* "d:/Mare5/dev/projects/LispQR/src/")

(defmacro load-file (relative)
  `(load (concatenate 'string ,*src-dir* ,relative)))

(asdf:oos 'asdf:load-op :zpng)

(load-file "packages.lisp")
(load-file "utils.lisp")
(load-file "galois.lisp")
(load-file "matrix.lisp")
(load-file "encode.lisp")
(load-file "image.lisp")

