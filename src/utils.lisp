;;; Use 1 for a dark module and 0 for a white/blank module.

(in-package :mare5x.lispqr.utils)


(defmacro loop-index-value ((index value) seq &body loop-body)
  `(loop for ,index from 0 below (length ,seq)
         for ,value = (elt ,seq ,index)
         ,@loop-body))

(defmacro char-tuple (key value)
  `(cons (character ,key) ,value))

(defmacro char-tuple-list (&rest pairs)
  `(loop for (k v) on (list ,@pairs) by #'cddr while v
         collect (char-tuple k v)))

(defun get-elt (seq index &optional (default nil))
  (if (< index (length seq))
      (elt seq index)
      default))

(defun clamp (val low high)
  (max low (min val high)))

(defun floor-div (a b)
  (floor (/ a b)))

(defun create-hash-table (plist)
  (let ((ht (make-hash-table :size (/ (length plist) 2))))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash key ht) value))
    ht))

(defun print-2d-array (array)
  (destructuring-bind (n m) (array-dimensions array)
    (loop for row from 0 below n do
          (loop for col from 0 below m
                do (format t "~a " (aref array row col)))
          (format t "~%"))))

(defun print-bits (bits &optional (type :hex))
  (if (bit-vector-p bits)
      (setf bits (split-list (pad-bits bits) 8)))
  (loop with ftype = nil
        initially (ecase type
                    (:hex (setf ftype "~2,'0x"))
                    (:bin (setf ftype "~8,'0b"))
                    (:dec (setf ftype "~d")))
        for string in bits do
        (format t (string+ ftype " ") (binary->decimal string))
        finally (format t "~%")))

(defmacro swap (seq i j)
  `(rotatef (elt ,seq ,i) (elt ,seq ,j)))

(defun splice-list (seq)
  ;; ((1 2 3)) -> (1 2 3) 
  (reduce #'(lambda (x y) (concatenate 'list x y)) seq))

(defun split-list (seq n)
  "Splits a sequence into a list of n-wide parts."
  (if (< (length seq) n)
      (if (plusp (length seq))
          (list seq)
          NIL)
      (append (list (subseq seq 0 n)) (split-list (subseq seq n) n))))

(defun pad-bits (bits)
  "(Left) pad a bit vector with 0s into a multiple of 8."
  (let ((pad (mod (length bits) 8)))
    (if (zerop pad) (return-from pad-bits (copy-seq bits)))
    (concatenate 'bit-vector
                 (make-array (- 8 pad) :element-type 'bit)
                 bits)))

(defun decimal->binary (n)
  (when (<= n 1)
    (return-from decimal->binary (make-array 2
                                             :element-type 'bit
                                             :adjustable t
                                             :fill-pointer 1
                                             :initial-contents (list n 0))))

  (let ((binary (decimal->binary (floor (/ n 2)))))
    (vector-push-extend (mod n 2) binary)
    binary))

(defun binary->decimal (seq)
  (loop for i from (1- (length seq)) downto 0
        for bit = (elt seq i)
        for p = 1 then (* 2 p)
        sum (* bit p)))

(defun shift-array (array k)
  (let* ((d (length array))
         (n (+ d k)))
    (adjust-array array n :fill-pointer n)
    (when (< d n)
      (loop
        for i from (1- d) downto 0
        for j = (1- n) then (+ i k)
        do (swap array i j)))
    array))

(defun decimal->n-bit (decimal n)
  (let ((bits (decimal->binary decimal)))
    (shift-array bits (- n (length bits)))))

(defun decimal->8-bit (decimal)
  (decimal->n-bit decimal 8))

(defmacro with-remainder ((var n divisor) &body body)
  "Bind var to be n mod divisor."
  `(let ((,var (mod ,n ,divisor)))
     ,@body))

(defun string+ (&rest strings)
  (apply #'concatenate 'string strings))

(defun string-split (str &optional (delim #\ ))
  (loop for i = (position-if-not #'(lambda (x) (char= delim x)) str)
        then (position-if-not #'(lambda (x) (char= delim x)) str :start (1+ j))
        for j = (and i (position delim str :start (1+ i)))
        if i collect (subseq str i j)
        while j))

(defun list-lpad (list n val)
  (append (make-list n :initial-element val) list))

(defun list-rpad (list n val)
  (append list (make-list n :initial-element val)))

(defun list-ltrim (list val)
  (loop while (eql (car list) val) do (pop list))
  list)

(defun list-xor (list &rest rest)
  (apply #'mapcar #'logxor list rest))

(defun sequence->list (seq)
  (map 'list #'identity seq))


