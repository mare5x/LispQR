;;; Use 1 for a dark module and 0 for a white/blank module.

(in-package :mare5x.lispqr.utils)


(defmacro loop-index-value ((index value) seq &body loop-body)
  `(loop for ,index from 0 to (1- (length ,seq))
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

(defun create-hash-table (plist)
  (let ((ht (make-hash-table :size (/ (length plist) 2))))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash key ht) value))
    ht))

(defun print-2d (arr)
  (dolist (row arr)
    (format t "~{~a~^~}~%" row)))

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


