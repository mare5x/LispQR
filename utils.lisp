;;; Use 1 for a dark module and 0 for a white/blank module.

;; Table of mode indicators as defined in Table 2 (8.4).
;; Each entry is a composed of 4 bits.
(defconstant +mode-indicators+
  (list :eci               #*0111
        :numeric           #*0001
        :alphanumeric      #*0010
        :8-bit-byte        #*0100
        :kanji             #*1000
        :structured-append #*0011
        :fnc1              NIL
        :terminator        #*0000))

; The character at the i-th index has a value of i.
(defconstant +alphanumeric-table+
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXZY $%*+-./:")

(defconstant +character-count-indicator-table+
  (make-array '(3 4) :element-type 'integer :initial-contents
              '((10 9 8 8)
                (12 11 16 10)
                (14 13 16 12))))

(defmacro character-count-indicator-bits (&key mode version)
  (setf mode (ecase mode
               (numeric 0)
               (alphanumeric 1)
               (8-bit-byte 2)
               (kanji 3)))
  (cond
    ((<= 1 version 9)
     (aref +character-count-indicator-table+ 0 mode))
    ((<= 10 version 26)
     (aref +character-count-indicator-table+ 1 mode))
    ((<= 27 version 40)
     (aref +character-count-indicator-table+ 2 mode))))

(defmacro char-tuple (key value)
  `(cons (character ,key) ,value))

(defmacro char-tuple-list (&rest pairs)
  `(loop for (k v) on (list ,@pairs) by #'cddr while v
         collect (char-tuple k v)))

(defun create-hash-table (plist)
  (let ((ht (make-hash-table :size (/ (length plist) 2))))
    (loop for (key value) on plist by #'cddr
          do (setf (gethash key ht) value))
    ht))

(defun version-size (version)
  (+ 21 (* 4 (1- version))))

(defun print-2d (arr)
  (dolist (row arr)
    (format t "~{~a~^~}~%" row)))

(defun finder-pattern ()
  "Returns a 2-d array representing a 7x7 finder pattern."

  (list (list 1 1 1 1 1 1 1)
        (list 1 0 0 0 0 0 1)
        (list 1 0 1 1 1 0 1)
        (list 1 0 1 1 1 0 1)
        (list 1 0 1 1 1 0 1)
        (list 1 0 0 0 0 0 1)
        (list 1 1 1 1 1 1 1)))

(defmacro swap (seq i j)
  `(rotatef (elt ,seq ,i) (elt ,seq ,j)))

(defun split-list (seq n)
  "Splits a sequence into a list of n-wide parts."
  (if (< (length seq) n)
      (if (plusp (length seq))
          (list seq)
          NIL)
      (append (list (subseq seq 0 n)) (split-list (subseq seq n) n))))

(defun decimal->binary (n)
  (when (zerop n)
    (return-from decimal->binary (make-array 2 :adjustable t :element-type 'bit :fill-pointer 0)))

  (let ((binary (decimal->binary (floor (/ n 2)))))
    (vector-push-extend (mod n 2) binary)
    binary))

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

(defun pair->binary (pair)
  (if (= 1 (length pair))
      (decimal->n-bit (elt pair 0) 6)
      (let ((a (elt pair 0))
            (b (elt pair 1)))
        (decimal->n-bit (+ b (* 45 a)) 11))))

(defun encode-alphanumeric (str)
  "Encodes the given string 'str' using alphanumeric mode."

  (let ((values nil)
        (char-count-bits nil))
    ;; Step 1. Determine character values according to translation table.
    ; The value of each character is it's position (index) in the table.
    (setf values (map 'vector #'(lambda (ch) (position ch +alphanumeric-table+)) str))

    ;; Step 2. Divide the result into groups of two decimal values.
    (setf values (split-list values 2))

    ;; Step 3. Convert each group to its 11-bit binary equivalent.
    (setf values (loop for pair in values
                       collect (pair->binary pair)))

    ;; Step 4. Character count indicator to binary.
    (setf char-count-bits (decimal->n-bit (length str) (character-count-indicator-bits :mode alphanumeric :version 1)))

    ;; Step 5. Add Mode Indicator and Character count indicator to
    ;; binary data.
    (push char-count-bits values)
    (push (getf +mode-indicators+ :alphanumeric) values)
    
    values))

