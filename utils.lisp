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

;; Defines the constant table, that maps (version, error-level) pairs
;; to the number of data codewords necessary.
(defconstant +data-codewords-table+
  (make-array '(41 4) :element-type 'integer :initial-contents
              '((0 0 0 0)
                (19 16 13 9)
                (34 28 22 16)
                (55 44 34 26)
                (80 64 48 36)
                (108 86 62 46)
                (136 108 76 60)
                (156 124 88 66)
                (194 154 110 86)
                (232 182 132 100)
                (274 216 154 122)
                (324 254 180 140)
                (370 290 206 158)
                (428 334 244 180)
                (461 365 261 197)
                (523 415 295 223)
                (589 453 325 253)
                (647 507 367 283)
                (721 563 397 313)
                (795 627 445 341)
                (861 669 485 385)
                (932 714 512 406)
                (1006 782 568 442)
                (1094 860 614 464)
                (1174 914 664 514)
                (1276 1000 718 538)
                (1370 1062 754 596)
                (1468 1128 808 628)
                (1531 1193 871 661)
                (1631 1267 911 701)
                (1735 1373 985 745)
                (1843 1455 1033 793)
                (1955 1541 1115 845)
                (2071 1631 1171 901)
                (2191 1725 1231 961)
                (2306 1812 1286 986)
                (2434 1914 1354 1054)
                (2566 1992 1426 1096)
                (2702 2102 1502 1142)
                (2812 2216 1582 1222)
                (2956 2334 1666 1276))))

;; Association list: (mode . index).
(defconstant +mode->index+
  (pairlis '(numeric alphanumeric 8-bit-byte kanji) '(0 1 2 3)))

;; Assoc list: (error-correction-level . index).
(defconstant +ec-level->index+
  (pairlis '(L M Q H) '(0 1 2 3)))

(defun get-required-data-codewords (&key version ec-level)
  (setf ec-level (cdr (assoc ec-level +ec-level->index+)))
  (aref +data-codewords-table+ version ec-level))

(defun character-count-indicator-bits (&key mode version)
  (setf mode (cdr (assoc mode +mode->index+)))
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

(defun clamp (val low high)
  (max low (min val high)))

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

(defmacro with-remainder ((var n divisor) &body body)
  "Bind var to be n mod divisor."
  `(let ((,var (mod ,n ,divisor)))
     ,@body))

(defconstant +pad-alternators+ '(#*11101100 #*00010001))

(defun bits->codewords (bits &key (version 1) (ec-level 'Q))
  "Transforms a sequence of bits in 
   (mode-indicator, character-count-indicator, encoded-data, terminator)
   to a valid concatenated sequence of 8-bit codewords and padded
   until the required data codewords quota is filled."
  (let ((required-bits (* 8 (get-required-data-codewords :version version :ec-level ec-level)))
        (total-bits (loop for seq in bits sum (length seq))))
    ;; Optionally add more 0s to make the length a multiple of 8.
    (with-remainder (rem total-bits 8)
      (when (not (zerop rem))
        (nconc bits (list (make-array (- 8 rem) :element-type 'bit :initial-element 0)))
        (incf total-bits (- 8 rem))))

    ;; Add pad bytes if the bit stream is still too short.
    (let ((pads (/ (- required-bits total-bits) 8)))
      (loop for i below pads
            for pad = (first +pad-alternators+) then (nth (mod i 2) +pad-alternators+)
            do (nconc bits (list pad))))

    (reduce #'(lambda (a b) (concatenate 'bit-vector a b)) bits)))

(defun encode-alphanumeric (str &key (version 1) (ec-level 'Q))
  "Encodes the given string 'str' using alphanumeric mode.
   Returns a list of parts: 
     (mode-indicator, character-count-indicator, encoded-data, terminator)."

  (let ((values nil)
        (char-count-bits nil)
        (required-bits nil)
        (total-bits nil)
        (terminator-width nil))
    ;; Step 1. Determine character values according to translation table.
    ; The value of each character is it's position (index) in the table.
    (setf values (map 'vector #'(lambda (ch) (position ch +alphanumeric-table+)) str))

    ;; Step 2. Divide the result into groups of two decimal values.
    (setf values (split-list values 2))

    ;; Step 3. Convert each group to its 11-bit binary equivalent.
    (setf values (loop for pair in values
                       collect (pair->binary pair)))
    ;; Concatenate the bit vectors into a single bit vector:
    ;; Now we have the one element list: (encoded-data).
    (setf values (list (reduce #'(lambda (a b) (concatenate 'bit-vector a b)) values)))

    ;; Step 4. Character count indicator to binary.
    (setf char-count-bits
          (decimal->n-bit (length str)
                          (character-count-indicator-bits
                           :mode 'alphanumeric :version version)))

    ;; Step 5. Add Mode Indicator and Character count indicator to
    ;; binary data.
    (push char-count-bits values)
    (push (getf +mode-indicators+ :alphanumeric) values)

    ;; Step 6. Add a terminator.
    (setf required-bits (get-required-data-codewords :version version :ec-level ec-level))
    (setf required-bits (* 8 required-bits))
    (setf total-bits (loop for bits in values
                           sum (length bits)))
    ; The terminator is between 0 and 4 bits of zeros.
    (setf terminator-width (clamp (- required-bits total-bits) 0 4))
    (nconc values (list (make-array terminator-width :element-type 'bit :initial-element 0)))
    
    values))

(defun encode (str &key (version 1) (ec-level 'Q))
  ;; error-correction must be L, M, Q, or H.
  (encode-alphanumeric str :version version :ec-level ec-level)
  
  )
