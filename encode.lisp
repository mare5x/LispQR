(in-package :mare5x.lispqr.encode)

(defclass version-ec-level-characteristics ()
  ((version :initarg :version)
   (ec-level :initarg :ec-level)
   (total-codewords :initarg :total-codewords)
   (data-codewords :initarg :data-codewords)
   (ec-codewords :initarg :ec-codewords)
   (ec-codewords-per-block :initarg :ec-codewords-per-block)
   (ec-blocks :initarg :ec-blocks) ; List of numbers.
   (data-codewords-per-block :initarg :data-codewords-per-block)))

(defun make-table-entry (version
                         ec-level
                         total-codewords
                         data-codewords
                         ec-codewords
                         ec-codewords-per-block
                         ec-blocks
                         data-codewords-per-block)
  (make-instance 'version-ec-level-characteristics
                 :version version
                 :ec-level ec-level
                 :total-codewords total-codewords
                 :data-codewords data-codewords
                 :ec-codewords ec-codewords
                 :ec-codewords-per-block ec-codewords-per-block
                 :ec-blocks ec-blocks
                 :data-codewords-per-block data-codewords-per-block))

(defconstant +version-ec-characteristics+
  (make-array 61 :element-type 'version-ec-level-characteristics :initial-contents
              (list (make-table-entry 1 'L 26 19 7 7 '(1) '(19))
                    (make-table-entry 1 'M 26 16 10 10 '(1) '(16))
                    (make-table-entry 1 'Q 26 13 13 13 '(1) '(13))
                    (make-table-entry 1 'H 26 9 17 17 '(1) '(9))
                    (make-table-entry 2 'L 44 34 10 10 '(1) '(34))
                    (make-table-entry 2 'M 44 28 16 16 '(1) '(28))
                    (make-table-entry 2 'Q 44 22 22 22 '(1) '(22))
                    (make-table-entry 2 'H 44 16 28 28 '(1) '(16))
                    (make-table-entry 3 'L 70 55 15 15 '(1) '(55))
                    (make-table-entry 3 'M 70 44 26 26 '(1) '(44))
                    (make-table-entry 3 'Q 70 34 36 18 '(2) '(17))
                    (make-table-entry 3 'H 70 26 44 22 '(2) '(13))
                    (make-table-entry 4 'L 100 80 20 20 '(1) '(80))
                    (make-table-entry 4 'M 100 64 36 18 '(2) '(32))
                    (make-table-entry 4 'Q 100 48 52 26 '(2) '(24))
                    (make-table-entry 4 'H 100 36 64 16 '(4) '(9))
                    (make-table-entry 5 'L 134 108 26 26 '(1) '(108))
                    (make-table-entry 5 'M 134 86 48 24 '(2) '(43))
                    (make-table-entry 5 'Q 134 62 72 18 '(2 2) '(15 16))
                    (make-table-entry 5 'H 134 46 88 22 '(2 2) '(11 12))
                    (make-table-entry 6 'L 172 136 36 18 '(2) '(68))
                    (make-table-entry 6 'M 172 108 64 16 '(4) '(27))
                    (make-table-entry 6 'Q 172 76 96 24 '(4) '(19))
                    (make-table-entry 6 'H 172 60 112 28 '(4) '(15))
                    (make-table-entry 7 'L 196 156 40 20 '(2) '(78))
                    (make-table-entry 7 'M 196 124 72 18 '(4) '(31))
                    (make-table-entry 7 'Q 196 88 108 18 '(2 4) '(14 15))
                    (make-table-entry 7 'H 196 66 130 26 '(4 1) '(13 14))
                    (make-table-entry 8 'L 242 194 48 24 '(2) '(97))
                    (make-table-entry 8 'M 242 154 88 22 '(2 2) '(38 39))
                    (make-table-entry 8 'Q 242 110 132 22 '(4 2) '(18 19))
                    (make-table-entry 8 'H 242 86 156 26 '(4 2) '(14 15))
                    (make-table-entry 9 'L 292 232 60 30 '(2) '(116))
                    (make-table-entry 9 'M 292 182 110 22 '(3 2) '(36 37))
                    (make-table-entry 9 'Q 292 132 160 20 '(4 4) '(16 17))
                    (make-table-entry 9 'H 292 100 192 24 '(4 4) '(12 13))
                    (make-table-entry 10 'L 346 274 72 18 '(2 2) '(68 69))
                    (make-table-entry 10 'M 346 216 130 26 '(4 1) '(43 44))
                    (make-table-entry 10 'Q 346 154 192 24 '(6 2) '(19 20))
                    (make-table-entry 10 'H 346 122 224 28 '(6 2) '(15 16))
                    (make-table-entry 11 'L 404 324 80 20 '(4) '(81))
                    (make-table-entry 11 'M 404 254 150 30 '(1 4) '(50 51))
                    (make-table-entry 11 'Q 404 180 224 28 '(4 4) '(22 23))
                    (make-table-entry 11 'H 404 140 264 24 '(3 8) '(12 13))
                    (make-table-entry 12 'L 466 370 96 24 '(2 2) '(92 93))
                    (make-table-entry 12 'M 466 290 176 22 '(6 2) '(36 37))
                    (make-table-entry 12 'Q 466 206 260 26 '(4 6) '(20 21))
                    (make-table-entry 12 'H 466 158 308 28 '(7 4) '(14 15))
                    (make-table-entry 13 'L 532 428 104 26 '(4) '(107))
                    (make-table-entry 13 'M 532 334 198 22 '(8 1) '(37 38))
                    (make-table-entry 13 'Q 532 244 288 24 '(8 4) '(20 21))
                    (make-table-entry 13 'H 532 180 352 22 '(12 4) '(11 12))
                    (make-table-entry 14 'L 581 461 120 30 '(3 1) '(115 116))
                    (make-table-entry 14 'M 581 365 216 24 '(4 5) '(40 41))
                    (make-table-entry 14 'Q 581 261 320 20 '(11 5) '(16 17))
                    (make-table-entry 14 'H 581 197 384 24 '(11 5) '(12 13))
                    (make-table-entry 15 'L 655 523 132 22 '(5 1) '(87 88))
                    (make-table-entry 15 'M 655 415 240 24 '(5 5) '(41 42))
                    (make-table-entry 15 'Q 655 295 360 30 '(5 7) '(24 25))
                    (make-table-entry 15 'H 655 223 432 24 '(11 7) '(12 13))
                    (make-table-entry 16 'L 733 589 144 24 '(5 1) '(98 99))
                    )))
(defmacro $$test-table-entries ()
  (and
   (every #'(lambda (entry) (= (slot-value entry 'total-codewords) (+ (slot-value entry 'data-codewords) (slot-value entry 'ec-codewords)))) +version-ec-characteristics+)
   (every #'(lambda (entry) (= (slot-value entry 'ec-codewords) (* (slot-value entry 'ec-codewords-per-block) (reduce #'+ (slot-value entry 'ec-blocks))))) +version-ec-characteristics+)
   (every #'(lambda (entry) (= (slot-value entry 'data-codewords) (reduce #'+ (map 'list #'* (slot-value entry 'ec-blocks) (slot-value entry 'data-codewords-per-block))))) +version-ec-characteristics+)))
;;;; TODO fill the table............................................................

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
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:")

(defconstant +character-count-indicator-table+
  (make-array '(3 4) :element-type 'integer :initial-contents
              '((10 9 8 8)
                (12 11 16 10)
                (14 13 16 12))))

(defconstant +remainder-bits-table+
  (make-array 41 :element-type 'integer :initial-contents
              '(0 0
                7 7 7 7 7
                0 0 0 0 0 0 0
                3 3 3 3 3 3 3
                4 4 4 4 4 4 4
                3 3 3 3 3 3 3
                0 0 0 0 0 0)))

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
  (pairlis '(:numeric :alphanumeric :8-bit-byte :kanji) '(0 1 2 3)))

;; Assoc list: (error-correction-level . index).
(defconstant +ec-level->index+
  (pairlis '(:L :M :Q :H) '(0 1 2 3)))

(defconstant +pad-alternators+ '(#*11101100 #*00010001))

(defun get-characteristics-entry (version ec-level)
  (setf ec-level (cdr (assoc ec-level +ec-level->index+)))
  (aref +version-ec-characteristics+ (+ (* 4 (1- version)) ec-level)))

(defmacro with-entry ((entry (version ec-level)) &body body)
  `(let ((,entry (get-characteristics-entry ,version ,ec-level)))
     ,@body))

(defmacro with-entry-slots (slots version ec-level &body body)
  `(with-entry (entry (,version ,ec-level))
     (with-slots ,slots entry
       ,@body)))

(defun get-entry-slot (version ec-level slot)
  (with-entry (entry (version ec-level))
    (slot-value entry slot)))

(defun get-required-data-codewords (version ec-level)
  (get-entry-slot version ec-level 'data-codewords))

(defun get-ec-codewords-per-block (version ec-level)
  (get-entry-slot version ec-level 'ec-codewords-per-block))

(defun character-count-indicator-bits (mode version)
  (setf mode (cdr (assoc mode +mode->index+)))
  (cond
    ((<= 1 version 9)
     (aref +character-count-indicator-table+ 0 mode))
    ((<= 10 version 26)
     (aref +character-count-indicator-table+ 1 mode))
    ((<= 27 version 40)
     (aref +character-count-indicator-table+ 2 mode))))

(defun pair->binary (pair)
  (if (= 1 (length pair))
      (decimal->n-bit (elt pair 0) 6)
      (let ((a (elt pair 0))
            (b (elt pair 1)))
        (decimal->n-bit (+ b (* 45 a)) 11))))

(defun bits->codewords (bits version ec-level)
  "Transforms a sequence of bits in 
   (mode-indicator, character-count-indicator, encoded-data, terminator)
   to a valid concatenated sequence of 8-bit codewords and padded
   until the required data codewords quota is filled."
  (let ((required-bits (* 8 (get-required-data-codewords version ec-level)))
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

    (setf bits (reduce #'(lambda (a b) (concatenate 'bit-vector a b)) bits))
    (split-list bits 8)))

(defun data-codewords->blocks (data-codewords version ec-level)
  (with-entry-slots (ec-blocks data-codewords-per-block) version ec-level
    (let ((blocks nil)
          (codeword-idx 0))
      (loop for group-idx upto (1- (length ec-blocks))
            for group-blocks = (elt ec-blocks group-idx)
            for codewords-in-block = (elt data-codewords-per-block group-idx)
            do (setf blocks (cons
                             (loop for block-idx upto (1- group-blocks)
                                   for block = (subseq data-codewords codeword-idx (+ codeword-idx codewords-in-block))
                                   do (incf codeword-idx codewords-in-block)
                                   collect block)
                             blocks)))
      (reverse blocks))))

(defun data-codeword-blocks->ec-blocks (blocks version ec-level)
  ;; For each data block generate the appropriate amount of
  ;; error correction codewords. Returns a list of the same
  ;; structure as the given data blocks.
  (let ((ec-words (get-ec-codewords-per-block version ec-level))
        result)
    (flet ((gen-ec-words (data-codewords) (generate-ec-codewords data-codewords ec-words)))
      (loop for blocks-in-group in blocks
            collect (map 'list #'gen-ec-words blocks-in-group)))))

(defun interleave-blocks (data-blocks ec-blocks version)
  "Returns the interleaving of data-blocks and ec-blocks as a list of bit-vectors.
   If necessary, remainder bits are also added to the result."
  (flet ((interleave (blocks)
           ;; Find the length of the longest block.
           (let ((longest-len (reduce #'max (map 'list #'length blocks))))
             ;; Interleave between 'blocks', collecting each column into a list.
             (splice-list
              (loop for i from 0 to (1- longest-len)
                    collect (loop-index-value (j blk) blocks
                                              for codeword = (get-elt blk i)
                                              if codeword ; is not nil
                                              collect (get-elt blk i)))))))

    ;; First get rid of the grouping, so that we are
    ;; left with only a list of blocks.
    (setf data-blocks (splice-list data-blocks))
    (setf ec-blocks (splice-list ec-blocks))
   
    (setf data-blocks (interleave data-blocks))
    (setf ec-blocks (interleave ec-blocks))

    (let (result
          (remainder-bits (aref +remainder-bits-table+ version)))
      ;; Put interleaved EC codewords after interleaved data codewords.
      (setf result (append data-blocks ec-blocks))

      ;; Add remainder bits if necessary.
      (if (plusp remainder-bits)
        (nconc result (list (make-array remainder-bits :element-type 'bit :initial-element 0))))

      result)))

(defun encode-alphanumeric (str &key (version 1) (ec-level :q))
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
                          (character-count-indicator-bits :alphanumeric version)))

    ;; Step 5. Add Mode Indicator and Character count indicator to
    ;; binary data.
    (push char-count-bits values)
    (push (getf +mode-indicators+ :alphanumeric) values)

    ;; Step 6. Add a terminator.
    (setf required-bits (get-required-data-codewords version ec-level))
    (setf required-bits (* 8 required-bits))
    (setf total-bits (loop for bits in values
                           sum (length bits)))
    ;; The terminator is between 0 and 4 bits of zeros.
    (setf terminator-width (clamp (- required-bits total-bits) 0 4))
    (nconc values (list (make-array terminator-width :element-type 'bit :initial-element 0)))
    
    values))

(defun encode (str &key (version 1) (ec-level :q))
  (let (data-codewords
        data-blocks
        ec-blocks
        result)   

    (setf data-codewords (bits->codewords
                          (encode-alphanumeric str :version version :ec-level ec-level)
                          version ec-level))
    
    (format t "data-codewords: ~a~%" data-codewords)
    (print-bits data-codewords :hex)
    
    (setf data-blocks
          (data-codewords->blocks data-codewords version ec-level))

    (format t "data-blocks: ~a~%" data-blocks)
    
    (setf ec-blocks
          (data-codeword-blocks->ec-blocks data-blocks version ec-level))

    (format t "ec-blocks: ~a~%" ec-blocks)

    (setf result (interleave-blocks data-blocks ec-blocks version))

    (format t "result: ~a~%" result)
    (print-bits result :hex)

    ;; Return a bit-stream (single bit-vector).
    (apply #'concatenate 'bit-vector result)))
