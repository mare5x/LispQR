(in-package :mare5x.lispqr.encode)

(defparameter *encode-debug* nil)

(defmacro when-debugging (&body body)
  `(when *encode-debug* ,@body))

(defclass version-ec-level-characteristics ()
  ((version
    :initarg :version
    :accessor version)
   (ec-level
    :initarg :ec-level
    :accessor ec-level)
   (total-codewords
    :initarg :total-codewords
    :accessor total-codewords)
   (data-codewords
    :initarg :data-codewords
    :accessor data-codewords)
   (ec-codewords
    :initarg :ec-codewords
    :accessor ec-codewords)
   (ec-codewords-per-block
    :initarg :ec-codewords-per-block
    :accessor ec-codewords-per-block)
   (ec-blocks
    :initarg :ec-blocks  ; List of numbers.
    :accessor ec-blocks) 
   (data-codewords-per-block
    :initarg :data-codewords-per-block
    :accessor data-codewords-per-block)))

(defun print-characteristics (entry)
  (with-slots (version
               ec-level
               total-codewords
               data-codewords
               ec-codewords
               ec-codewords-per-block
               ec-blocks
               data-codewords-per-block)
      entry 
    (format t "~a|~a (data|ec ~a|~a -> ~a) [~a ec / block in ~a] [~a data / block]~%"
            version ec-level
            data-codewords ec-codewords total-codewords
            ec-codewords-per-block ec-blocks
            data-codewords-per-block)))

(defmacro make-ec-info-table (&rest rows)
  `(make-array ,(length rows) :element-type 'version-ec-level-characteristics :initial-contents
               ',(loop for (version
                            ec-level
                            total-codewords
                            data-codewords
                            ec-codewords
                            ec-codewords-per-block
                            ec-blocks
                            data-codewords-per-block)
                       in rows
                       collect (make-instance 'version-ec-level-characteristics
                                              :version version
                                              :ec-level ec-level
                                              :total-codewords total-codewords
                                              :data-codewords data-codewords
                                              :ec-codewords ec-codewords
                                              :ec-codewords-per-block ec-codewords-per-block
                                              :ec-blocks ec-blocks
                                              :data-codewords-per-block data-codewords-per-block))))

(defconstant +version-ec-characteristics+
  (make-ec-info-table
   (1 L 26 19 7 7 (1) (19))
   (1 M 26 16 10 10 (1) (16))
   (1 Q 26 13 13 13 (1) (13))
   (1 H 26 9 17 17 (1) (9))
   (2 L 44 34 10 10 (1) (34))
   (2 M 44 28 16 16 (1) (28))
   (2 Q 44 22 22 22 (1) (22))
   (2 H 44 16 28 28 (1) (16))
   (3 L 70 55 15 15 (1) (55))
   (3 M 70 44 26 26 (1) (44))
   (3 Q 70 34 36 18 (2) (17))
   (3 H 70 26 44 22 (2) (13))
   (4 L 100 80 20 20 (1) (80))
   (4 M 100 64 36 18 (2) (32))
   (4 Q 100 48 52 26 (2) (24))
   (4 H 100 36 64 16 (4) (9))
   (5 L 134 108 26 26 (1) (108))
   (5 M 134 86 48 24 (2) (43))
   (5 Q 134 62 72 18 (2 2) (15 16))
   (5 H 134 46 88 22 (2 2) (11 12))
   (6 L 172 136 36 18 (2) (68))
   (6 M 172 108 64 16 (4) (27))
   (6 Q 172 76 96 24 (4) (19))
   (6 H 172 60 112 28 (4) (15))
   (7 L 196 156 40 20 (2) (78))
   (7 M 196 124 72 18 (4) (31))
   (7 Q 196 88 108 18 (2 4) (14 15))
   (7 H 196 66 130 26 (4 1) (13 14))
   (8 L 242 194 48 24 (2) (97))
   (8 M 242 154 88 22 (2 2) (38 39))
   (8 Q 242 110 132 22 (4 2) (18 19))
   (8 H 242 86 156 26 (4 2) (14 15))
   (9 L 292 232 60 30 (2) (116))
   (9 M 292 182 110 22 (3 2) (36 37))
   (9 Q 292 132 160 20 (4 4) (16 17))
   (9 H 292 100 192 24 (4 4) (12 13))
   (10 L 346 274 72 18 (2 2) (68 69))
   (10 M 346 216 130 26 (4 1) (43 44))
   (10 Q 346 154 192 24 (6 2) (19 20))
   (10 H 346 122 224 28 (6 2) (15 16))
   (11 L 404 324 80 20 (4) (81))
   (11 M 404 254 150 30 (1 4) (50 51))
   (11 Q 404 180 224 28 (4 4) (22 23))
   (11 H 404 140 264 24 (3 8) (12 13))
   (12 L 466 370 96 24 (2 2) (92 93))
   (12 M 466 290 176 22 (6 2) (36 37))
   (12 Q 466 206 260 26 (4 6) (20 21))
   (12 H 466 158 308 28 (7 4) (14 15))
   (13 L 532 428 104 26 (4) (107))
   (13 M 532 334 198 22 (8 1) (37 38))
   (13 Q 532 244 288 24 (8 4) (20 21))
   (13 H 532 180 352 22 (12 4) (11 12))
   (14 L 581 461 120 30 (3 1) (115 116))
   (14 M 581 365 216 24 (4 5) (40 41))
   (14 Q 581 261 320 20 (11 5) (16 17))
   (14 H 581 197 384 24 (11 5) (12 13))
   (15 L 655 523 132 22 (5 1) (87 88))
   (15 M 655 415 240 24 (5 5) (41 42))
   (15 Q 655 295 360 30 (5 7) (24 25))
   (15 H 655 223 432 24 (11 7) (12 13))
   (16 L 733 589 144 24 (5 1) (98 99))
   (16 M 733 453 280 28 (7 3) (45 46))
   (16 Q 733 325 408 24 (15 2) (19 20))
   (16 H 733 253 480 30 (3 13) (15 16))
   (17 L 815 647 168 28 (1 5) (107 108))
   (17 M 815 507 308 28 (10 1) (46 47))
   (17 Q 815 367 448 28 (1 15) (22 23))
   (17 H 815 283 532 28 (2 17) (14 15))
   (18 L 901 721 180 30 (5 1) (120 121))
   (18 M 901 563 338 26 (9 4) (43 44))
   (18 Q 901 397 504 28 (17 1) (22 23))
   (18 H 901 313 588 28 (2 19) (14 15))
   (19 L 991 795 196 28 (3 4) (113 114))
   (19 M 991 627 364 26 (3 11) (44 45))
   (19 Q 991 445 546 26 (17 4) (21 22))
   (19 H 991 341 650 26 (9 16) (13 14))
   (20 L 1085 861 224 28 (3 5) (107 108))
   (20 M 1085 669 416 26 (3 13) (41 42))
   (20 Q 1085 485 600 30 (15 5) (24 25))
   (20 H 1085 385 700 28 (15 10) (15 16))
   (21 L 1156 932 224 28 (4 4) (116 117))
   (21 M 1156 714 442 26 (17) (42))
   (21 Q 1156 512 644 28 (17 6) (22 23))
   (21 H 1156 406 750 30 (19 6) (16 17))
   (22 L 1258 1006 252 28 (2 7) (111 112))
   (22 M 1258 782 476 28 (17) (46))
   (22 Q 1258 568 690 30 (7 16) (24 25))
   (22 H 1258 442 816 24 (34) (13))
   (23 L 1364 1094 270 30 (4 5) (121 122))
   (23 M 1364 860 504 28 (4 14) (47 48))
   (23 Q 1364 614 750 30 (11 14) (24 25))
   (23 H 1364 464 900 30 (16 14) (15 16))
   (24 L 1474 1174 300 30 (6 4) (117 118))
   (24 M 1474 914 560 28 (6 14) (45 46))
   (24 Q 1474 664 810 30 (11 16) (24 25))
   (24 H 1474 514 960 30 (30 2) (16 17))
   (25 L 1588 1276 312 26 (8 4) (106 107))
   (25 M 1588 1000 588 28 (8 13) (47 48))
   (25 Q 1588 718 870 30 (7 22) (24 25))
   (25 H 1588 538 1050 30 (22 13) (15 16))
   (26 L 1706 1370 336 28 (10 2) (114 115))
   (26 M 1706 1062 644 28 (19 4) (46 47))
   (26 Q 1706 754 952 28 (28 6) (22 23))
   (26 H 1706 596 1110 30 (33 4) (16 17))
   (27 L 1828 1468 360 30 (8 4) (122 123))
   (27 M 1828 1128 700 28 (22 3) (45 46))
   (27 Q 1828 808 1020 30 (8 26) (23 24))
   (27 H 1828 628 1200 30 (12 28) (15 16))
   (28 L 1921 1531 390 30 (3 10) (117 118))
   (28 M 1921 1193 728 28 (3 23) (45 46))
   (28 Q 1921 871 1050 30 (4 31) (24 25))
   (28 H 1921 661 1260 30 (11 31) (15 16))
   (29 L 2051 1631 420 30 (7 7) (116 117))
   (29 M 2051 1267 784 28 (21 7) (45 46))
   (29 Q 2051 911 1140 30 (1 37) (23 24))
   (29 H 2051 701 1350 30 (19 26) (15 16))
   (30 L 2185 1735 450 30 (5 10) (115 116))
   (30 M 2185 1373 812 28 (19 10) (47 48))
   (30 Q 2185 985 1200 30 (15 25) (24 25))
   (30 H 2185 745 1440 30 (23 25) (15 16))
   (31 L 2323 1843 480 30 (13 3) (115 116))
   (31 M 2323 1455 868 28 (2 29) (46 47))
   (31 Q 2323 1033 1290 30 (42 1) (24 25))
   (31 H 2323 793 1530 30 (23 28) (15 16))
   (32 L 2465 1955 510 30 (17) (115))
   (32 M 2465 1541 924 28 (10 23) (46 47))
   (32 Q 2465 1115 1350 30 (10 35) (24 25))
   (32 H 2465 845 1620 30 (19 35) (15 16))
   (33 L 2611 2071 540 30 (17 1) (115 116))
   (33 M 2611 1631 980 28 (14 21) (46 47))
   (33 Q 2611 1171 1440 30 (29 19) (24 25))
   (33 H 2611 901 1710 30 (11 46) (15 16))
   (34 L 2761 2191 570 30 (13 6) (115 116))
   (34 M 2761 1725 1036 28 (14 23) (46 47))
   (34 Q 2761 1231 1530 30 (44 7) (24 25))
   (34 H 2761 961 1800 30 (59 1) (16 17))
   (35 L 2876 2306 570 30 (12 7) (121 122))
   (35 M 2876 1812 1064 28 (12 26) (47 48))
   (35 Q 2876 1286 1590 30 (39 14) (24 25))
   (35 H 2876 986 1890 30 (22 41) (15 16))
   (36 L 3034 2434 600 30 (6 14) (121 122))
   (36 M 3034 1914 1120 28 (6 34) (47 48))
   (36 Q 3034 1354 1680 30 (46 10) (24 25))
   (36 H 3034 1054 1980 30 (2 64) (15 16))
   (37 L 3196 2566 630 30 (17 4) (122 123))
   (37 M 3196 1992 1204 28 (29 14) (46 47))
   (37 Q 3196 1426 1770 30 (49 10) (24 25))
   (37 H 3196 1096 2100 30 (24 46) (15 16))
   (38 L 3362 2702 660 30 (4 18) (122 123))
   (38 M 3362 2102 1260 28 (13 32) (46 47))
   (38 Q 3362 1502 1860 30 (48 14) (24 25))
   (38 H 3362 1142 2220 30 (42 32) (15 16))
   (39 L 3532 2812 720 30 (20 4) (117 118))
   (39 M 3532 2216 1316 28 (40 7) (47 48))
   (39 Q 3532 1582 1950 30 (43 22) (24 25))
   (39 H 3532 1222 2310 30 (10 67) (15 16))
   (40 L 3706 2956 750 30 (19 6) (118 119))
   (40 M 3706 2334 1372 28 (18 31) (47 48))
   (40 Q 3706 1666 2040 30 (34 34) (24 25))
   (40 H 3706 1276 2430 30 (20 61) (15 16))))

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

;; The character at the i-th index has a value of i.
(defconstant +alphanumeric-table+
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:")

;;                | Numeric mode | Alphanumeric mode | Byte mode | Kanji mode
;; Version 1 to 9 |
;;       10 to 26 |
;;       27 to 40 | 
(defconstant +character-count-indicator-table+
  #2A ((10 9 8 8)
       (12 11 16 10)
       (14 13 16 12)))

(defconstant +remainder-bits-table+
  #(0 0
    7 7 7 7 7
    0 0 0 0 0 0 0
    3 3 3 3 3 3 3
    4 4 4 4 4 4 4
    3 3 3 3 3 3 3
    0 0 0 0 0 0))

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

(defun alnum-pair->binary (pair)
  (if (= 1 (length pair))
      (decimal->n-bit (elt pair 0) 6)
      (let ((a (elt pair 0))
            (b (elt pair 1)))
        (decimal->n-bit (+ b (* 45 a)) 11))))

(defun encoded-parts->codewords (bits version ec-level)
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
  (let ((ec-words (get-ec-codewords-per-block version ec-level)))
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

(defun encode-character-count-indicator (str version encoding-mode)
  (decimal->n-bit (length str)
                  (character-count-indicator-bits encoding-mode version)))

(defun encode-mode-indicator (encoding-mode)
  (getf +mode-indicators+ encoding-mode))

(defun encode-terminator (parts version ec-level)
  "Calculate required terminator bits. 
   parts is a sequence that contains the mode indicator and encoded data."
  (let ((required-bits (* 8 (get-required-data-codewords version ec-level)))
        (total-bits (loop for bits in parts
                          sum (length bits))))
    ;; The terminator is between 0 and 4 bits of zeros.
    (list (make-array (clamp (- required-bits total-bits) 0 4)
                      :element-type 'bit :initial-element 0))))

(defun encode-add-metadata (encoded-data str version ec-level encoding-mode)
  "Add Mode Indicator, Character count indicator and terminator to binary data.
   Returns a list of codewords."
  (let ((parts (list encoded-data))) 
    (push (encode-character-count-indicator str version encoding-mode)
          parts)
    (push (encode-mode-indicator encoding-mode) parts)
    (nconc parts (encode-terminator parts version ec-level))
    (encoded-parts->codewords parts version ec-level)))

(defun encode-alphanumeric (str)
  "Encodes the given string 'str' using alphanumeric mode."
  (let ((values nil))
    ;; Step 1. Determine character values according to translation table.
    ;; The value of each character is it's position (index) in the table.
    (setf values (map 'vector #'(lambda (ch) (position ch +alphanumeric-table+)) str))

    ;; Step 2. Divide the result into groups of two decimal values.
    (setf values (split-list values 2))

    ;; Step 3. Convert each group to its 11-bit binary equivalent.
    (setf values (loop for pair in values
                       collect (alnum-pair->binary pair)))
    ;; Concatenate the bit vectors into a single bit vector:
    ;; Now we have the one element list: (encoded-data).
    (splice-list values 'bit-vector)))

(defun encode-bytes (str)
  "Encodes the given string 'str' using Byte mode.
   The character set used is: ISO/IEC 8859-1, which is very similar to Unicode.
   As such encoding is very simple."
  (flet ((encode-to-byte (ch)
           (decimal->8-bit (char-code ch))))
    (splice-list (map 'list #'encode-to-byte str) 'bit-vector)))

(defun encode-string-using-mode (str encoding-mode)
  (ecase encoding-mode
    (:alphanumeric (encode-alphanumeric str))
    (:8-bit-byte (encode-bytes str))))

(defun encode-codewords (str version ec-level encoding-mode)
  (encode-add-metadata (encode-string-using-mode str encoding-mode)
                       str version ec-level encoding-mode))

(defun encode (str version ec-level encoding-mode)
  "Completes the encoding necessary to fill in the QR matrix.

   Assumes the given version, error correction level and encoding mode
   are compatible with the given string."
  
  (let (data-codewords
        data-blocks
        ec-blocks
        result)
    (setf data-codewords (encode-codewords str version ec-level encoding-mode))

    (when-debugging
      (format t "data-codewords: ~a~%" data-codewords)
      (print-bits data-codewords :hex))
    
    (setf data-blocks
          (data-codewords->blocks data-codewords version ec-level))

    (when-debugging (format t "data-blocks: ~a~%" data-blocks))
    
    (setf ec-blocks
          (data-codeword-blocks->ec-blocks data-blocks version ec-level))

    (when-debugging (format t "ec-blocks: ~a~%" ec-blocks))

    (setf result (interleave-blocks data-blocks ec-blocks version))

    (when-debugging
      (format t "result: ~a~%" result)
      (print-bits result :hex))

    ;; Return a bit-stream (single bit-vector).
    (apply #'concatenate 'bit-vector result)))

(defun alphanumeric-bit-stream-length (str version)
  (+ (length (encode-mode-indicator :alphanumeric))
     (character-count-indicator-bits :alphanumeric version)
     (* 11 (floor-div (length str) 2))
     (* 6 (mod (length str) 2))))

(defun byte-bit-stream-length (str version)
  (+ (length (encode-mode-indicator :8-bit-byte))
     (character-count-indicator-bits :8-bit-byte version)
     (* 8 (length str))))

(defun bit-stream-length (str version encoding-mode)
  (ecase encoding-mode
    (:8-bit-byte (byte-bit-stream-length str version))
    (:alphanumeric (alphanumeric-bit-stream-length str version))))

(defun get-min-version (str min-version ec-level encoding-mode)
  "Returns the minimum version that fits the given string, encoded
   using encoding-mode and using the specified error correction level."

  (loop for version from min-version to 40
        for len = (bit-stream-length str version encoding-mode)
        for entry = (get-characteristics-entry version ec-level)
        while (< (* 8 (data-codewords entry)) len)
        finally (return version)))

(defun get-min-version-and-ec-level (str min-version encoding-mode)
  "Returns the highest error correction level and lowest version
   that encodes the given string."

  (loop for version from min-version to 40 do
        (loop for ec-level in '(:H :Q :M :L)
              for entry = (get-characteristics-entry version ec-level)
              for len = (bit-stream-length str version encoding-mode)
              do (if (< len (* 8 (data-codewords entry)))
                     (return-from get-min-version-and-ec-level (values version ec-level))))))

(defun encodable-alphanumerically? (str)
  (every #'(lambda (c) (position c +alphanumeric-table+)) str))

(defun encodable-byte-mode? (str)
  (every #'(lambda (c) (<= (char-code c) 255)) str))

(defun get-best-encoding-mode (str)
  (cond ((encodable-alphanumerically? str) :alphanumeric)
        ((encodable-byte-mode? str) :8-bit-byte)
        (t (error "Cannot encode string! ~a" str))))

(defclass encoding-parameters-t () 
  ((version :initarg :version)
   (ec-level :initarg :ec-level)
   (encoding-mode :initarg :encoding-mode)))

(defun get-encoding-parameters (str &key (min-version 1) ec-level encoding-mode)
  "Picks the 'best' encoding parameters to encode the given string, prioritizing lower
   versions and higher error correction levels."
  
  (setf encoding-mode (if encoding-mode
                          encoding-mode
                          (get-best-encoding-mode str)))
  (if ec-level
      (setf min-version (get-min-version str min-version ec-level encoding-mode))
      (setf (values min-version ec-level) (get-min-version-and-ec-level str min-version encoding-mode)))
  (make-instance 'encoding-parameters-t
                 :version min-version
                 :ec-level ec-level 
                 :encoding-mode encoding-mode))

(defun encode->image (str path &key (min-version 1) ec-level encoding-mode (mask-number -1))
  "Encode the string 'str' and save it to 'path' (png format). 

   Optionally specify the minimum version to be used 'min-version' (1-40).
   Error correction level 'ec-level' can be one of (:L, :M, :Q, :H) as defined by the standard. 
   The 'encoding-mode' must be in (:alphanumeric :8-bit-byte). 
   To force the use of a specific masking pattern, use 'mask-number' (0-7). 

   If optional parameters are omitted, appropriate values will be automatically computed."
  
  (with-slots (version ec-level encoding-mode)
      (get-encoding-parameters str
                               :min-version min-version
                               :ec-level ec-level
                               :encoding-mode encoding-mode)
    (format t "Encoding \"~a\"~%" str)
    (format t "Version ~a~%" version)
    (format t "Error correction level ~a~%" ec-level)
    (format t "Encoding mode ~a~%" encoding-mode)
    (format t "Mask-number ~a~%" mask-number)
    (write-qr-matrix path 
                     (make-qr-matrix
                      (encode str version ec-level encoding-mode)
                      version
                      ec-level
                      mask-number))))

