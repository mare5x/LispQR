;;;; Module placement in matrix.

;;; The QR matrix shall be M by M modules in size,
;;; where M is given by version-size.
;;; In the matrix a value of 1 shall denote a dark module
;;; and a value of 0 a white module.
;;; Indexing:
;;; Top left is (0, 0), bottom right is (M-1, M-1).
;;; NOTE: the standard calls the QR matrix the 'QR Code symbol'.

(in-package :mare5x.lispqr.matrix)

;; These are the row/column coordinates of the center module
;; of each alignment pattern. For each version there is a list.
(defconstant +alignment-pattern-locations+
  #1A (()
       ()
       (6 18)
       (6 22)
       (6 26)
       (6 30)
       (6 34)
       (6 22 38)
       (6 24 42)
       (6 26 46)
       (6 28 50)
       (6 30 54)
       (6 32 58)
       (6 34 62)
       (6 26 46 66)
       (6 26 48 70)
       (6 26 50 74)
       (6 30 54 78)
       (6 30 56 82)
       (6 30 58 86)
       (6 34 62 90)
       (6 28 50 72 94)
       (6 26 50 74 98)
       (6 30 54 78 102)
       (6 28 54 80 106)
       (6 32 58 84 110)
       (6 30 58 86 114)
       (6 34 62 90 118)
       (6 26 50 74 98 122)
       (6 30 54 78 102 126)
       (6 26 52 78 104 130)
       (6 30 56 82 108 134)
       (6 34 60 86 112 138)
       (6 30 58 86 114 142)
       (6 34 62 90 118 146)
       (6 30 54 78 102 126 150)
       (6 24 50 76 102 128 154)
       (6 28 54 80 106 132 158)
       (6 32 58 84 110 136 162)
       (6 26 54 82 110 138 166)
       (6 30 58 86 114 142 170)))

(defconstant +finder-pattern+
  #2A (#*1111111
       #*1000001
       #*1011101
       #*1011101
       #*1011101
       #*1000001
       #*1111111))

(defconstant +alignment-pattern+
  #2A (#*11111
       #*10001
       #*10101
       #*10001
       #*11111))

(defmacro loop-submatrix (((row col) (&optional (sub-row (gensym)) (sub-col (gensym))))
                          (sub-dimensions &optional (position '(0 0)))
                          &body do-body)
  (let ((sn (gensym))
        (sm (gensym))
        (y (gensym))
        (x (gensym)))
    
    `(destructuring-bind ((,sn ,sm) (,y ,x)) `(,,sub-dimensions ,,position)
       (loop for ,sub-row from 0 below ,sn
             for ,row = (+ ,y ,sub-row)
             do (loop for ,sub-col from 0 below ,sm
                      for ,col = (+ ,x ,sub-col)
                      do ,@do-body)))))

(defun fill-submatrix (matrix sub-matrix &optional (position '(0 0)))
  "Place sub-matrix into matrix at position."
  (loop-submatrix ((row col) (sub-row sub-col)) ((array-dimensions sub-matrix) position)
    (setf (aref matrix row col) (aref sub-matrix sub-row sub-col)))
  matrix)

(defun mark-submatrix (matrix sub-dimensions &optional (position '(0 0)))
  "Mark matrix's sub-dimensions with 1s at the given position."
  (loop-submatrix ((row col) nil) (sub-dimensions position)
    (setf (aref matrix row col) 1))
  matrix)

(defun fill&mark-submatrix (matrix marked sub-matrix &optional (position '(0 0)))
  (fill-submatrix matrix sub-matrix position)
  (mark-submatrix marked (array-dimensions sub-matrix) position))

(defun is-marked-submatrix (matrix sub-dimensions &optional (position '(0 0)))
  "Check if there are any 1s at the given sub-dimensions at position in matrix."
  (loop-submatrix ((row col) nil) (sub-dimensions position)
    (if (= 1 (aref matrix row col))
        (return-from is-marked-submatrix t)))
  nil)

(defun is-fully-marked-submatrix (matrix sub-dimensions position &optional (value 1))
  "Check if all values in sub-matrix are of the given value."
  (loop-submatrix ((row col) nil) (sub-dimensions position)
    (if (/= value (aref matrix row col))
        (return-from is-fully-marked-submatrix nil)))
  t)

(defun match-submatrix (matrix sub-matrix position)
  (loop-submatrix ((row col) (sub-row sub-col)) ((array-dimensions sub-matrix) position)
    (if (out-of-bounds (array-dimensions matrix) row col)
        (return-from match-submatrix))
    (if (/= (aref matrix row col)
            (aref sub-matrix sub-row sub-col))
        (return-from match-submatrix)))
  t)

(defun out-of-bounds (dimensions row col)
  (destructuring-bind (rows cols) dimensions
    (or (< row 0)
        (< col 0)
        (>= row rows)
        (>= col cols))))

(defun matrix-size (matrix)
  (array-dimension matrix 0))

(defun version-size (version)
  "Returns the size of the QR code (in modules)."
  (+ 21 (* 4 (1- version))))

(defun init-matrix (size &key (initial-element 0))
  (make-array (list size size) :element-type 'bit :initial-element initial-element))

(defun copy-matrix (matrix)
  (let* ((size (matrix-size matrix))
         (new-matrix (init-matrix size)))
    (dotimes (i (array-total-size matrix))
      (setf (row-major-aref new-matrix i)
            (row-major-aref matrix i)))
    new-matrix))

(defun add-finder-patterns (matrix marked)
  (let ((n (matrix-size matrix))
        (pattern-size (array-dimension +finder-pattern+ 0)))
    ;; In addition to the finder patterns, also mark the separators.
    ;; The separators don't have to be filled in, since they are 0s.
    ;; Top left at (0, 0):
    (fill-submatrix matrix +finder-pattern+ '(0 0))
    (mark-submatrix marked `(,(1+ pattern-size) ,(1+ pattern-size)) '(0 0))
    ;; Top right:
    (fill-submatrix matrix +finder-pattern+ `(0 ,(- n pattern-size)))
    (mark-submatrix marked `(,(1+ pattern-size) ,(1+ pattern-size)) `(0 ,(- n pattern-size 1)))
    ;; Bottom left:
    (fill-submatrix matrix +finder-pattern+ `(,(- n pattern-size) 0))
    (mark-submatrix marked `(,(1+ pattern-size) ,(1+ pattern-size)) `(,(- n pattern-size 1) 0)))
  matrix)

(defun add-alignment-patterns (matrix marked version)
  (loop for middle-row in (aref +alignment-pattern-locations+ version)
        with pattern-size = (array-dimension +alignment-pattern+ 0)
        with pattern-size/2 = (floor-div pattern-size 2)
        with pattern-dim = `(,pattern-size ,pattern-size)
        for top-left-row = (- middle-row pattern-size/2)
        do (loop for middle-col in (aref +alignment-pattern-locations+ version)
                 for top-left-col = (- middle-col pattern-size/2)
                 for pos = `(,top-left-row ,top-left-col)
                 do (if (not (is-marked-submatrix marked pattern-dim pos))
                        (fill&mark-submatrix matrix marked +alignment-pattern+ pos)))))

(defun add-timing-patterns (matrix marked)
  ;; The horizontal starts at the fixed 6th row.
  ;; The vertical starts at the fixed 6th col.
  (let ((size (matrix-size matrix)))
    (loop for col from 0 below size
          with row = 6
          for color-bit = (mod (1+ col) 2)
          for marked-bit = (aref marked row col)
          do (when (= 0 marked-bit)
               (setf (aref matrix row col) color-bit)
               (setf (aref marked row col) 1)))
    (loop for row from 0 below size
          with col = 6
          for color-bit = (mod (1+ row) 2)
          for marked-bit = (aref marked row col)
          do (when (= 0 marked-bit)
               (setf (aref matrix row col) color-bit)
               (setf (aref marked row col) 1))))
  matrix)

(defun add-dark-module (matrix marked version)
  ;; Next to bottom left finder, using a direct formula.
  (let ((row (+ 9 (* 4 version)))
        (col 8))
    (setf (aref matrix row col) 1)
    (setf (aref marked row col) 1))
  matrix)

(defun reserve-format-information (marked)
  (let ((size (matrix-size marked))
        (pattern-size (array-dimension +finder-pattern+ 0)))
    ;; Top left:
    (mark-submatrix marked `(1 ,(+ 2 pattern-size)) `(,(1+ pattern-size) 0))
    (mark-submatrix marked `(,(+ 2 pattern-size) 1) `(0 ,(1+ pattern-size)))
    ;; Top right:
    (mark-submatrix marked `(1 ,(1+ pattern-size)) `(,(1+ pattern-size) ,(- size pattern-size 1)))
    ;; Bottom left:
    (mark-submatrix marked `(,pattern-size 1) `(,(- size pattern-size) ,(1+ pattern-size))))
  marked)

(defun reserve-version-information (marked version)
  (if (< version 7)
      (return-from reserve-version-information marked))
  
  (let ((size (matrix-size marked))
        (pattern-size (array-dimension +finder-pattern+ 0)))
    ;; Top right (6x3):
    (mark-submatrix marked '(6 3) `(0 ,(- size pattern-size 4)))
    ;; Bottom left (3x6):
    (mark-submatrix marked '(3 6) `(,(- size pattern-size 4) 0)))
  marked)

(defun add-data-bits (matrix marked data)
  ;; 'data' is a bit vector (a stream of bits).
  (loop named outer
        with n = (matrix-size matrix)
        with vertical-timing-col = (1- (array-dimension +finder-pattern+ 0))
        with col = (1- n)
        with bit-ctr = 0
        with data-size = (length data)
        while (< bit-ctr data-size)
        do
        ;; Zig-zag upwards.
        (loop for row from (1- n) downto 0 do
              (loop for dx in '(0 -1)
                    for cur-col = (+ col dx)
                    for marked-bit = (aref marked row cur-col)
                    do (when (= 0 marked-bit)  ; skip marked bits
                         (setf (aref marked row cur-col) 1)
                         (setf (aref matrix row cur-col) (elt data bit-ctr))
                         (incf bit-ctr)))
              (if (>= bit-ctr data-size)
                  (return-from outer)))

        ;; Move to next column.
        (decf col 2)
        (if (<= 0 (- col vertical-timing-col) 1)  ; Exception: vertical timing pattern.
            (decf col))  
        
        ;; Zig-zag downwards.
        (loop for row from 0 below n do
              (loop for dx in '(0 -1)
                    for cur-col = (+ col dx)
                    for marked-bit = (aref marked row cur-col)
                    do (when (= 0 marked-bit)  ; skip marked bits
                         (setf (aref marked row cur-col) 1)
                         (setf (aref matrix row cur-col) (elt data bit-ctr))
                         (incf bit-ctr)))
              (if (>= bit-ctr data-size)
                  (return-from outer)))
        
        ;; Move to next column.
        (decf col 2)
        (if (<= 0 (- col vertical-timing-col) 1)  ; Exception: vertical timing pattern.
            (decf col)))
  matrix)

(defun mask-pattern (matrix test-fn)
  ;; The masking region must only be applied to data and error correction modules.
  ;; Use test-fn to specify all conditions that should apply for masking to take effect
  ;; at any given (row col).
  (let ((masked (copy-matrix matrix))
        (size (matrix-size matrix)))
    (loop for row from 0 below size do
          (loop for col from 0 below size do
                (if (funcall test-fn row col)
                    (setf (aref masked row col)
                          (logxor 1 (aref masked row col))))))
    masked))

(defun mask-pattern-test-fn (mask-number)
  (case mask-number
    (0 #'(lambda (row col) (zerop (mod (+ row col) 2))))
    (1 #'(lambda (row col) (zerop (mod row 2))))
    (2 #'(lambda (row col) (zerop (mod col 3))))
    (3 #'(lambda (row col) (zerop (mod (+ row col) 3))))
    (4 #'(lambda (row col) (zerop (mod (+ (floor-div row 2)
                                          (floor-div col 3))
                                       2))))
    (5 #'(lambda (row col) (zerop (+ (mod (* row col) 2)
                                     (mod (* row col) 3)))))
    (6 #'(lambda (row col) (zerop (mod (+ (mod (* row col) 2)
                                          (mod (* row col) 3))
                                       2))))
    (7 #'(lambda (row col) (zerop (mod (+ (mod (+ row col) 2)
                                          (mod (* row col) 3))
                                       2))))))

(defun mask-pattern-region (matrix protected-region mask-number)
  ;; Wrap each test-fn to also take the protected-region into account.
  (flet ((test-fn (row col) (and (zerop (aref protected-region row col))
                                 (funcall (mask-pattern-test-fn mask-number)
                                          row col))))
    (mask-pattern matrix #'test-fn)))

(defun eval-mask-penalty-rule-1 (matrix)
  ;; Rule 1: row & column penalty.
  (let ((size (matrix-size matrix))
        (penalty 0))
    ;; Row penalties.
    (loop for row below size
          for consecutive-count = 0
          for prev-bit = -1
          do (loop for col below size
                   for bit = (aref matrix row col)
                   do (if (= prev-bit bit)
                          (incf consecutive-count)
                          (progn (if (>= consecutive-count 5)
                                     (incf penalty (- consecutive-count 2)))
                                 (setf consecutive-count 1)))
                      (setf prev-bit bit)))
    
    ;; Column penalties.
    (loop for col below size
          for consecutive-count = 0
          for prev-bit = -1
          do (loop for row below size
                   for bit = (aref matrix row col)
                   do (if (= prev-bit bit)
                          (incf consecutive-count)
                          (progn (if (>= consecutive-count 5)
                                     (incf penalty (- consecutive-count 2)))
                                 (setf consecutive-count 1)))
                      (setf prev-bit bit)))

    (format t "Rule1 penalty: ~a~%" penalty)
    penalty))

(defun eval-mask-penalty-rule-2 (matrix)
  ;; Block modules in same color.
  ;; Note: the standard is very ambiguous regarding
  ;; this section (m x n blocks). I will count each
  ;; 2 x 2 block instead (as was done by thonky).
  (let ((size (matrix-size matrix))
        (penalty 0))
    (loop for row below (1- size) do
          (loop for col below (1- size)
                for bit = (aref matrix row col)
                do (when (is-fully-marked-submatrix matrix '(2 2) `(,row ,col) bit)
                     (format t "Found 2x2 pattern at (~a ~a)~%" row col)
                     (incf penalty 3))))
    (format t "Rule2 penalty: ~a~%" penalty)
    penalty))

(defun eval-mask-penalty-rule-3 (matrix)
  ;; Dark-light patterns.
  ;; Note: the standard says to look for 1011101,
  ;; whereas thonky says to look for either 10111010000
  ;; or 00001011101. The latter way seems more reasonable.
  (let ((size (matrix-size matrix))
        (penalty 0))
    (loop with horizontal-patterns = `(,#2a (#*10111010000) ,#2a (#*00001011101))
          with vertical-patterns = `(,#2a (#*1 #*0 #*1 #*1 #*1 #*0 #*1 #*0 #*0 #*0 #*0)
                                          ,#2a (#*0 #*0 #*0 #*0 #*1 #*0 #*1 #*1 #*1 #*0 #*1))
          for row below size do
          (loop for col below size do
                (when (some #'(lambda (pattern) (match-submatrix matrix pattern `(,row ,col)))
                            horizontal-patterns)
                  (format t "Found horizontal pattern at (~a ~a)~%" row col)
                  (incf penalty 40))
                (when (some #'(lambda (pattern) (match-submatrix matrix pattern `(,row ,col)))
                            vertical-patterns)
                  (format t "Found vertical pattern at (~a ~a)~%" row col)
                  (incf penalty 40))))

    (format t "Rule3 penalty ~a~%" penalty)
    penalty))

(defun eval-mask-penalty-rule-4 (matrix)
  ;; Proportion of dark modules.
  (let* ((size (matrix-size matrix))
         (total-modules (* size size))
         (dark-modules 0)
         (k 0)
         proportion)
    (loop for row below size do
          (loop for col below size
                for bit = (aref matrix row col)
                do (if (= bit 1)
                       (incf dark-modules))))
    
    (setf proportion (* 100 (/ dark-modules total-modules)))
    (setf k (if (<= proportion 50)
                (- 9 (floor proportion 5))
                (- (ceiling proportion 5) 11)))
    (format t "Proportion of dark modules: ~$~%" proportion)
    (format t "Rule4 penalty ~a~%" (* 10 k))
    (* 10 k)))

(defun eval-mask-penalty (matrix)
  (format t "Evaluating penalties ...~%")
  (print-2d-array matrix)
  (+ (eval-mask-penalty-rule-1 matrix)
     (eval-mask-penalty-rule-2 matrix)
     (eval-mask-penalty-rule-3 matrix)
     (eval-mask-penalty-rule-4 matrix)))

(defun best-mask-pattern (matrix protected-region)
  (loop for mask-number below 8
        with min-penalty = most-positive-fixnum
        with min-matrix = nil
        with best-mask = 0

        for masked = (mask-pattern-region matrix protected-region mask-number)
        for penalty = (eval-mask-penalty masked)

        do (when (< penalty min-penalty)
             (setf min-penalty penalty)
             (setf min-matrix masked)
             (setf best-mask mask-number))

        finally (format t "Lowest penalty (~a) using pattern ~a~%" min-penalty best-mask)
                (print-2d-array min-matrix)
                (return min-matrix)))

(defun make-qr-matrix (data version)
  ;; Use 'matrix' to place modules.
  ;; Use 'marked' to mark which places in matrix have already been assigned.
  (let ((matrix (init-matrix (version-size version)))
        (marked (init-matrix (version-size version)))
        (mask-protected-region nil))
    (add-finder-patterns matrix marked)
    (print-2d-array marked)
    (add-alignment-patterns matrix marked version)
    (format t "~%")
    (print-2d-array marked)
    (add-timing-patterns matrix marked)
    (format t "~%")
    (print-2d-array marked)
    (add-dark-module matrix marked version)
    (format t "~%")
    (print-2d-array marked)
    
    ;; Only reserve the required regions, without actually
    ;; placing any modules. The regions will be filled in later.
    ;; This is necessary so that the data placing step can be performed.
    (reserve-format-information marked)
    (format t "~%")
    (print-2d-array marked)
    (reserve-version-information marked version)
    (format t "~%")
    (print-2d-array marked)

    ;; At this point 'marked' has marked everything that should NOT be
    ;; masked. Therefore we can simply use 'marked' when masking ...
    (setf mask-protected-region (copy-matrix marked))

    (add-data-bits matrix marked data)
    (format t "~%")
    (print-2d-array marked)
    (format t "~%")
    (print-2d-array matrix)

    ;; Note: ISO/IEC 18004:2000 is clear that masking should be done
    ;; on the encoding region of the symbol excluding the Format Information.
    ;; And the penalty evaluation area is the complete symbol. However,
    ;; https://www.nayuki.io/page/creating-a-qr-code-step-by-step draws
    ;; the format bits BEFORE evaluating the penalty. I will trust in the
    ;; standard and thonky.com. However, this doesn't make much sense ...
    (setf matrix (best-mask-pattern matrix mask-protected-region))
    (format t "~%")
    (print-2d-array marked)
    (format t "~%")
    (print-2d-array matrix)

    matrix))
