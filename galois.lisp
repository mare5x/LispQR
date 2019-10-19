;;;; Galois field GF(256) arithmetic.

(in-package :mare5x.lispqr.galois)

(defconstant +MOD-CONST+ #b100011101)

(defmacro generate-powers-of-2 (&optional (n 256))
  ;; Generate the powers of 2 at compile time.
  ;; The result must be quoted, because the resulting
  ;; list is not self-evaluating.
  `(quote ,(loop repeat n
                 for prev = 1 then (* 2 prev)
                 do (if (> prev (1- n))
                        (setf prev (logxor prev +MOD-CONST+)))
                 collect prev)))

(defconstant +POWERS-OF-2+ (generate-powers-of-2))

(defun int->power (a)
  (position a +powers-of-2+))

(defun add (&rest integers)
  (apply #'logxor integers))

(defun sub (&rest integers)
  (apply #'add integers))

(defun mul-exponents (a b)
  (mod (+ a b) 255))

(defun mul (a b)
  (if (or (= 0 a) (= 0 b))
      0
      (elt +powers-of-2+ (mul-exponents (int->power a) (int->power b)))))

;; Polynomial representation:
;; f(x) = c0 + c1*x1 + ... + cn*xn
;; where ci is of the form 2^ki or 0.
;; Put f into a list like so: (c0 c1 ... cn).
;; Note: negative numbers don't exist in this field.

(defun polynom-trim (f)
  "Remove excess zero coefficients."
  (let ((j (1- (length f))))
    (loop while (and (>= j 0) (= 0 (elt f j))) do (decf j))
    (if (>= j 0)
        (subseq f 0 (1+ j))
        f)))

(defun order (f)
  (1- (length f)))

(defun polynom->alpha (f)
  (map 'list #'(lambda (a) (int->power a)) f))

(defun alpha->polynom (f)
  (map 'list #'(lambda (a) (elt +powers-of-2+ a)) f))

(defun polynom-mul (f g)
  ;; The order of f*g is ord(f) + ord(g).
  ;; A polynomial of order n has up to n + 1 coefficients.
  (let ((result (make-array (+ (order f) (order g) 1) :element-type 'integer)))
    (loop-index-value (i a) f
      do (loop-index-value (j b) g
           for exponent = (mul-exponents i j)
           do (setf (elt result exponent) (add (mul a b) (elt result exponent)))))
    result))

(defun polynom-mul-scalar (f alpha)
  (polynom-mul f (list alpha)))

(defun polynom-mul-x^n (f n)
  ;; Simply add n zeros to the front of the polynomial list.
  (concatenate 'list
               (loop repeat n collect 0)
               f))

(defun polynom-add (f g)
  ;; Trim the result in case some coefficients cancel out. 
  (polynom-trim (map 'list #'add f g)))

(defun ec-polynom-div (f g ec-codewords)
  ;; Assume that the leading coefficient of g is 1.
  (loop repeat (length f)
        initially (setf f (polynom-mul-x^n f ec-codewords))
                 (setf g (polynom-mul-x^n g (- (length f) (length g))))
        with remainder
        for lead-coef = (car (last f))
        do ; (format t "~a ~a ~a ~%" f g lead-coef)
           (setf remainder (polynom-mul-scalar g lead-coef))
           (setf remainder (polynom-add f remainder))
           (setf f remainder)
           (setf g (rest g))
        finally (return remainder)))

(defun gen-generator (n)
  (if (<= n 1)
      (list 1 1)
      (polynom-mul (list (elt +powers-of-2+ (1- n)) 1) (gen-generator (1- n)))))

(defun bytes->polynomial (bytes)
  (reverse (map 'list #'binary->decimal bytes)))

(defun generate-ec-codewords (data-bytes ec-codewords)
  "Generate error correction codewords for the given data.
   Returns a list of 8-bit error correction codewords."
  (map 'list #'decimal->8-bit
       (reverse (ec-polynom-div
                 (bytes->polynomial data-bytes)
                 (gen-generator ec-codewords)
                 ec-codewords))))

