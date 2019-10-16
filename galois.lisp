;;;; Galois field GF(256) arithmetic.

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

(defmacro loop-index-value ((index value) seq &body loop-body)
  `(loop for ,index from 0 to (1- (length ,seq))
         for ,value = (elt ,seq ,index)
         ,@loop-body))

(defconstant +POWERS-OF-2+ (generate-powers-of-2))

(defun int->power (a)
  (position a +powers-of-2+))

(defmacro add (&rest integers)
  `(logxor ,@integers))

(defmacro sub (&rest integers)
  `(add ,@integers))

(defun mul-exponents (a b)
  (mod (+ a b) 255))

(defun mul (a b)
  (elt +powers-of-2+ (mul-exponents (int->power a) (int->power b))))

;; Polynomial representation:
;; f(x) = c0 + c1*x1 + ... + cn*xn
;; where ci is of the form 2^ki or 0.
;; Put f into a list like so: (c0 c1 ... cn).
;; Note: negative numbers don't exist in this field.

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

(defun polynom-div (f g)
  
  )

(defun gen-generator (n)
  (if (<= n 1)
      (list 1 1)
      (polynom-mul (list (elt +powers-of-2+ (1- n)) 1) (gen-generator (1- n)))))

