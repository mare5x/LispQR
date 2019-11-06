(in-package :mare5x.lispqr.encode)

(defparameter *data-blocks* `((,(map 'list #'decimal->8-bit '(67 85 70 134 87 38 85 194 119 50 6 18 6 103 38))
                               ,(map 'list #'decimal->8-bit '(246 246 66 7 118 134 242 7 38 86 22 198 199 146 6)))
                              (,(map 'list #'decimal->8-bit '(182 230 247 119 50 7 118 134 87 38 82 6 134 151 50 7))
                               ,(map 'list #'decimal->8-bit '(70 247 118 86 194 6 151 50 16 236 17 236 17 236 17 236)))))

(defparameter *data-codewords*
  '(#*01000011 #*01010101 #*01000110 #*10000110 #*01010111 #*00100110 #*01010101 #*11000010 #*01110111 #*00110010 #*00000110
    #*00010010 #*00000110 #*01100111 #*00100110 #*11110110 #*11110110 #*01000010 #*00000111 #*01110110 #*10000110 #*11110010
    #*00000111 #*00100110 #*01010110 #*00010110 #*11000110 #*11000111 #*10010010 #*00000110 #*10110110 #*11100110 #*11110111
    #*01110111 #*00110010 #*00000111 #*01110110 #*10000110 #*01010111 #*00100110 #*01010010 #*00000110 #*10000110 #*10010111
    #*00110010 #*00000111 #*01000110 #*11110111 #*01110110 #*01010110 #*11000010 #*00000110 #*10010111 #*00110010 #*00010000
    #*11101100 #*00010001 #*11101100 #*00010001 #*11101100 #*00010001 #*11101100))

(defun blocks->dec (el)
  "This function is an absolute beauty."
  (if (bit-vector-p el)
      (binary->decimal el)
      (map 'list #'blocks->dec el)))

(defun test-codewords->blocks ()
  "https://www.thonky.com/qr-code-tutorial/error-correction-coding"

  (format t "TEST: codewords to blocks~%codewords:~%") 
  (print-bits *data-codewords* :dec)
  (format t "my blocks:~%~a~%" (blocks->dec (data-codewords->blocks *data-codewords* 5 :Q)))
  (format t "their blocks:~%~a~%" (blocks->dec *data-blocks*))
  (equal (data-codewords->blocks *data-codewords* 5 :Q)
         *data-blocks*))

(defun test-error-correction-placement ()
  "Test the examples on these pages:
   https://www.thonky.com/qr-code-tutorial/structure-final-message
   https://www.thonky.com/qr-code-tutorial/error-correction-coding
   
   This tests error correction codeword generation (galois) and proper block interleaving."
  
  (let ((data-blocks *data-blocks*)
        (result-stream #*01000011111101101011011001000110010101011111011011100110111101110100011001000010111101110111011010000110000001110111011101010110010101110111011000110010110000100010011010000110000001110000011001010101111100100111011010010111110000100000011110000110001100100111011100100110010101110001000000110010010101100010011011101100000001100001011001010010000100010001001011000110000001101110110000000110110001111000011000010001011001111001001010010111111011000010011000000110001100100001000100000111111011001101010101010111100101001110101111000111110011000111010010011111000010110110000010110001000001010010110100111100110101001010110101110011110010100100110000011000111101111011011010000101100100111111000101111100010010110011101111011111100111011111001000100001111001011100100011101110011010101111100010000110010011000010100010011010000110111100001111111111011101011000000111100110101011001001101011010001101111010101001001101111000100010000101000000010010101101010001101101100100000111010000110100011111100000010000001101111011110001100000010110010001001111000010110001101111011000000000))

    (equal (apply #'concatenate 'bit-vector
                  (interleave-blocks data-blocks
                                     (data-codeword-blocks->ec-blocks data-blocks 5 :Q) 5))
           result-stream)))

(defun test-alphanumeric-encoding ()
  ;; Test from https://www.thonky.com/qr-code-tutorial/data-encoding
  (format t "TEST: HELLO WORLD 1/Q~%")
  (print-bits '(#*00100000 #*01011011 #*00001011 #*01111000 #*11010001 #*01110010 #*11011100 #*01001101 #*01000011 #*01000000 #*11101100 #*00010001 #*11101100))
  (print-bits (bits->codewords (encode-alphanumeric "HELLO WORLD") 1 :q))

  ;; Test against examples from https://www.nayuki.io/page/creating-a-qr-code-step-by-step
  (format t "TEST: PROJECT NAYUKI 2/H~%")
  (print-bits #*00100000011101001000000010001001011010100000101010011110110000010101110000110000111001011000000011101100000100011110110000010001)
  (print-bits (bits->codewords (encode-alphanumeric "PROJECT NAYUKI" :version 2 :ec-level :H) 2 :H)))

(defun test-ecc ()
  ;; Test "PROJECT NAYUKI" in alphanumeric mode from:
  ;; https://www.nayuki.io/page/creating-a-qr-code-step-by-step
  
  (format t "TEST ECC: PROJECT NAYUKI 2/L~%")
  (print-bits (generate-ec-codewords
               (map 'list #'decimal->8-bit '(#x20 #x74 #x80 #x89 #x6a #x0a #x9e #xc1 #x5c #x30 #xe5 #x80 #xec #x11 #xec #x11 #xec #x11 #xec))
               7))
  (print-bits #*10001100000001011001001011101101010000101010000101101111)

  (format t "TEST ECC: PROJECT NAYUKI 2/M~%")
  (print-bits (generate-ec-codewords
               (map 'list #'decimal->8-bit '(#x20 #x74 #x80 #x89 #x6a #x0a #x9e #xc1 #x5c #x30 #xe5 #x80 #xec #x11 #xec #x11))
               10))
  (print-bits #*01000000110000010111001110111000000000001100101101001100110101100001011101100111)

  (format t "TEST ECC: PROJECT NAYUKI 2/Q~%")
  (print-bits (generate-ec-codewords
               (map 'list #'decimal->8-bit '(#x20 #x74 #x80 #x89 #x6a #x0a #x9e #xc1 #x5c #x30 #xe5 #x80 #xec))
               13))
  (print-bits #*00001001011101001001111110111000100000110010000001001011001101111110010001111001011110100011111111011010)
  
  (format t "TEST ECC: PROJECT NAYUKI 2/H~%")
  (print-bits (generate-ec-codewords
               (map 'list #'decimal->8-bit '(#x20 #x74 #x80 #x89 #x6a #x0a #x9e #xc1 #x5c #x30 #xe5 #x80 #xec #x11 #xec #x11))
               28))
  (print-bits #*11101110111110000011101100010110011000010101011110000111111101001100111001110001101101011101100011110100100010010101001111010010011001000000101100110111100111100101001001000001100011000101011110100010001111100001001010111011))



