;;;; Creates QR code images.

(in-package :mare5x.lispqr.image)

(defun write-qr-matrix (file-path matrix &key (module-size-px 4))
  ;; 'matrix' is a 2-d array of bit values.
  ;; Bit 1 -> black;
  ;; Bit 0 -> white;
  
  (let* ((matrix-size (array-dimension matrix 0))
         (size-px (* matrix-size module-size-px))
         (png (make-instance 'streamed-png
                             :width size-px
                             :height size-px
                             :color-type :grayscale)))
    (with-open-file (file-stream file-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
      (start-png png file-stream)
      (loop for row from 0 below matrix-size
            for row-data = (row-data png)
            do (loop for col from 0 below matrix-size
                     for px-value = (* 255 (- 1 (aref matrix row col)))
                     for start-idx = (* col module-size-px)
                     do (fill row-data px-value :start start-idx :end (+ start-idx module-size-px)))
               ;; Repeat each row multiple times, to fill the whole module-size-px:
               (loop repeat module-size-px do (write-row row-data png)))
      (finish-png png))))

