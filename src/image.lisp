;;;; Creates QR code images.

(in-package :mare5x.lispqr.image)

(defun write-qr-matrix (file-path matrix &key (module-size-px 8))
  ;; 'matrix' is a 2-d array of bit values.
  ;; Bit 1 -> black;
  ;; Bit 0 -> white;

  ;; NOTE: add the "Quiet zone" (a blank border around the matrix).
  ;; The Quiet zone is 4 modules wide in each direction.
  (let* ((matrix-size (array-dimension matrix 0))
         (image-size-px (+ (* 2 4 module-size-px)  ; Quiet zone
                           (* matrix-size module-size-px)))
         (png (make-instance 'streamed-png
                             :width image-size-px
                             :height image-size-px
                             :color-type :grayscale)))
    (with-open-file (file-stream (ensure-directories-exist file-path)
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
      (start-png png file-stream)
      (loop with row-offset = 4 ; For centering the image
            with col-offset = 4 ; 4 is the quiet zone width
            with blank-row = (copy-seq (fill (row-data png) 255))
            initially (loop repeat (* row-offset module-size-px) do (write-row blank-row png))  ; Fill the quiet zone rows

            for row from 0 below matrix-size
            for row-data = (row-data png)
            do (loop for col from 0 below matrix-size
                     for px-value = (* 255 (- 1 (aref matrix row col)))
                     for start-idx = (* (+ col col-offset) module-size-px)
                     do (fill row-data px-value :start start-idx :end (+ start-idx module-size-px)))
               ;; Repeat each row multiple times, to fill the whole module-size-px:
               (loop repeat module-size-px do (write-row row-data png))

            ;; Fill the bottom quiet zone
            finally (loop repeat (* row-offset module-size-px) do (write-row blank-row png)))
      (finish-png png))
    (format t "~a~%" (truename file-path))))

