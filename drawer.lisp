(load "/home/salva/g/lisp/cl-vector-real/vector-real.lisp")
(load "/home/salva/g/lisp/cl-vector-real/shapes.lisp")
(load "/home/salva/g/lisp/cl-vector-real/kdtree.lisp")

(defun draw-shapes (n &key (depth 2) (draw-frame nil) (draw-kdtree nil))
  (let* ((transformation (make-instance 'transformation
                                        :translation (-> 0.5 0)
                                        :rotation (/ pi 6)
                                        :scalation 0.7))
         (sheet (make-instance 'sheet
                               :c0 (-> (- (sqrt 2.0d0)) -1.0d0)
                               :c1 (-> (sqrt 2) 1.0d0)
                               :pixel-size (/ 1.0d0 256)
                               :transformations (list transformation)))
         (sheet-size2 (sheet-size2 sheet))
         (kdtree (make-instance 'kdtree
                                :sheet sheet))
         (shapes nil))
    (dotimes (i n)
      (let ((o (kdtree-random-point kdtree)))
        (multiple-value-bind (nearest-shape max-r2) (kdtree-nearest-shape kdtree o (* 0.02d0 sheet-size2))
          (declare (ignore nearest-shape))
          (do ((i 0 (1+ i))
                                        ; (r (random (sqrt max-r2)) (* 0.9d0 r)))
               (r (sqrt max-r2) (* 0.9d0 r)))
              ((> i 20))
            (let ((shape (make-instance 'circle :o o :r r)))
              ; (format t "~s~%" (list :sheet-size (sqrt sheet-size2) :max-r (sqrt max-r2) :r r))
              (when (kdtree-shape-fits-p kdtree shape)
                (kdtree-add-shape kdtree shape)
                (setf shapes (cons shape shapes))
                (return)))))))
    (with-sheet-canvas sheet transformation
      (sheet-draw-shapes-with-transformation sheet shapes transformation depth draw-frame
                                             (if draw-kdtree kdtree nil))
      (save-png "shapes.png")
    shapes)))