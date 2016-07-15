(defclass kdtree-space ()
  ((c0 :type (vector-real 2)
       :initarg :c0)
   (c1 :type (vector-real 2)
       :initarg :c1)
   (subspace0 :type kdtree-space
              :initform nil)
   (subspace1 :type kdtree-space
              :initform nil)
   (axis :type fixnum
         :initform nil)
   (cut :type (vector-real 2)
        :initform nil)
   (free-area :type long-float
              :reader kdtree-space-free-area)
   (shapes :initform nil)
   (n :initform 0)))

(defmethod print-object ((space kdtree-space) stream)
  (with-slots (c0 c1 axis cut free-area n) space
    (format stream "#<SPACE C0:~s C1:~s AXIS:~s CUT:~s FA:~s N:~s>"
            c0 c1 axis cut free-area n))
  space)

(defparameter *kdtree-space-split-threshold* 3)

(defmethod initialize-instance :after ((space kdtree-space) &key &allow-other-keys)
  (with-slots (c0 c1 free-area) space
    (setf free-area (->box-volume c1 c0))))

(defclass kdtree ()
  ((sheet :initarg :sheet
          :type sheet
          :reader kdtree-sheet)
   (space :type :kdtree-space)))

(defmethod initialize-instance :after ((kdtree kdtree) &key &allow-other-keys)
  (with-slots (sheet space) kdtree
    (setf space (make-instance 'kdtree-space :c0 (sheet-c0 sheet) :c1 (sheet-c1 sheet)))))

(defun kdtree-add-shape (kdtree shape)
  (with-slots (space) kdtree
    (kdtree-space-add-shape space shape)))

(defun kdtree-space-add-shape (space shape)
  (with-slots (c0 c1 free-area subspace0 subspace1 shapes n) space
    (let ((intersection-area (shape-box-intersection-area shape c0 c1)))
      (when (> intersection-area 0.0d0)
        (decf free-area intersection-area)
        (when (< free-area 0.0d0)
          (format t "free-area has become less than zero: ~s!~%" free-area)
          (setf free-area 0))
        (if subspace0
            (progn (kdtree-space-add-shape subspace0 shape)
                   (kdtree-space-add-shape subspace1 shape))
            (progn (setf shapes (cons shape shapes))
                   (incf n)
                   (when (> n *kdtree-space-split-threshold*)
                     (kdtree-space-split space))))))))

(defun kdtree-space-split (space)
  (with-slots (c0 c1 subspace0 subspace1 axis cut shapes n) space
    (multiple-value-bind (s0-c1 s1-c0 i x) (->box-split c0 c1)
      (let ((s0 (make-instance 'kdtree-space :c0 c0 :c1 s0-c1))
            (s1 (make-instance 'kdtree-space :c0 s1-c0 :c1 c1)))
        (dolist (shape shapes)
          (kdtree-space-add-shape s0 shape)
          (kdtree-space-add-shape s1 shape))
        (setf axis i
              cut x
              subspace0 s0
              subspace1 s1
              shapes nil
              n 0)))))

(defun kdtree-random-point (kdtree)
  (with-slots (space) kdtree
    (kdtree-space-random-point space)))

(defun kdtree-space-random-point (space)
  (with-slots (c0 c1 subspace0 subspace1 shapes) space
    (if subspace0
        (let ((a0 (kdtree-space-free-area subspace0))
              (a1 (kdtree-space-free-area subspace1)))
          (if (> a0 (random (+ a0 a1)))
              (kdtree-space-random-point subspace0)
              (kdtree-space-random-point subspace1)))
        (do ((r (->box-random c0 c1) (->box-random c0 c1)))
            ((not (shapes-touch-point-p shapes r)) r)))))

(defun kdtree-shape-fits-p (kdtree shape)
  (with-slots (space) kdtree
    (kdtree-space-shape-fits-p space shape)))

(defun kdtree-space-shape-fits-p (space shape)
  (with-slots (c0 c1 subspace0 subspace1 shapes) space
    (if (shape-touches-box-p shape c0 c1)
        (if subspace0
            (and (kdtree-space-shape-fits-p subspace0 shape)
                 (kdtree-space-shape-fits-p subspace1 shape))
            (not (shapes-touch-shape-p shapes shape)))
        t)))

(defun kdtree-nearest-shape (kdtree p &optional (max-d2 most-positive-double-float))
  (with-slots (space) kdtree
    (kdtree-space-nearest-shape space p nil max-d2)))

(defun kdtree-space-nearest-shape (space p best max-d2)
  (with-slots (c0 c1 subspace0 subspace1 shapes) space
    (if (< (->box-dist2 c0 c1 p) max-d2)
        (if subspace0
            (multiple-value-bind (best max-d2) (kdtree-space-nearest-shape subspace0 p best max-d2)
              (kdtree-space-nearest-shape subspace1 p best max-d2))
            (dolist (shape shapes (values best max-d2))
              (let ((d2 (shape-dist2-to-point shape p)))
                (when (< d2 max-d2)
                  (setf best shape
                        max-d2 d2)))))
        (values best max-d2))))

(defun kdtree-draw-with-transformation (kdtree transformation)
  (with-slots (space) kdtree
    (kdtree-space-draw-with-transformation space transformation)))

(defun kdtree-space-draw-with-transformation (space transformation)
  (with-slots (c0 c1 free-area subspace0 subspace1) space
    (if subspace0
        (progn (kdtree-space-draw-with-transformation subspace0 transformation)
               (kdtree-space-draw-with-transformation subspace1 transformation))
        (let* ((area (->box-volume c0 c1))
               (shade (- 1.0 (/ free-area area)))
               (a (point-transformate c0 transformation))
               (b (point-transformate (->mix c1 c0) transformation))
               (c (point-transformate c1 transformation))
               (d (point-transformate (->mix c0 c1) transformation)))
          (if (> shade 0)
              (set-rgb-fill shade shade 1.0)
              (set-rgb-fill 0.8 0.6 0.1))
          (move-to (->[] a 0) (->[] a 1))
          (line-to (->[] b 0) (->[] b 1))
          (line-to (->[] c 0) (->[] c 1))
          (line-to (->[] d 0) (->[] d 1))
          (close-subpath)
          (fill-path)))))

