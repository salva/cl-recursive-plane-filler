(defclass transformation ()
  ((translation :initarg :translation
                :type (vector-real 2)
                :reader transformation-translation)
   (rotation :initarg :rotation
             :type long-float
             :reader transformation-rotation)
   (scalation :initarg :scalation
              :type long-float
              :reader transformation-scalation)
   (v0 :type (vector-real 2))
   (v1 :type (vector-real 2)))
  (:default-initargs :translation (-> 0 0) :rotation 0.0d0 :scalation 1.0d0))

(defmethod initialize-instance :after ((instance transformation) &key &allow-other-keys)
  (with-slots (rotation scalation v0 v1) instance
    (let ((s (* scalation (sin rotation)))
          (c (* scalation (cos rotation))))
      (setf v0 (-> c s)
            v1 (-> (- s) c)))))

(defun point-transformate (p transformation)
  (declare (type transformation transformation))
  (with-slots (translation v0 v1) transformation
    (->*+ translation 1.0d0 v0 (->[] p 0) v1 (->[] p 1))))

(defun vector-transformate (u transformation)
  (declare (type transformation transformation))
  (with-slots (v0 v1) transformation
    (->*+ v0 (->[] u 0) v1 (->[] u 1))))

(defun scalar-transformate (s transformation)
  (declare (type transformation transformation))
  (with-slots (scalation) transformation
    (* scalation s)))

(defun angle-transformate (angle transformation)
  (declare (type transformation transformation))
  (with-slots (rotation) transformation
    (+ rotation angle)))

(defun transformation-transformate (a b)
  (with-slots (translation rotation scalation) a
    (make-instance 'transformation
                   :translation (point-transformate translation b)
                   :rotation (angle-transformate rotation b)
                   :scalation (scalar-transformate scalation b))))

(defun transformation-dump (transformation)
  (with-slots (translation rotation scalation v0 v1) transformation
    (format t "~s~%" (list :ox (->[] translation 0) :oy (->[] translation 1)
                           :rotation rotation :scalation scalation
                           :v0x (->[] v0 0) :v0y (->[] v0 1)
                           :v1x (->[] v1 0) :v1y (->[] v1 1)))))

(defclass shape () ())
(defgeneric shape-transformate (shape transformation))
(defgeneric shape-box-intersection-area (shape c0 c1))

(defgeneric shape-draw-with-transformation (shape transformation))
(defgeneric shape-touches-point-p (shape point))
(defgeneric shape-touches-box-p (shape c0 c1))
(defgeneric shape-touches-shape-p (a b))
(defgeneric shape-dist2-to-point (shape p))

(defclass parallelogram (shape)
  ((a  :initarg :a
       :type (vector-real 2)
       :reader parallelogram-a)
   (ab :initarg :ab
       :type (vector-real 2)
       :reader parallelogram-ab)
   (ad :initarg :ad
       :type (vector-real 2)
       :reader parallelogram-ad))
  (:default-initargs :a (-> 0 0) :ab (-> 1 0) :ad (-> 0 1)))

(defclass circle (shape)
  ((o :initarg :o
      :type (vector-real 2)
      :reader circle-o)
   (r :initarg :r
      :type long-float
      :reader circle-r))
  (:default-initargs :o (-> 0 0) :r 1.0d0))

(defmethod shape-transformate ((parallelogram parallelogram) (transformation transformation))
  (with-slots (a ab ad) parallelogram
    (make-instance 'parallelogram
                   :a (point-transformate a transformation)
                   :ab (vector-transformate ab transformation)
                   :ad (vector-transformate ad transformation))))

(defmethod shape-transformate ((circle circle) (transformation transformation))
  (with-slots (o r) circle
    (with-slots (scalation) transformation
    (make-instance 'circle
                   :o (point-transformate o transformation)
                   :r (scalar-transformate r transformation)))))


(defmethod print-object ((circle circle) stream)
  (with-slots (r o) circle
    (let ((area (* pi r r)))
    (format stream "#<C ~s ~s [~s]>" r o area))
  circle))

(defclass sheet ()
  ((c0 :initarg :c0
       :type (vector-real 2)
       :reader sheet-c0)
   (c1 :initarg :c1
       :type (vector-real 2)
       :reader sheet-c1)
   (pixel-size :initarg :pixel-size
               :type long-float
               :reader pixel-size)
   (transformations :initarg :transformations
                    :reader sheet-transformations))
  (:default-initargs :c0 (-> 0 0) :c1 (-> (sqrt 2) 1) :pixel-size (/ 1.0d0 1024) :transformations ()))

(defun sheet-diagonal (sheet)
  (with-slots (c0 c1) sheet
    (->- c1 c0)))

(defun sheet-size2 (sheet)
  (with-slots (c0 c1) sheet
    (->dist2 c0 c1)))

(defmacro with-sheet-canvas (sheet transformation &body body)
  (with-gensyms (pixel-size c0 c1)
    `(let ((,pixel-size (slot-value ,sheet 'pixel-size))
           (,c0 (slot-value ,sheet 'c0))
           (,c1 (slot-value ,sheet 'c1)))
       (with-canvas (:width  (floor (/ (- (->[] ,c1 0) (->[] ,c0 0)) ,pixel-size))
                     :height (floor (/ (- (->[] ,c1 1) (->[] ,c0 1)) ,pixel-size)))
         (let ((,transformation (make-instance 'transformation
                                               :scalation (/ 1.0d0 ,pixel-size)
                                               :translation (->* ,c0 (/ -1.0d0 ,pixel-size)))))
           ,@body)))))


(defun sheet-draw-shapes (sheet shapes &key (depth 10) (filename "shapes.png") (draw-frame nil))
  (with-sheet-canvas sheet transformation
    (sheet-draw-shapes-with-transformation sheet shapes transformation depth draw-frame)
    (save-png filename)))

(defun sheet-draw-shapes-with-transformation (sheet shapes transformation depth &optional draw-frame kdtree)
  ; (transformation-dump transformation)
  (when kdtree
    (kdtree-draw-with-transformation kdtree transformation))
  (when draw-frame
    (sheet-draw-border-with-transformation sheet transformation))
  (dolist (shape shapes)
    (shape-draw-with-transformation shape transformation))
  (when (> depth 0)
    (with-slots (transformations) sheet
      (dolist (child transformations)
        (sheet-draw-shapes-with-transformation sheet shapes
                                               (transformation-transformate child transformation)
                                               (1- depth) draw-frame kdtree)))))

(defun sheet-draw-border-with-transformation (sheet transformation)
  (with-slots (c0 c1) sheet
    (let ((a (point-transformate c0 transformation))
          (b (point-transformate (->mix c1 c0) transformation))
          (c (point-transformate c1 transformation))
          (d (point-transformate (->mix c0 c1) transformation)))
      (set-rgb-stroke 1.0 0.0 0.0)
      (set-line-width 3)
      (move-to (->[] a 0) (->[] a 1))
      (line-to (->[] b 0) (->[] b 1))
      (line-to (->[] c 0) (->[] c 1))
      (line-to (->[] d 0) (->[] d 1))
      (line-to (->[] a 0) (->[] a 1))
      (stroke))))

(defmethod shape-draw-with-transformation ((circle circle) (transformation transformation))
  (with-slots (o r) circle
    (let ((o1 (point-transformate o transformation))
          (r1 (scalar-transformate r transformation)))
      (set-rgb-fill 0.0 0.0 0.0)
      (arc (->[] o1 0) (->[] o1 1) r1 0.0d0 (* 2 pi))
      (fill-path))))

(defmethod shape-touches-point-p ((circle circle) point)
  (with-slots (o r) circle
    (<= (->dist2 o point) (* r r))))


(defmethod shape-touches-shape-p ((a circle) (b circle))
  (let ((r (+ (circle-r a) (circle-r b))))
    (<= (->dist2 (circle-o a) (circle-o b)) (* r r))))

(defun shapes-touch-point-p (shapes p)
  (when shapes
    (or (shape-touches-point-p (car shapes) p)
        (shapes-touch-point-p (cdr shapes) p))))

(defun shapes-touch-shape-p (as b)
  (when as
    (or (shape-touches-shape-p (car as) b)
        (shapes-touch-shape-p (cdr as) b))))

(defmethod shape-dist2-to-point ((circle circle) p)
  (with-slots (o r) circle
    (let ((d (- (->dist o p) r)))
      (if (< d 0) 0 (* d d)))))

(defmethod shape-touches-box-p ((circle circle) c0 c1)
  (with-slots (o r) circle
    (< (->box-dist2 c0 c1 o) (* r r))))

(defun ia2-circle-segment (r2 a b)
  (let* ((ab (->- b a))
         (c2 (->norm2 ab)))
    (if (<= c2 0) 0
        (let* ((c1 (->. a ab))
               (c0 (- (->norm2 a) r2))
               (discriminant (- (* c1 c1) (* c0 c2))))
          (format t "~s~%" (list :c1 c1 :c0 c0 :c2 c2 :discriminant discriminant :a a :ab ab))
          (if (> discriminant 0)
              (let* ((inv-c2 (/ 1.0 c2))
                     (sqrt-discriminant (sqrt discriminant))
                     (alfa0 (* inv-c2 (- (- c1) sqrt-discriminant)))
                     (alfa1 (* inv-c2 (+ (- c1) sqrt-discriminant))))
                (format t "~s~%" (list :inv-c2 inv-c2 :sqrt-discriminant sqrt-discriminant :alfa0 alfa0 :alfa1 alfa1))
                (if (and (< alfa0 1.0) (> alfa1 0))
                    (let ((beta 0)
                          (p0)
                          (p1))
                      (if (> alfa0 0)
                          (setf p0 (->*+ a 1.0d0 ab alfa0)
                                beta (+ beta (->angle a p0)))
                          (setf p0 a))
                      (if (<= alfa1 1)
                          (setf p1 (->*+ a 1.0d0 ab alfa1)
                                beta (+ beta (->angle p1 b)))
                          (setf p1 b))
                      (return-from ia2-circle-segment
                        (+ (- (* (->[] p0 0) (->[] p1 1))
                              (* (->[] p0 1) (->[] p1 0)))
                           (* beta r2)))))))
          (* r2 (->angle a b))))))

(defmethod shape-box-intersection-area ((circle circle) c0 c1)
  (with-slots (o r) circle
    (circle-box-intersection-area o r c0 c1)))

(defun circle-box-intersection-area (o r c0 c1)
  (declare (type (vector-real 2) o c0 c1)
           (type long-float r)
           (notinline ->box-dist2 ->box-boundary-dist2))
  (let ((r2 (* r r)))
    (declare (type long-float r2))
    (cond ((< r2 (->box-dist2 c0 c1 o)) 0.0d0)
          ((< r2 (->box-boundary-dist2 c0 c1 o)) (* pi r r))
          (t (let* ((a (->- c0 o))
                    (c (->- c1 o))
                    (b (->mix a c))
                    (d (->mix c a)))
               (declare (type (vector-real 2) a c b d))
               (abs (* 0.5 (+ (ia2-circle-segment r2 a b)
                              (ia2-circle-segment r2 b c)
                              (ia2-circle-segment r2 c d)
                              (ia2-circle-segment r2 d a)))))))))

