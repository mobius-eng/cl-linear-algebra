(in-package cl-linear-algebra)

(defmacro with-vector-items (items vector &body body)
  "Destructure vectors by reffering to particular indices. Each reference is a place
ITEMS is the list of pairs of the form ((X 0) (Y 1) (Z 2)). Here Y will refer to 1-st
      component of the VECTOR. (SETF Y) will change the 1-st item of the VECTOR
VECTOR is the vector on which the BODY operates. Evaluated once only"
  (let ((gv (gensym "VECTOR")))
    (let ((bindings (mapcar (lambda (var-index)
                              (destructuring-bind (var index) var-index
                                `(,var (aref ,gv ,index))))
                            items)))
      `(let ((,gv ,vector))
         (symbol-macrolet (,@bindings)
           ,@body)))))

(defmacro iter-vector ((bindings (counter &optional (min 0) max (step 1)) &optional result)
                       &body body)
  "Iterate over vector values. For example,

  (iter-vector (((x vec1)
                 (y vec2)
                 ...)
                (i from below step)
                result)
      (setf (x i) (+ (x i) (y i))))

In BINDINGS each symbol is MACROLET-bound to '(aref vector i)' of each vector.
COUNTER is used to refer to current index. BODY is evaluated at every iteration"
  (let ((gvectors (mapcar (lambda (b)
                            `(,(gensym "VECTOR") ,(second b)))
                          bindings))
        (gmax (gensym "MAX"))
        (gstep (gensym "STEP")))
    `(let (,@gvectors
           (,gmax ,max)
           (,gstep ,step))
       (unless ,gmax
         (setf ,gmax (length ,(caar gvectors))))
       (macrolet (,@(mapcar (lambda (b gv)
                              `(,(first b) (i)
                                 (list 'aref ',(first gv) i)))
                            bindings
                            gvectors))
         (do ((,counter ,min (+ (the fixnum ,counter) (the fixnum ,gstep))))
             ((>= ,counter ,gmax) ,@(if result (list result) nil))
           ,@body)))))

(define-condition vector-length-mismatch (error)
  ((vector-length-mismatch-length1 :initarg :length1
                                   :reader vector-length-mismatch-length1)
   (vector-length-mismatch-length2 :initarg :length2
                                   :reader vector-length-mismatch-length2))
  (:documentation
   "Condition raised when two vectors have mismatching lengths"))

(defun check-vector-lengths (vec &rest more-vectors)
  "Tests if all vectors have the same length. Returns the length if so, and
raises the condition VECTOR-LENGTH-MISMATCH otherwise"
  (declare (optimize (speed 3) (debug 1) (safety 1))
           (type (simple-array * *) vec))
  (let ((len (length vec))
        (other-length 0))
    (declare (type fixnum len other-length))
    (or (every (lambda (w)
                 (declare (type (simple-array * *) w))
                 (setf other-length (length w))
                 (eql other-length len))
               more-vectors)
        (error 'vector-length-mismatch
               :length1 len
               :length2 other-length))
    len))

(defun make-vector (length &optional (type t) (initial-element nil init-p))
  "Make (SIMPLE-ARRAY TYPE LENGTH) or (SIMPLE-VECTOR LENGTH) if TYPE is T"
  (declare (type fixnum length))
  (if init-p
      (make-array length :element-type type :initial-element initial-element)
      (make-array length :element-type type)))

(defun vec (type &rest items)
  "Construct a vector with ITEMS of a given TYPE"
  (make-array (length items)
              :element-type type
              :initial-contents (mapcar (lambda (x) (coerce x type)) items)))

(defun copy-vector-contents (from to &optional (coerce-to 'double-float))
  "Copy all items from vector FROM to vector TO. Both must be SIMPLE-ARRAY's.
Coerce all items to fit the type COERCE-TO (defaults to 'DOUBLE-FLOAT)"
  (declare (type simple-array from to))
  (let ((length (check-vector-lengths from to)))
    (case coerce-to
      ((nil t) (iter-vector (((x from) (y to))
                             (i 0 length))
                 (setf (y i) (x i))))
      (t (iter-vector (((x from) (y to))
                       (i 0 length))
           (setf (y i) (coerce (x i) coerce-to)))))))

(defun dot (vector1 vector2 &optional (generic-type t))
  "Dot product of two vectors:
    __      
   \    v w 
   /__   i i
    i

Lengths of V W must be the same.
Both vectors must be SIMPLE-ARRAY's.
If GENERIC-TYPE is not T, they are considered DOUBLE-FLOAT arrays."
  (declare (type simple-array vector1 vector2)
           (optimize (speed 3) (debug 1) (safety 1) (space 0)))
  (check-vector-lengths vector1 vector2)
  (case generic-type
    ((t) (let ((result 0))
           (iter-vector (((x vector1) (y vector2)) (i) result)
             (incf result (* (x i) (y i))))))
    (t (let ((result 0.0d0))
         (declare (type double-float result))
         (iter-vector (((x vector1) (y vector2)) (i) result)
           (incf result (the double-float (* (the double-float (x i))
                                             (the double-float (y i))))))))))


(defun square-vector (vector &optional (generic-type t))
  "Dot product of SIMPLE-ARRAY V on itself:
    __
   \    v v 
   /__   i i
    i

"
  (declare (type simple-array vector)
           (optimize (speed 3) (debug 1) (safety 1)))
  (case generic-type
    ((t) (let ((result 0))
           (iter-vector (((x vector)) (i) result)
             (incf result (expt (x i) 2)))))
    (t (let ((result 0d0))
         (declare (type double-float result))
         (iter-vector (((x vector)) (i) result)
           (incf result (the double-float (expt (the double-float (x i)) 2))))))))

(defun l2norm (vector &optional (generic-type t))
  "L2 norm of a SIMPLE-ARRAY:
         _________
        / __   2
    |  / \    v 
    | /  /__   i
    |/    i

If GENERIC-TYPE is not (EQ T), consider VECTOR to be DOUBLE-FLOAT vector"
  (sqrt (square-vector vector generic-type)))

(defun l2norm-diff (vector &rest other-vectors)
  "L2NORM of difference between VECTOR and OTHER-VECTORS
Does not allocate any extra space"
  (declare (type simple-array vector)
           (optimize (speed 3) (debug 1) (safety 1)))
  (apply #'check-vector-lengths vector other-vectors)
  (match other-vectors
    (nil (l2norm vector))
    ((list vector2)
     (declare (type simple-array vector2))
     (let ((result 0d0))
       (declare (type (double-float 0d0) result))
       (iter-vector (((x vector) (y vector2)) (i) (sqrt result))
         (incf result (the double-float
                           (expt (coerce (- (x i) (y i)) 'double-float) 2))))))
    (otherwise
     (let ((result 0d0))
       (declare (type (double-float 0d0) result))
       (iter-vector (((x vector)) (i) (sqrt result))
         (incf result
               (expt (coerce
                      (reduce #'- other-vectors
                              :initial-value (x i)
                              :key (lambda (v) (aref v i)))
                      'double-float)
                     2)))))))


(defgeneric zeros (specs)
  (:documentation "Produce the array of zeros based on specification"))

(defmethod zeros (#-clisp(specs fixnum) #+clisp(specs integer))
  (make-array specs :initial-element 0))

(defmethod zeros ((specs array))
  (let ((element-type (array-element-type specs))
        (dimensions (array-dimensions specs)))
    (make-array dimensions
                :element-type element-type
                :initial-element (coerce 0 element-type))))

(defun zeros! (array)
  "Set all items of the ARRAY to zero"
  (let* ((length (reduce #'* (array-dimensions array)))
         (element-type (array-element-type array))
         (zero-element (coerce 0 element-type)))
    (dotimes (i length)
      (setf (row-major-aref array i) zero-element))))

(defun vector-elementwise (destination function vector &rest more-vectors)
  "Applies FUNCTION to corresponding elements of supplied vectors. Result of function
  application is put into corresponding element in DESTINATION"
  (dotimes (i (length destination))
    (setf (aref destination i)
          (apply function (aref vector i) (mapcar (lambda (v) (aref v i)) more-vectors)))))

(defun sum-vectors (destination &key vectors multipliers)
  "Sum vectors with multipliers setting the destination to be
    ---
    \   a  v
    /    i  i
    ---

 "
  (assert (= (length vectors) (length multipliers))
          ()
          "Vectors and their multipliers length mismatch")
  (zeros! destination)
  (loop for vector in vectors
     for multiplier in multipliers
       
     do
       (vector-elementwise destination
                            (lambda (x y)
                              (+ x (* multiplier y)))
                            destination
                            vector)))


(defun add-vectors (vector &key vectors multipliers)
  "Add vectors to vector with multipliers, setting VECTOR u to

       ---
  u  + \   a  v
       /    i  i
       ---
"
  (assert (= (length vectors) (length multipliers))
          ()
          "Vectors and their multipliers length mismatch")
  (loop for vec in vectors
     for multiplier in multipliers       
     do
       (vector-elementwise vector
                            (lambda (x y)
                              (+ x (* multiplier y)))
                            vector
                            vec)))

(defun lincomb (multiplier1 vector1 &key vectors multipliers)
  "Calculate linear combination of vectors setting VECTOR1 u to be:

          ---
  a  u  + \   a  v
          /    i  i
          ---
"
  (vector-elementwise vector1 (lambda (v) (* multiplier1 v)) vector1)
  (loop for vector in vectors
     for multiplier in multipliers
     do
       (vector-elementwise vector1
                           (lambda (x y) (+ x (* multiplier y)))
                           vector1
                           vector)))


(defun nscale-vector (factor vector &optional (result vector))
  "Scale every item of VECTOR by FACTOR. Return the result in RESULT"
  (vector-elementwise result (lambda (x) (* x factor)) vector))

(defun nnegate-vector (vector &optional (vout vector))
  "Negate every item of VECTOR returning the result in VOUT"
  (vector-elementwise vout #'- vector))

;; ** Matrix multiplication
(defun mmul (matrix vector result)
  (let ((nm (array-dimensions matrix)))
    (dotimes (i (first nm))
      (setf (aref result i) (coerce 0 (array-element-type result)))
      (dotimes (j (second nm))
        (incf (aref result i) (* (aref matrix i j) (aref vector j)))))))

(defun mmul-function (matrix)
  (lambda (vector result) (mmul matrix vector result)))



(defgeneric solve-linear (method a b x)
  (:documentation
   "Solve linear set of equations A * X = B using METHOD
A must be a function of the form (A P Q) equivalent to A * P = Q
Solution will be put into X. X can act as initial approximation for
  an iterative METHOD
Returns (VALUES X SUCCESSFUL-P FINAL-RESIDUAL-L2-NORM MORE-INFO*)
"))
