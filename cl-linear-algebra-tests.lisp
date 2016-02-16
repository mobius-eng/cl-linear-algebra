(defpackage :cl-linear-algebra-tests
  (:use #:cl #:fiveam #:cl-linear-algebra #:cl-numerics-utils))

(in-package cl-linear-algebra-tests)

(def-suite cl-linear-algebra)

(in-suite cl-linear-algebra)

(test basic-operations
  (let* ((u (vec t 1 2 3 4 5))
         (v (make-vector 5 'double-float))
         (true-v (vec 'double-float 1d0 2d0 3d0 4d0 5d0))
         (zeros (zeros v))
         (dot-v (vec 'double-float 2d0 4d0 -3d0 0d0 1.2d4))
         (dot-u (vec 'double-float -1d0 3d0 0d0 -1d+7 1d-5))
         (dot-t (dot dot-u dot-v))
         (dot-d (dot dot-u dot-v 'double-float))
         (square-v (square-vector dot-v)))
    (copy-vector-contents u v)
    (is-true
     (let ((result t))
       (dotimes (i 5 result)
         (unless (num= (aref v i) (aref true-v i))
           (setf result nil)
           (return result)))))
    (is (= (length zeros) (length v)))
    (is (eq (array-element-type zeros) (array-element-type v)))
    (is-true (every #'zerop zeros))
    (zeros! v)
    (is-true (every #'zerop v))
    (is-true (num= dot-t dot-d))
    (is-true (num= dot-d (loop for x across dot-v
                            for y across dot-u
                            sum (* x y))))
    (is-true (num= square-v (loop for x across dot-v
                               sum (* x x))))
    (is-true (num= (l2norm dot-v) (sqrt square-v)))
    (is-true (every #'num=
                    (let ((x (make-vector 5 'double-float)))
                      (vector-elementwise x #'* dot-v dot-u)
                      x)
                    (loop for x across dot-v
                       for y across dot-u
                         collect (* x y))))))

(run! 'basic-operations)


(test sum-operations
  (let* ((u (vec 'double-float 2d0 4d0 -3d0 0d0 1.2d4))
         (v (vec 'double-float -1d0 3d0 0d0 -1d+7 1d-5))
         (w (vec 'double-float -1d2 3d5 5d0 0 1.3d-1))
         (coeff1 1.2d-1)
         (coeff2 2.3d0)
         (coeff 4.3d0)
         (u1 (copy-seq u))
         (u2 (copy-seq u))
         (u3 (copy-seq u)))
    (add-vectors u1 :vectors (list v w) :multipliers (list coeff1 coeff2))
    (sum-vectors u2 :vectors (list v w) :multipliers (list coeff1 coeff2))
    (lincomb coeff u3 :vectors (list v w) :multipliers (list coeff1 coeff2))
    (is-true (every #'num= u1 (loop for x across v
                                 for y across w
                                 for z across u
                                 collect (+ z (* x coeff1) (* y coeff2)))))
    (is-true (every #'num= u2 (loop for x across v
                                 for y across w
                                 for z across u
                                 collect (+ (* x coeff1) (* y coeff2)))))
    (is-true (every #'num= u3 (loop for x across v
                                 for y across w
                                 for z across u
                                 collect (+ (* z coeff) (* x coeff1) (* y coeff2)))))))

(run! 'sum-operations)

(test unary-operations
  (let* ((u (vec 'double-float 2d0 4d0 -3d0 0d0 1.2d4))
         (v (vec 'double-float -1d0 3d0 0d0 -1d+7 1d-5))
         (u1 (copy-seq u))
         (v1 (copy-seq v)))
    (nnegate-vector u1)
    (nscale-vector 3.5d0 v1)
    (is-true (every #'num= u1 (loop for x across u collect (- x))))
    (is-true (every #'num= v1 (loop for x across v collect (* 3.5d0 x))))))

(run!'unary-operations)


(test mmul-operations
  (let* ((matrix #2A((1 2 3) (4 5 6) (7 8 9)))
         (vector #(1 -1 1))
         (expected-result #(2 5 8))
         (result (make-vector 3))
         (mfun (mmul-function matrix))
         (result-fun (make-vector 3)))
    (mmul matrix vector result)
    (funcall mfun vector result-fun)
    (is-true (every #'num= result result-fun))
    (is-true (every #'num= expected-result result))))


(run! 'mmul-operations)
