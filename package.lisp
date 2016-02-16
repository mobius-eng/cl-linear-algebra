(in-package cl-user)

(defpackage :cl-linear-algebra
  (:use #:cl #:optima)
  (:export
   ;; vector operations
   #:with-vector-items #:iter-vector
   #:vector-length-mistmatch
   #:vector-length-mistmatch-length1
   #:vector-length-mistmatch-length2
   #:check-vector-lengths
   #:make-vector #:vec
   #:copy-vector-contents
   #:zeros #:zeros!
   #:dot #:square-vector #:l2norm
   #:vector-elementwise
   #:sum-vectors #:add-vectors #:lincomb
   #:nscale-vector #:nnegate-vector
   #:mmul #:mmul-function
   #:solve-linear))
