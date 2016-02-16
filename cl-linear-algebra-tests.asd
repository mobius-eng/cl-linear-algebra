(asdf:defsystem :cl-linear-algebra
  :description "Linear algebra functions purely in Common Lisp"
  :version "0.1.0"
  :author "Alexey Cherkaev (mobius-eng)"
  :license "LGPLv3"
  :depends-on (:cl-linear-algebra :fiveam :cl-numerics-utils)
  :serial t
  :components ((:file "cl-linear-algebra-tests")))
