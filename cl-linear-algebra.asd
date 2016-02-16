(asdf:defsystem :cl-linear-algebra
  :description "Linear algebra functions purely in Common Lisp"
  :version "0.1.0"
  :author "Alexey Cherkaev (mobius-eng)"
  :license "LGPLv3"
  :depends-on (:optima)
  :serial t
  :components ((:file "package")
               (:file "cl-linear-algebra")))
