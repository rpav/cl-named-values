(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-gendoc))

(defsystem :cl-named-values-docs
  :depends-on (:cl-gendoc :cl-named-values)

  :serial t
  :pathname "doc"

  :components
  ((:file "generate")))

(gendoc:define-gendoc-load-op :cl-named-values-docs :named-values.docs 'generate)
