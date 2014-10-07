;;;; cl-trivial-templates.asd

(asdf:defsystem #:cl-trivial-templates
  :serial t
  :description "A very simple tools to template text (e.g. source code)"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-ppcre #:cl-interpol #:iterate)
  :components ((:file "package")
               (:file "cl-trivial-templates")))

(defsystem :cl-trivial-templates-tests
  :description "Tests for CL-TRIVIAL-TEMPLATES."
  :licence "GPL"
  :serial t
  :depends-on (#:cl-trivial-templates #:fiveam #:iterate #:cl-interpol)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-trivial-templates))))
  (load-system :cl-trivial-templates-tests)
  (funcall (intern "RUN-TESTS" :cl-trivial-templates-tests)))


