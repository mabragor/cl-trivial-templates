
(in-package #:cl-user)

(defpackage #:cl-trivial-templates-tests
  (:use #:cl #:cl-trivial-templates #:fiveam #:iterate)
  (:export #:run-tests))


(in-package :cl-trivial-templates-tests)

(cl-interpol:enable-interpol-syntax)

(def-suite trivial-templates)
(in-suite trivial-templates)

(defun run-tests ()
  (let ((results (run 'trivial-templates)))
    (explain! results)
    (unless (results-status results)
      (error "Tests failed."))))

(define-template-modifier frob (x)
  (ttt-> :whatever (string-downcase x)))

(defun smart-indents ()
  (let ((template
"if (###test###) {
    ###then###
} else {
    ###else###
}")
	(*indent-style* :smart-newline))
    (declare (special template))
    (ttt-> :then #?"a;\nb;\nc;")
    (ttt<- :else #?"d;\ne;\nf;")
    (ttt<> :test #?"1 == 2")))

(defun smart-butlast-indents ()
  (let ((template "  ###whatever###")
	(*indent-style* :smart-butlast-newline))
    (declare (special template))
    (ttt-> :whatever #?"a;\nb;\nc;")))


(test simple
  (is (equal `(,#?"a\n" ,#?"b\n" #?"c\n") (let ((template "###whatever###"))
					    (declare (special template))
					    (iter (for x in '(a b c))
						  (collect (finalize-template (frob x)))))))
  (is (equal '("a" "b" "c") (let ((template "###whatever###")
				  (*indent-style* :none))
			      (declare (special template))
			      (iter (for x in '(a b c))
				    (collect (finalize-template (frob x)))))))
  (is (equal "if (1 == 2) {
    a;
    b;
    c;
    ###then###
} else {
    ###else###
    d;
    e;
    f;
}" (smart-indents)))
  (is (equal "  a;
  b;
  c;###whatever###" (smart-butlast-indents))))

