;;;; package.lisp

(defpackage #:cl-trivial-templates
  (:use #:cl #:cl-ppcre #:iterate)
  (:export #:append-to-template #:prepend-to-template #:replace-in-template #:replace-all-in-template
	   #:*template-defaults* #:finalize-template #:finalize-template!
	   #:t-> #:t<- #:t<> #:t<<>> #:tt-> #:tt<- #:tt<> #:tt<<>>
	   #:ttt-> #:ttt<- #:ttt<> #:ttt<<>>
	   #:define-template-modifier #:template #:define-template-modifier! #:*indent-style*))
