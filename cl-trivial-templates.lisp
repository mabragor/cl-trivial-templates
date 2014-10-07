;;;; cl-trivial-templates.lisp

(in-package #:cl-trivial-templates)

(cl-interpol:enable-interpol-syntax)

;; *INDENT-TYPE* can also be :NONE and :SMART-NEWLINE
(defvar *indent-style* :simple-newline)

(defun crunch-in-indent (indent str)
  (format nil #?"~{~a~^~%$(indent)~}" (cl-ppcre:split "\\n" str)))

(defun %%append-to-template (template key what)
  (let ((place-string #?"###$((string-downcase key))###"))
    (cond ((eq :simple-newline *indent-style*)
	   (cl-ppcre:regex-replace place-string template #?"$(what)\n$(place-string)"))
	  ((eq :smart-newline *indent-style*)
	   (cl-ppcre:regex-replace #?"( *)$(place-string)" template
				   (lambda (match indent)
				     (declare (ignore match))
				     #?"$(indent)$((crunch-in-indent indent what))\n$(indent)$(place-string)")
				   :simple-calls t))
	  (t (cl-ppcre:regex-replace place-string template #?"$(what)$(place-string)")))))

(defun %append-to-template (template key what)
  (if (not what)
      template
      (%append-to-template (%%append-to-template template key (car what)) key (cdr what))))

(defun append-to-template (template key &rest what)
  (%append-to-template template key what))


(defun %%prepend-to-template (template key what)
  (let ((place-string #?"###$((string-downcase key))###"))
    (cond ((eq :simple-newline *indent-style*)
	   (cl-ppcre:regex-replace place-string template #?"$(place-string)\n$((car what))"))
	  ((eq :smart-newline *indent-style*)
	   (cl-ppcre:regex-replace
	    #?"( *)$(place-string)" template
	    (lambda (match indent)
	      (declare (ignore match))
	      #?"$(indent)$(place-string)\n$(indent)$((crunch-in-indent indent what))")
	    :simple-calls t))
	  (t (cl-ppcre:regex-replace place-string template #?"$(place-string)$((car what))")))))

(defun %prepend-to-template (template key what)
  (if (not what)
      template
      (%prepend-to-template (%%prepend-to-template template key (car what)) key (cdr what))))

(defun prepend-to-template (template key &rest what)
  (%prepend-to-template template key what))

(defun replace-in-template (template key what)
  (let ((place-string #?"###$((string-downcase key))###"))
    (cond ((eq :smart-newline *indent-style*)
	   (cl-ppcre:regex-replace
	    #?"( *)$(place-string)" template
	    (lambda (match indent)
	      (declare (ignore match))
	      #?"$(indent)$((crunch-in-indent indent what))")
	    :simple-calls t))
	  (t (cl-ppcre:regex-replace place-string template #?"$(what)")))))
(defun replace-all-in-template (template key what)
  (let ((place-string #?"###$((string-downcase key))###"))
    (cond ((eq :smart-newline *indent-style*)
	   (cl-ppcre:regex-replace-all
	    #?"( *)$(place-string)" template
	    (lambda (match indent)
	      (declare (ignore match))
	      #?"$(indent)$((crunch-in-indent indent what))")
	    :simple-calls t))
	  (t (cl-ppcre:regex-replace-all place-string template #?"$(what)")))))

(defparameter *template-defaults* (make-hash-table))

(defun finalize-template (template)
  (iter (for (key val) in-hashtable *template-defaults*)
	(setf template (cl-ppcre:regex-replace-all #?"###$((string-downcase key))###"
						   template val)))
  (cl-ppcre:regex-replace-all "###[a-z0-9-]+###" template ""))

(defmacro finalize-template! ()
  `(finalize-template template))

(defun t-> (template key &rest what)
  (apply #'append-to-template `(,template ,key ,.what)))
(defun t<- (template key &rest what)
  (apply #'prepend-to-template `(,template ,key ,.what)))
(defun t<> (template key what)
  (replace-in-template template key what))
(defun t<<>> (template key what)
  (replace-all-in-template template key what))


(defmacro tt-> (templace key &rest what)
  `(setf ,templace (t-> ,templace ,key ,@what)))
(defmacro tt<- (templace key &rest what)
  `(setf ,templace (t<- ,templace ,key ,@what)))
(defmacro tt<> (templace key what)
  `(setf ,templace (t<> ,templace ,key ,what)))
(defmacro tt<<>> (templace key what)
  `(setf ,templace (t<<>> ,templace ,key ,what)))

(defmacro ttt-> (key &rest what)
  "We really want all the side-effects to be applied to the template before passing it to T->"
  `(let ((evaled-what (list ,@what)))
     (setf template (apply #'t-> (append (list template ,key)
					 evaled-what)))))
(defmacro ttt<- (key &rest what)
  `(let ((evaled-what (list ,@what)))
     (setf template (apply #'t<- (append (list template ,key)
					 evaled-what)))))
(defmacro ttt<> (key what)
  `(let ((evaled-what ,what))
     (setf template (t<> template ,key evaled-what))))
(defmacro ttt<<>> (key what)
  `(let ((evaled-what ,what))
     (setf template (t<<>> template ,key evaled-what))))

(defmacro define-template-modifier (name args &body body)
  "Conveniently operate on template by side effects"
  `(defun ,name ,args
     (declare (special template))
     (let ((template template))
       (declare (special template))
       ,@body
       template)))

(defmacro define-template-modifier! (name args &body body)
  "Same as DEFINE-TEMPLATE-MODIFIER, but with side-effects on the template being operated on,
and returning whatever"
  `(defun ,name ,args
     (declare (special template))
     ,@body))
