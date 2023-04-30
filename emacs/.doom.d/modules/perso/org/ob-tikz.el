;;; ob-tikz.el --- Babel Functions for TikZ        -*- lexical-binding: t; -*-

;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;;; Commentary:

;; Org-Babel support for evaluating TikZ source code.
;;
;; Currently on evaluation this returns raw TikZ code, unless a :file
;; header argument is given in which case small png or pdf files will
;; be created directly form the tikz source code.

;;; Code:
(require 'ob)
(require 'org-macs)

(declare-function org-create-formula-image "org" (string tofile options buffer &optional type))
(declare-function org-tikz-compile "ox-latex" (texfile &optional snippet))
(declare-function org-tikz-guess-inputenc "ox-latex" (header))
(declare-function org-splice-tikz-header "org" (tpl def-pkg pkg snippets-p &optional extra))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("tikz" . "tikz"))

(defvar org-format-tikz-header)	  ; From org.el
(defvar org-format-tikz-options)	  ; From org.el
(defvar org-tikz-default-packages-alist) ; From org.el
(defvar org-tikz-packages-alist)	  ; From org.el

(defvar org-babel-default-header-args:tikz
  '((:results . "latex") (:exports . "results"))
  "Default arguments to use when evaluating a TikZ source block.")

(defconst org-babel-header-args:tikz
  '((border       . :any)
    (fit          . :any)
    (imagemagick  . ((nil t)))
    (iminoptions  . :any)
    (imoutoptions . :any)
    (packages     . :any)
    (pdfheight    . :any)
    (pdfpng       . :any)
    (pdfwidth     . :any)
    (headers      . :any)
    (packages     . :any)
    (buffer       . ((yes no))))
  "TikZ-specific header arguments.")

(defcustom org-babel-tikz-htlatex "htlatex"
  "The htlatex command to enable conversion of TikZ to SVG or HTML."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-tikz-preamble
  (lambda (_)
    "\\documentclass[preview]{standalone}
\\def\\pgfsysdriver{pgfsys-tex4ht.def}
")
  "Closure which evaluates at runtime to the TikZ preamble.

It takes 1 argument which is the parameters of the source block."
  :group 'org-babel
  :type 'function)

(defcustom org-babel-tikz-begin-env
  (lambda (_) "\\begin{document}")
  "Function that evaluates to the begin part of the document environment.

It takes 1 argument which is the parameters of the source block.
This allows adding additional code that will be ignored when
exporting the literal TikZ source."
  :group 'org-babel
  :type 'function)

(defcustom org-babel-tikz-end-env
  (lambda (_)
    "\\end{document}")
  "Closure which evaluates at runtime to the end part of the document environment.

It takes 1 argument which is the parameters of the source block.
This allows adding additional code that will be ignored when
exporting the literal TikZ source."
  :group 'org-babel
  :type 'function)

(defcustom org-babel-tikz-pdf-svg-process
  "inkscape --pdf-poppler %f -T -l -o %O"
  "Command to convert a PDF file to an SVG file."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-tikz-htlatex-packages
  '("[usenames]{color}" "{tikz}" "{color}" "{listings}" "{amsmath}")
  "Packages to use for htlatex export."
  :group 'org-babel
  :type '(repeat (string)))

(defun org-babel-expand-body:tikz (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (mapc (lambda (pair) ;; replace variables
          (setq body
                (replace-regexp-in-string
                 (regexp-quote (format "%S" (car pair)))
                 (if (stringp (cdr pair))
                     (cdr pair) (format "%S" (cdr pair)))
                 body)))
	(org-babel--get-vars params))
  (org-trim body))

(defun org-babel-execute:tikz (body params)
  "Execute a block of Latex code with Babel.
This function is called by `org-babel-execute-src-block'."
  (setq body (org-babel-expand-body:tikz body params))
  (if (cdr (assq :file params))
      (let* ((out-file (cdr (assq :file params)))
	     (extension (file-name-extension out-file))
	     (tex-file (org-babel-temp-file "tikzpicture-" ".tikz"))
	     (border (cdr (assq :border params)))
	     (imagemagick (cdr (assq :imagemagick params)))
	     (im-in-options (cdr (assq :iminoptions params)))
	     (im-out-options (cdr (assq :imoutoptions params)))
	     (fit (or (cdr (assq :fit params)) border))
	     (height (and fit (cdr (assq :pdfheight params))))
	     (width (and fit (cdr (assq :pdfwidth params))))
	     (headers (cdr (assq :headers params)))
	     (in-buffer (not (string= "no" (cdr (assq :buffer params)))))
	     (org-tikz-packages-alist
	      (append (cdr (assq :packages params)) org-tikz-packages-alist)))
        (cond
         ((and (string-suffix-p ".png" out-file) (not imagemagick))
          (let ((org-format-tikz-header
		 (concat org-format-tikz-header "\n"
			 (mapconcat #'identity headers "\n"))))
	    (org-create-formula-image
             body out-file org-format-tikz-options in-buffer)))
	 ((string= "svg" extension)
	  (with-temp-file tex-file
	    (insert (concat (funcall org-babel-tikz-preamble params)
			    (mapconcat #'identity headers "\n")
			    (funcall org-babel-tikz-begin-env params)
			    body
			    (funcall org-babel-tikz-end-env params))))
	  (let ((tmp-pdf (org-babel-tikz-tex-to-pdf tex-file)))
            (let* ((log-buf (get-buffer-create "*Org Babel TikZ Output*"))
                   (err-msg "org babel tikz failed")
                   (img-out (org-compile-file
	                     tmp-pdf
                             (list org-babel-tikz-pdf-svg-process)
                             extension err-msg log-buf)))
              (shell-command (format "mv %s %s" img-out out-file)))))
         ((string-suffix-p ".tikz" out-file)
	  (when (file-exists-p out-file) (delete-file out-file))
	  (with-temp-file out-file
	    (insert body)))
	 ((and (string= "html" extension)
	       (executable-find org-babel-tikz-htlatex))
	  ;; TODO: this is a very different way of generating the
	  ;; frame tikz document than in the pdf case.  Ideally, both
	  ;; would be unified.  This would prevent bugs creeping in
	  ;; such as the one fixed on Aug 16 2014 whereby :headers was
	  ;; not included in the SVG/HTML case.
	  (with-temp-file tex-file
	    (insert (concat
		     "\\documentclass[preview]{standalone}
\\def\\pgfsysdriver{pgfsys-tex4ht.def}
"
		     (mapconcat (lambda (pkg)
				  (concat "\\usepackage" pkg))
				org-babel-tikz-htlatex-packages
				"\n")
		     (if headers
			 (concat "\n"
				 (if (listp headers)
				     (mapconcat #'identity headers "\n")
				   headers) "\n")
		       "")
		     "\\begin{document}"
		     body
		     "\\end{document}")))
	  (when (file-exists-p out-file) (delete-file out-file))
	  (let ((default-directory (file-name-directory tex-file)))
	    (shell-command (format "%s %s" org-babel-tikz-htlatex tex-file)))
	  (cond
	   ((file-exists-p (concat (file-name-sans-extension tex-file) "-1.svg"))
	    (if (string-suffix-p ".svg" out-file)
		(progn
		  (shell-command "pwd")
		  (shell-command (format "mv %s %s"
					 (concat (file-name-sans-extension tex-file) "-1.svg")
					 out-file)))
	      (error "SVG file produced but HTML file requested")))
	   ((file-exists-p (concat (file-name-sans-extension tex-file) ".html"))
	    (if (string-suffix-p ".html" out-file)
		(shell-command "mv %s %s"
			       (concat (file-name-sans-extension tex-file)
				       ".html")
			       out-file)
	      (error "HTML file produced but SVG file requested")))))
	 ((or (string= "pdf" extension) imagemagick)
	  (with-temp-file tex-file
	    (require 'ox-latex)
	    (insert
	     (org-tikz-guess-inputenc
	      (org-splice-tikz-header
	       org-format-tikz-header
	       (delq
		nil
		(mapcar
		 (lambda (el)
		   (unless (and (listp el) (string= "hyperref" (cadr el)))
		     el))
		 org-tikz-default-packages-alist))
	       org-tikz-packages-alist
	       nil))
	     (if fit "\n\\usepackage[active, tightpage]{preview}\n" "")
	     (if border (format "\\setlength{\\PreviewBorder}{%s}" border) "")
	     (if height (concat "\n" (format "\\pdfpageheight %s" height)) "")
	     (if width  (concat "\n" (format "\\pdfpagewidth %s" width))   "")
	     (if headers
		 (concat "\n"
			 (if (listp headers)
			     (mapconcat #'identity headers "\n")
			   headers) "\n")
	       "")
	     (if fit
		 (concat "\n\\begin{document}\n\\begin{preview}\n" body
			 "\n\\end{preview}\n\\end{document}\n")
	       (concat "\n\\begin{document}\n" body "\n\\end{document}\n"))))
          (when (file-exists-p out-file) (delete-file out-file))
	  (let ((transient-pdf-file (org-babel-tikz-tex-to-pdf tex-file)))
	    (cond
	     ((string= "pdf" extension)
	      (rename-file transient-pdf-file out-file))
	     (imagemagick
	      (org-babel-tikz-convert-pdf
	       transient-pdf-file out-file im-in-options im-out-options)
	      (when (file-exists-p transient-pdf-file)
		(delete-file transient-pdf-file)))
	     (t
	      (error "Can not create %s files, please specify a .png or .pdf file or try the :imagemagick header argument"
		     extension))))))
        nil) ;; signal that output has already been written to file
    body))

(defun org-babel-tikz-convert-pdf (pdffile out-file im-in-options im-out-options)
  "Generate a file from a pdf file using imagemagick."
  (let ((cmd (concat "convert " im-in-options " " pdffile " "
		     im-out-options " " out-file)))
    (message "Converting pdffile file %s..." cmd)
    (shell-command cmd)))

(defun org-babel-tikz-tex-to-pdf (file)
  "Generate a pdf file according to the contents FILE."
  (require 'ox-latex)
  (org-tikz-compile file))

(defun org-babel-prep-session:tikz (_session _params)
  "Return an error because TikZ doesn't support sessions."
  (error "TikZ does not support sessions"))

(provide 'ob-tikz)

;;; ob-tikz.el ends here
