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

(declare-function org-splice-tikz-header   "org" (tpl def-pkg pkg snippets-p &optional extra))

(defvar org-format-tikz-header nil)	 ; From org.el
(defvar org-format-tikz-options nil)	 ; From org.el
(defvar org-tikz-default-packages-alist nil) ; From org.el
(defvar org-tikz-packages-alist nil)	 ; From org.el

(defvar org-babel-default-header-args:tikz
  '((:results . "file graphics") (:exports . "results"))
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

(defgroup ob-tikz nil
  "Org-mode blocks for TikZ figures."
  :group 'org)

(defcustom org-babel-tikz-htlatex "htlatex"
  "The htlatex command to enable conversion of TikZ to SVG or HTML."
  :group 'ob-tikz
  :type 'string)

(defcustom org-babel-tikz-preamble
  (lambda (_)
    "\\documentclass[border = 10pt, convert, tikz]{standalone}
\\def\\pgfsysdriver{pgfsys-tex4ht.def}
\\usetikzlibrary{arrows, arrows.meta, calc, fit, patterns, positioning, shadings, shapes, shapes.multipart, intersections}
")
  "Closure which evaluates at runtime to the TikZ preamble.

It takes 1 argument which is the parameters of the source block."
  :group 'ob-tikz
  :type 'function)

(defcustom org-babel-tikz-begin-env
  (lambda (_) "\\begin{document}
\\begin{tikzpicture}\\n")
  "Function that evaluates to the begin part of the document environment.

It takes 1 argument which is the parameters of the source block.
This allows adding additional code that will be ignored when
exporting the literal TikZ source."
  :group 'ob-tikz
  :type 'function)

(defcustom org-babel-tikz-end-env
  (lambda (_) "\\n \\end{tikzpicture}
\\end{document}")
  "Closure which evaluates at runtime to the end part of the document environment.

It takes 1 argument which is the parameters of the source block.
This allows adding additional code that will be ignored when
exporting the literal TikZ source."
  :group 'ob-tikz
  :type 'function)

(defcustom org-babel-tikz-pdf-svg-process
  "inkscape --pdf-poppler %f -T -l -o %O"
  "Command to convert a PDF file to an SVG file."
  :group 'ob-tikz
  :type 'string)

(defcustom org-babel-tikz-htlatex-packages
  '("[usenames]{color}" "{amsmath}" "{pgfplots}")
  "Packages to use for htlatex export."
  :group 'ob-tikz
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
  "Execute a block of TikZ code with Babel.
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
         ((string= "png" extension)
	 (with-temp-file tex-file
	    (insert (concat (funcall org-babel-tikz-preamble params)
			    (mapconcat #'identity headers "\n")
			    (funcall org-babel-tikz-begin-env params)
			    body
			    (funcall org-babel-tikz-end-env params))))
	  (let ((tmp-pdf (org-babel-tikz-tex-to-pdf tex-file)))
            (org-babel-tikz-convert-pdf tmp-pdf out-file im-in-options im-out-options))
          )
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
         ((string= "tikz" extension)
	  (when (file-exists-p out-file) (delete-file out-file))
	  (with-temp-file out-file (insert body))
          )
	 ((string= "pdf" extension)
         (with-temp-file tex-file (insert (concat (funcall org-babel-tikz-preamble params)
                                                        (mapconcat #'identity headers "\n")
                                                        (funcall org-babel-tikz-begin-env params)
                                                        body
                                                        (funcall org-babel-tikz-end-env params))))
         (let* ((tmp-pdf (org-babel-tikz-tex-to-pdf tex-file)))
           (shell-command (format "mv %s %s" tmp-pdf out-file)))
            )
         )
        nil) ;; signal that output has already been written to file
    body))

(defun org-babel-prep-session:tikz (_session _params)
  "Return an error because TikZ doesn't support sessions."
  (error "TikZ does not support sessions"))

(defun org-babel-tikz-create-tex-file (filename headers params body)
  "Create the TeX file."
  (let (( _res (with-temp-file filename
                 (insert (concat (funcall org-babel-tikz-preamble params)
		                 (mapconcat #'identity headers "\n")
		                 (funcall org-babel-tikz-begin-env params)
		                 body
		                 (funcall org-babel-tikz-end-env params)
                                 )))))
    filename)
  )

(defun org-babel-tikz-convert-pdf (pdffile out-file im-in-options im-out-options)
  "Generate a file from a pdf file using imagemagick."
  (let ((cmd (concat "convert " im-in-options " " pdffile " "
		     im-out-options " " out-file)))
    (message "Converting pdffile file %s..." cmd)
    (shell-command cmd)))

(defun org-babel-tikz-tex-to-pdf (file)
  "Generate a pdf file according to the contents FILE."
  (require 'ox-latex)
  (org-latex-compile file))

(eval-after-load "org"
  (lambda ()
    (add-to-list 'org-src-lang-modes         '("tikz" . latex))
    (add-to-list 'org-babel-tangle-lang-exts '("tikz" . "tikz"))
    ))

(provide 'ob-tikz)

;;; ob-tikz.el ends here
