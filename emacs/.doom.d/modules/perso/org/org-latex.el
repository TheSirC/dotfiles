;; Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
(add-hook! 'org-mode-hook
           #'org-fragtog-mode
           #'evil-tex-mode)

(after! ox-latex

  (load! "org-latex-preview")

  (setq org-latex-pdf-process (list "latexmk -f -pdf -quiet -bibtex -view=none -output-directory=%o -pdflatex='%latex -interaction=nonstopmode --synctex=1 --shell-escape' %f")
        org-latex-default-figure-position "!htbp"  ;; Default position for LaTeX figures
        org-latex-prefer-user-labels t             ;; Use user defined labels instead
        org-export-with-smart-quotes t             ;; Activate smart quotes during export (convert " to \og, \fg in French)
        org-export-with-sub-superscripts '{}       ;; Interpret "_" and "^" for export when braces are used
        )

  ;; Adding impossibly invaluable packages
  (add-to-list 'org-latex-default-packages-alist '("separate-uncertainty=true,list-units=repeat,multi-part-units=brackets" "siunitx"                t nil))   ;; We want units in snippets
  (add-to-list 'org-latex-default-packages-alist '("" "multicol"               nil nil))
  (add-to-list 'org-latex-default-packages-alist '("" "imakeidx"               nil nil)) ;; For the index; must be loaded before            the 'caption' and 'hyperref' packages
  (add-to-list 'org-latex-default-packages-alist '("" "varioref"               nil nil)) ;; For the cross-references; must be loaded before the 'hyperref' and 'cleveref' packages
  (add-to-list 'org-latex-default-packages-alist '("" "tabularx"               nil nil))
  (add-to-list 'org-latex-default-packages-alist '("" "booktabs"               nil nil))
  (add-to-list 'org-latex-default-packages-alist '("" "tikz"                   nil nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pgfplots"               nil nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pstricks, pstricks-add" nil nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pst-plot, pst-node"     nil nil))
  (add-to-list 'org-latex-default-packages-alist '("" "multido"                nil nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pst-eucl"               nil nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pst-intersect"          nil nil))

  (add-to-list 'org-latex-classes
               '("kaohandt"
                 "\\documentclass{kaohandt}
% Packages required for the function of the class
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  )
