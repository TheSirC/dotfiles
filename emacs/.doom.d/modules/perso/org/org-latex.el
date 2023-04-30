;; Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them
;; TODO: File mode specification error: (doom-hook-error org-mode-hook org-fragtog-mode (void-function org-fragtog-mode))
(add-hook 'org-mode-hook 'org-fragtog-mode)
(after! ox-latex
  (setq org-latex-pdf-process (list "latexmk -f -pdf -quiet -bibtex -view=none -pdflatex='xelatex -interaction=nonstopmode --shell-escape' %f")
        org-latex-prefer-user-labels t             ;; Use user defined labels instead
        org-latex-default-figure-position "!htbp"  ;; Default position for LaTeX figures
        org-export-with-smart-quotes t             ;; activate smart quotes during export (convert " to \og, \fg in French)
        org-export-with-sub-superscripts '{}       ;; interpret "_" and "^" for export when braces are used
        )

  ;; Adding impossibly invaluable packages
  (add-to-list 'org-latex-default-packages-alist '("" "siunitx"       t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "multicol"      t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "imakeidx"      t nil)) ;; For the index; must be loaded before the 'caption' and 'hyperref' packages
  (add-to-list 'org-latex-default-packages-alist '("" "varioref"      t nil)) ;; For the cross-references; must be loaded before the 'hyperref' and 'cleveref' packages
  (add-to-list 'org-latex-default-packages-alist '("" "sectsty"       t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "tabularx"      t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "booktabs"      t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "tikz"          t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pgfplots"      t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "epstopdf"      t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pstricks"      t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pst-plot"      t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pst-node"      t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "multido"       t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pstricks-add"  t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pst-eucl"      t nil))
  (add-to-list 'org-latex-default-packages-alist '("" "pst-intersect" t nil))

  ;; Adding i18n to the latex export
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel"       t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

  (add-to-list 'org-latex-classes
               '("kaobook"
                "\\documentclass{kaobook}
% Packages required for the function of the class
\\usepackage{etoolbox}                           % Easy programming to modify TeX stuff
\\usepackage{calc}                               % Make calculations
\\usepackage[usenames,dvipsnames,table]{xcolor}  % Colours
\\usepackage{iftex}                              % Check wether XeTeX is being used
\\usepackage{xifthen}                            % Easy conditionals
\\usepackage{options}                            % Manage class options
\\usepackage{xparse}                             % Parse arguments for macros
\\usepackage{xpatch}                             % Patch LaTeX code in external packages
\\usepackage{xstring}                            % Parse strings
\\usepackage{afterpage}                          % Run commands after specific pages

[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("kaohandt"
                "\\documentclass{kaohandt}
% Packages required for the function of the class
\\usepackage{etoolbox}                           % Easy programming to modify TeX stuff
\\usepackage{calc}                               % Make calculations
\\usepackage[usenames,dvipsnames,table]{xcolor}  % Colours
\\usepackage{iftex}                              % Check wether XeTeX is being used
\\usepackage{xifthen}                            % Easy conditionals
\\usepackage{options}                            % Manage class options
\\usepackage{xparse}                             % Parse arguments for macros
\\usepackage{xpatch}                             % Patch LaTeX code in external packages
\\usepackage{xstring}                            % Parse strings
\\usepackage{afterpage}                          % Run commands after specific pages

[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  )
