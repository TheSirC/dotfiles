(TeX-add-style-hook
 "personnel"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("caption" "small" "bf" "hang") ("cleveref" "noabbrev" "nameinlink") ("InriaSans" "lining")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "tabularx"
    "booktabs"
    "multicol"
    "imakeidx"
    "xcolor"
    "enumitem"
    "tocloft"
    "parskip"
    "amsmath"
    "amsfonts"
    "amssymb"
    "calc"
    "siunitx"
    "listings"
    "tdsfrmath"
    "caption"
    "sidecap"
    "cleveref"
    "autonum"
    "url"
    "cite"
    "hyperref"
    "InriaSans"
    "tikz"
    "pgfplots"
    "pgfplotstable"
    "placeins"
    "graphicx"
    "longtable"
    "todonotes"
    "epstopdf"
    "float"
    "subfig"
    "fancyhdr"
    "nag"
    "textcomp"
    "csvsimple"
    "filecontents")
   (TeX-add-symbols
    "newline"
    "oldnormalfont"
    "normalfont")
   (LaTeX-add-array-newcolumntypes
    "L"
    "C"
    "R"))
 :latex)

