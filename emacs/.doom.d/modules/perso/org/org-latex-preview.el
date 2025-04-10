;;; perso/org/org-latex-preview.el -*- lexical-binding: t; -*-

;; Latex preview
(defvar luamagick
'(luamagick
        :programs ("lualatex" "convert")
        :description "pdf > png"
        :message "you need to install lualatex and imagemagick."
        :use-xcolor t
        :image-input-type "pdf"
        :image-output-type "png"
        :image-size-adjust (1.0 . 1.0)
        :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
        :image-converter ("convert -density %D -trim -antialias %f -quality 90 %O")))
(add-to-list 'org-preview-latex-process-alist luamagick)

(defvar luasvg
'(luasvg
        :programs ("lualatex" "dvisvgm")
        :description "dvi > svg"
        :message "you need to install lualatex and dvisvgm."
        :use-xcolor t
        :image-input-type "dvi"
        :image-output-type "svg"
        :image-size-adjust (1.0 . 1.0)
        :latex-compiler ("lualatex -interaction nonstopmode -output-format dvi -output-directory %o %f")
        :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))
(add-to-list 'org-preview-latex-process-alist luasvg)

(setq org-preview-latex-default-process 'luasvg
        org-format-latex-header "\\documentclass{standalone}\n\\usepackage[usenames]{color}\n\\usepackage{siunitx}\n\\usepackage{amsfonts,amsmath,amssymb}\n\\pagestyle{empty}\n\\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}"
        org-format-latex-options          (plist-put org-format-latex-options :background "Transparent"))
