;;"The path on the current host to the Org files."
(setq org-dir-path (getenv "SCIENCIA_PATH"))

;; Org-latex export configuration
(load! "org-latex")
;; Org buffer styling
(load! "theme")
;; Improving the rendering (time & look) of LaTeX expressions
(load! "faster-latex-previews")
;; Ligatures
(load! "ligatures")
;; Keymaps
(load! "keymaps")
;; Deft
(load! "deft")

(after! org
         (setq org-directory                     org-dir-path
               org-babel-R-command               "/usr/bin/env R --slave --no-save --encoding=UTF-8" ;; Diacritics characters on graphics
               org-html-coding-system            'utf-8
               org-pretty-entities               t
               org-startup-with-inline-images    t
               org-export-allow-bind-keywords    t
               org-confirm-babel-evaluate        nil)
         (add-to-list 'org-src-lang-modes '("r" . R))
         (add-to-list 'org-src-lang-modes '("tikz" . latex))
         (add-to-list 'org-src-lang-modes '("pst" . latex)))

(use-package! ox-extra
  :config (ox-extras-activate '(latex-header-blocks ignore-headlines)))

;; Citation configurations
(setq org-cite-csl-styles-dir "~/Zotero/styles"
      bibtex-completion-bibliography (getenv "BIBTEX_REF")
      ivy-bibtex-default-action 'ivy-bibtex-insert-key)
