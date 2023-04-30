;; Org-latex export configuration
(load! "org-latex")
;; Org buffer styling
;;(load! "theme")
;;
;; Improving the rendering (time & look) of LaTeX expressions
;; TODO: Make me work : package not found
;; (load! "faster-latex-previews")

(after! org
         (setq org-directory                  org-dir-path
               org-babel-R-command            "/usr/bin/env R --slave --no-save --encoding=UTF-8" ;; Diacritics characters on graphics
               org-html-coding-system         'utf-8
               org-hide-emphasis-markers      t
               org-pretty-entities            t
               org-startup-with-inline-images t
               org-image-actual-width         '(300))
        (add-to-list 'org-src-lang-modes '("r" . R))
        (add-to-list 'org-src-lang-modes '("tikz" . latex))
)


;; Citation configurations
(setq org-cite-csl-styles-dir "~/Zotero/styles"
      bibtex-completion-bibliography (getenv "BIBTEX_REF")
      ivy-bibtex-default-action 'ivy-bibtex-insert-key)
