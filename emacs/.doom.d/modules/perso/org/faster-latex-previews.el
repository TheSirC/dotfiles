;; From https://karthinks.com/software/scaling-latex-previews-in-emacs
(add-hook! 'org-mode-hook #'org-auctex-mode)

(map!
  :after org
  :mode 'org-mode
  :localleader
  :map org-mode-map
  :desc "LaTeX preview"
  :n ";" #'org-auctex-preview-dwim)
