;; From https://karthinks.com/software/scaling-latex-previews-in-emacs
(add-hook! org-mode #'org-auctex-mode)

(map!
  :mode org-mode
  :localleader
  :desc "LaTeX preview"
  :n ";" #'org-auctex-preview-dwim)
