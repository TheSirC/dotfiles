;;; perso/org/keymaps.el -*- lexical-binding: t; -*-
;;(load! "si-unit")

(defun +org-open-math ()
  "Surround point with math parentheses, or if already in math move point to exit it."
  (interactive)
  (insert "\\(")
  (save-excursion (insert "\\)")))
(defun +org-open-display-math ()
  "Open a displaymath context,or if already in math move point to exit it."
  (interactive)
  (insert "\\[ ")
  (save-excursion
    (insert " \\]\n")))
(defun +org-open-align ()
  "Open an aligned context, or if already in math move point to exit it."
  (interactive)
  (insert "\\begin{align}\n")
  (save-excursion
    (insert "\n\\end{align}\n")))
(defun +org-open-align-s ()
  "Open an not numbered aligned context, or if already in math move pokint to exit it."
  (interactive)
  (insert "\\begin{align*}\n")
  (save-excursion
    (insert "\n\\end{align*}\n")))
(defun +org-open-eq ()
  "Open an aligned context, or if already in math move point to exit it."
  (interactive)
  (insert "\\begin{equation}\n")
  (save-excursion
    (insert "\n\\end{equation}\n")))
(defun +org-open-eq-s ()
  "Open an aligned context, or if already in math move point to exit it."
  (interactive)
  (insert "\\begin{equation*}\n")
  (save-excursion (insert "\n\\end{equation*}\n")))

(map! :after org
      :mode 'org-mode
      :localleader
      :map org-mode-map
      :prefix "m" :desc "Math"
      :desc "Insert math"      :n "m" #'+org-open-math
      :desc "Insert Math"      :n "M" #'+org-open-display-math
      :desc "Insert align"     :n "r" #'+org-open-align
      :desc "Insert align*"    :n "R" #'+org-open-align-s
      :desc "Insert equation"  :n "e" #'+org-open-eq
      :desc "Insert equation*" :n "E" #'+org-open-eq-s
      :desc "Close math env."  :n "c" #'LaTeX-close-environment)

(map! :after org
      :mode 'org-mode
      :localleader
      :map org-mode-map
      :desc "Toggle ligatures" :n "L" #'org-toggle-pretty-entities)

(map! :after org
      :mode 'org-mode
      :desc "Toggle inline images" :n "z i" #'org-toggle-inline-images)

(map! :after org
      :mode 'org-mode
      :localleader
      :map org-mode-map
      :desc "Transpose table" :n "b T" #'org-table-transpose-table-at-point)
