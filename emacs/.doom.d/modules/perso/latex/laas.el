(use-package! laas
  :hook (LaTeX-mode . laas-mode)
  :hook (org-mode   . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
    :cond #'texmathp ; expand only while in math
    "Sum"    (cmd! (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
    "Prod"   (cmd! (yas-expand-snippet "\\prod_{$1}^{$2} $0"))
    "Int"    (cmd! (yas-expand-snippet "\\int_{$1}^{$2} $0"))
    "IInt"   (cmd! (yas-expand-snippet "\\iint_{$1}^{$2} $0"))
    "IIInt"  (cmd! (yas-expand-snippet "\\iiint_{$1}^{$2} $0"))
    "Eq"     (cmd! (yas-expand-snippet "$0 = $1"))
    "Aeq"    (cmd! (yas-expand-snippet "$0 = $1"))
    "Neq"    (cmd! (yas-expand-snippet "$0 \\neq $1"))
    "Ineqs"  (cmd! (yas-expand-snippet "$0 \\geq $1"))
    "Ineqi"  (cmd! (yas-expand-snippet "$0 \\leq $1"))
    "Deriv"  (cmd! (yas-expand-snippet "\\dfrac{d$0}{d$1}"))
    "Pderiv" (cmd! (yas-expand-snippet "\\dfrac{\\partial $0}{\\partial $1}"))
    ";2p"    (cmd! (yas-expand-snippet "2 \\pi"))
    ;; add accent snippets
    :cond (lambda () (and (texmathp) (laas-object-on-left-condition)))
    "'B"     (cmd! (laas-wrap-previous-object "mathbb"))
    "qq"     (cmd! (laas-wrap-previous-object "sqrt"))
    ",si"    (cmd! (laas-wrap-previous-object "sin"))
    ",co"    (cmd! (laas-wrap-previous-object "cos"))
    ",ta"    (cmd! (laas-wrap-previous-object "tan"))))
