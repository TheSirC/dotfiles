(use-package! laas
  :hook (LaTeX-mode . laas-mode)
  :hook (org-mode .   laas-mode)
  :config
  (aas-set-snippets 'laas-mode
                    :cond #'texmathp ; expand only while in math
                    "Sum"    (lambda () (interactive) (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Int"    (lambda () (interactive) (yas-expand-snippet "\\int_{$1}^{$2} $0"))
                    "IInt"   (lambda () (interactive) (yas-expand-snippet "\\iint_{$1}^{$2} $0"))
                    "IIInt"  (lambda () (interactive) (yas-expand-snippet "\\iiint_{$1}^{$2} $0"))
                    "Eq"     (lambda () (interactive) (yas-expand-snippet "$0 = $1"))
                    "Neq"    (lambda () (interactive) (yas-expand-snippet "$0 \\neq $1"))
                    "Ineqs"  (lambda () (interactive) (yas-expand-snippet "$0 \\geq $1"))
                    "Ineqi"  (lambda () (interactive) (yas-expand-snippet "$0 \\leq $1"))
                    "Deriv"  (lambda () (interactive) (yas-expand-snippet "\\dfrac{d$0}{d$1}"))
                    "Pderiv" (lambda () (interactive) (yas-expand-snippet "\\dfrac{\\partial $0}{\\partial $1}"))
                    ";2p"    (lambda () (interactive) (yas-expand-snippet "2 \\pi"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq"     (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
))
