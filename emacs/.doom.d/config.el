;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Claude-Alban RANÉLY-VERGÉ-DÉPRÉ"
      user-mail-address (getenv "EMAIL"))

(defvar org-dir-path (getenv "SCIENCIA_PATH") "The path on the current host to the Org files.")

;; Obsfuscate my screen
(load! "modules/ligatures")

;; Keymaps related configuration
(load! "modules/keymaps")

(setq display-line-numbers      'visual            ;; Relative line number for faster Vim movement
      display-line-numbers-type 'relative)

(setq dired-dwim-target        t                   ;; Allow to quickly move files/directories in dired
      global-auto-revert-mode  t                   ;; Auto refresh files
      projectile-require-project-root 'prompt)     ;; Projectile will ask you to select a project

(use-package-hook! evil
  :pre-init
  (setq evil-respect-visual-line-mode t)           ;; Sane j and k behavior
  t)

(after! evil-snipe
      (setq evil-snipe-scope           'whole-line ;; Setting up evil-snipe to be more efficient
            evil-snipe-spillover-scope 'whole-visible)
)

;; Deft-related parameters
(setq deft-directory org-dir-path
      deft-extensions '("txt" "md" "tex" "org")
      deft-recursive              t
      deft-use-filename-as-title  t)

;; Switch to the new window after splitting
(setq evil-split-window-below   t
      evil-vsplit-window-right  t)

(after! lsp (advice-remove #'lsp #'+lsp-dont-prompt-to-install-servers-maybe-a))

;; Use delta for magit diffs
(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; I want snippets to modify the buffer
;; WARN: This might mean that some snippets can do bad stuff without warning (?)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; Add an extra line to work around bug in which-key imprecise
(defun add-which-key-line (f &rest r) "This is supposed to help with cropped which-key frame." (progn (apply f (list (cons (+ 1 (car (car r))) (cdr (car r)))))) )
(advice-add 'which-key--show-popup :around #'add-which-key-line)
