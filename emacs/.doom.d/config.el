;; ~/.DOOM.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Claude-Alban RANÉLY-VERGÉ-DÉPRÉ"
      user-mail-address (getenv "EMAIL")
      user-emacs-directory-warning nil)

;; Default mode as text-mode
(setq-default major-mode 'text-mode)

;; Obsfuscate my screen
(load! "modules/ligatures")

;; Change THESE strings
(load! "modules/string-inflection")

;; Keymaps related configuration
(load! "modules/keymaps")

;; Lisp config
(load! "modules/lisp")

;; Collection of fixes for bugs found that are affecting our way of lisp
(load! "modules/fixes")

;; Use delta for magit diffs
(use-package! magit-delta
  :if (executable-find "delta")
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :commands magit-delta-mode)

