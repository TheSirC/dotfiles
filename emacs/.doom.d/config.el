;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;; Doom related configuration
(load! "config/doom/theme")

;; Rust-related configuration
(load! "config/rust.el")

(setq!
  ;; Place your private configuration here
  dired-dwim-target t ; Allow to quickly move files/directories in dired
  ;; Relative line number for faster Vim movement
  display-line-numbers-type 'visual
  ;; Setting up evil-snipe to be more efficint
  evil-snipe-scope 'whole-line
  evil-snipe-spillover-scope 'whole-visible
  ;; Change the Org directory (for now)
  org-directory "/mnt/Long/Claude-Alban/Documents/Personnel/Org")
  ;; Deft-related parameters
  ;; deft-extensions '("txt" "md" "tex" "org")
  ;; deft-directory "/mnt/Long/Claude-Alban/Documents/Personnel/Org" ; TODO: This is not portable : set a ENV variable for each machine
  ;; deft-recursive t
  ;; deft-use-filename-as-title t)

;; Use delta for magit diffs
;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
