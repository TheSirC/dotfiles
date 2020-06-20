;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;; Doom related configuration
(load! "config/doom/theme")

;; Rust-related configuration
(load! "config/rust.el")

;; Place your private configuration here
(setq dired-dwim-target t) ; Allow to quickly move files/directories in dired

;; Relative line number for faster Vim movement
(setq display-line-numbers-type 'visual)

;; Setting up evil-snipe to be more efficint
(setq evil-snipe-scope 'whole-line
      evil-snipe-spillover-scope 'whole-visible)

; Change the Org directory (for now)
(setq org-directory "/mnt/Long/Claude-Alban/Documents/Personnel/Org")
