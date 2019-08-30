;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq dired-dwim-target t) ; Allow to quickly move files/directories in dired
(direnv-mode) ; Auto-start direnv
;; Relative line number for faster Vim movement
(setq display-line-numbers t)
(setq display-line-numbers-mode 'relative)
(setq display-line-numbers-type 'relative)

; for Christ's sake evil-surround those "<" aren't tags
(add-hook 'rust-mode-hook (lambda ()
                            (push '(?< . ("< " . " >")) evil-surround-pairs-alist)
                            )
)
; Change the Org directory (for now)
; TODO: Change this directory to something local when the Windows transfert is done
(setq org-directory "/mnt/Long/Claude-Alban/Documents/Personnel/Org")
