;; NixOS cleanup (need to point to the correct places)
(defvar-local nixos-config-path (getenv "OUROBOROS") "The path on the current host to the NixOS config folder.")
(defvar-local doom-config-path (concat nixos-config-path "/dotfiles/emacs/.doom.d") "The path on the current host to the doom config folder.")
(setq doom-private-dir     doom-config-path
      doom-snippets-dir    (concat doom-config-path "/snippets")
      doom-modules-dir     (concat doom-config-path "/modules/")
      +snippets-dir        (concat doom-config-path "/snippets")
      user-emacs-directory (concat doom-config-path "/emacs/")
      doom-modules-dirs    (add-to-list 'doom-modules-dirs (concat doom-config-path "/modules/"))
      doom-local-dir       doom-etc-dir
      user-emacs-directory-warning nil)

(advice-add 'doom/reload :before (lambda (&rest r) (setq doom-private-dir doom-config-path)))
