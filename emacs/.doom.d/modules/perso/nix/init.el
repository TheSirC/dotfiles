;; NixOS cleanup (need to point to the correct places)
(defvar-local nixos-config-path (getenv "OUROBOROS") "The path on the current host to the NixOS config folder.")
(defvar-local doom-config-path (concat nixos-config-path "/dotfiles/emacs/.doom.d") "The path on the current host to the doom config folder.")
(setq doom-user-dir      doom-config-path
      user-emacs-directory  (concat doom-config-path "/emacs/")
      +snippets-dir         (concat doom-config-path "/snippets")
     doom-local-dir        doom-etc-dir)

(advice-add 'doom/reload :before (lambda (&rest r) (setq doom-private-dir doom-config-path)))
