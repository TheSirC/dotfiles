;; nix -- Nix(OS) related configuration
(use-package! lsp-mode
  :config
  (advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers)))
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix")))

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["alejandra"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(customize-set-variable 'nix-nixfmt-bin "alejandra")
(set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))
