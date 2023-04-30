;; nix -- Nix(OS) related configuration
(use-package! lsp-mode
  :config
  (advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers)))
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(add-hook! nix-mode #'lsp!)
(customize-set-variable 'nix-nixfmt-bin "alejandra")
(set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode))
