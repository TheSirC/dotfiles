; Change the nix formatter
(add-hook 'nix-mode-hook (lambda () (customize-set-variable 'nix-nixfmt-bin "nixpkgs-fmt")))
(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  :major-modes '(nix-mode)
                  :server-id 'nix))
