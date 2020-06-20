; Change the nix formatter
(add-hook 'nix-mode-hook (lambda () (customize-set-variable 'nix-nixfmt-bin "nixpkgs-fmt")))
