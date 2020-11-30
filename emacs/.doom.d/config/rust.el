;; LSP Server settings
(after! rustic
  (setq
   lsp-rust-full-docs t
   lsp-rust-analyzer-macro-expansion-method (quote rustic-analyzer-macro-expand)
   lsp-rust-analyzer-server-command "rust-analyzer"
   lsp-rust-analyzer-cargo-watch-command "clippy"
   ;; LSP Hints
   lsp-rust-analyzer-display-chaining-hints t
   lsp-rust-analyzer-display-parameter-hints t
   lsp-rust-analyzer-server-display-inlay-hints t

   lsp-ui-doc-alignment (quote window)
   lsp-ui-doc-position (quote top)
   ;; Rustic-specific settings
   rustic-lsp-server (quote rust-analyzer)
   rustic-lsp-client (quote lsp-mode) ; Client side settings
   rustic-lsp-format t
   rustic-format-trigger (quote on-compile))


  (after! smartparens
    (sp-with-modes 'rustic
      (sp-local-pair "'" nil))) (setq rustic-format-on-save t))


;; for Christ's sake evil-surround those "<" aren't tags
(add-hook 'rust-mode-hook (lambda ()
                            (push '(?< . ("< " . " >")) evil-surround-pairs-alist)
                            (setq display-line-numbers 'relative)
                            )
          )
