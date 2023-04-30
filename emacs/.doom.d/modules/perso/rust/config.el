;; rust -- Rust related configuration
;;

(when (featurep! :lang rust +lsp)

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

   ;; LSP UI docs parameters
   lsp-ui-doc-alignment (quote window)
   lsp-ui-doc-position (quote top)

   ;; Rustic-specific settings
   rustic-lsp-format t
   rustic-rustfmt-args "--edition=2021"
   rustic-flycheck-clippy-params "--message-format=json"
   rustic-format-trigger (quote on-compile))

  (after! smartparens
    (sp-with-modes 'rustic
      (sp-local-pair "'" nil))) (setq rustic-format-on-save t))
  )
