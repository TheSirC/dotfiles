;; LSP Server settings
(after! rustic
  (custom-set-variables
   '(lsp-rust-analyzer-display-chaining-hints t)
   '(lsp-rust-analyzer-display-parameter-hints nil)
   '(lsp-rust-analyzer-macro-expansion-method (quote rustic-analyzer-macro-expand))
   '(lsp-rust-analyzer-server-command (quote ("/~/.cargo/bin/rust-analyzer")))
   '(lsp-rust-analyzer-server-display-inlay-hints nil)
   '(lsp-rust-full-docs t)
   '(lsp-rust-server (quote rust-analyzer))
   '(rustic-lsp-server (quote rust-analyzer))
   '(lsp-ui-doc-alignment (quote window))
   '(lsp-ui-doc-position (quote top))
   '(lsp-ui-sideline-enable nil)
   )
  (push 'rustic-clippy flycheck-checkers)
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  (remove-hook 'rustic-mode-hook 'flycheck-mode) ; Turn off flycheck.

  ;; Client side settings
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-trigger 'on-save)
  (setq rustic-format-on-save t)
  )

;; for Christ's sake evil-surround those "<" aren't tags
(add-hook 'rust-mode-hook (lambda ()
                            (push '(?< . ("< " . " >")) evil-surround-pairs-alist)
                            (setq display-line-numbers 'relative)
                            )
          )
