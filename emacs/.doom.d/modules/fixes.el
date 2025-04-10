;;; fixes.el -*- lexical-binding: t; -*-

(setq dired-dwim-target                     t  ;; Allow                                    to quickly move files/directories in dired
      global-auto-revert-mode               t  ;; Auto refresh files
      which-key-allow-multiple-replacements t
      +lookup-dictionary-prefer-offline     t
      yas-triggers-in-field                 t  ;; Nested snippets are good, so let’s enable that.
      projectile-require-project-root 'prompt) ;; Projectile will ask you to select a project

;; I want snippets to modify the buffer
;; WARN: This might mean that some snippets can do bad stuff without warning (?)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; Add an extra line to work around bug in which-key imprecise
(defun add-which-key-line (f &rest r) "This is supposed to help with cropped which-key frame." (progn (apply f (list (cons (+ 1 (car (car r))) (cdr (car r)))))))
(advice-add 'which-key--show-popup :around #'add-which-key-line)

(after! lsp (advice-remove #'lsp #'+lsp-dont-prompt-to-install-servers-maybe-a))

(add-hook 'doom-first-buffer-hook
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))))
