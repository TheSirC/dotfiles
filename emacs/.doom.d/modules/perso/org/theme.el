;; (use-package! org-modern
;;   :hook (org-mode . org-modern-mode)
;;   :config
;;   (setq org-modern-star ["◉" "○" "✸" "◆" "✤" "✜" "✿" "▶"]
;;         org-modern-table-vertical 1
;;         org-modern-table-horizontal 0.2
;;         org-modern-list '((43 . "➤")
;;                           (45 . "–")
;;                           (42 . "•"))
;;         org-modern-footnote
;;         (cons nil (cadr org-script-display))
;;         org-modern-progress nil
;;         org-modern-priority nil))

;; TODO: Find out why this errs
;; (custom-set-faces!
;;   '(org-modern-statistics :inherit org-checkbox-statistics-todo)
;;   '(writegood-duplicates-face    ((t nil)))
;;   '(writegood-passive-voice-face ((t nil))))


(let* ((variable-tuple
        (cond ('(:font "Inria Sans"))
              ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8        ((t (,@headline ,@variable-tuple))))
   `(org-level-7        ((t (,@headline ,@variable-tuple))))
   `(org-level-6        ((t (,@headline ,@variable-tuple))))
   `(org-level-5        ((t (,@headline ,@variable-tuple))))
   `(org-level-4        ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3        ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2        ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1        ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Inria Sans"    :height 110 :weight thin))))
 '(fixed-pitch    ((t (:family "IBM Plex Mono" :height 110)))))

(custom-theme-set-faces
 'user
 '(org-block                 ((t (:inherit fixed-pitch))))
 '(org-code                  ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info         ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
 '(org-latex-and-related     ((t (:inherit (fixed-pitch)))))
 '(org-link                  ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value        ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table                 ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim              ((t (:inherit (shadow fixed-pitch))))))

(setq
 ;; Edit settings
 org-auto-align-tags                nil
 org-tags-column                    0
 org-special-ctrl-a/e               t
 org-insert-heading-respect-content t
 org-startup-with-inline-images     t
 org-startup-with-latex-preview     t
 org-image-actual-width             800

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities       t
 org-ellipsis              " ▾ "

 ;; Source code
 org-src-fontify-natively          t
 org-src-tab-acts-natively         t

 ;; Agenda styling
 org-agenda-tags-column     0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "—"))))))

;; Pixel-perfect visual alignment for Org and Markdown tables
;; (use-package! valign
;;   :hook   (org-mode . valign-mode)
;;   :config (setq valign-fancy-bar t))

(add-hook! 'org-mode-hook
           #'visual-line-mode
           #'org-bullets-mode)
