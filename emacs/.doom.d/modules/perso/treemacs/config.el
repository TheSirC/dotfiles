;; treemacs -- Treemacs relaated configuration
;;
(after! treemacs
        ;; Adding common directories to litter
        (add-to-list 'treemacs-litter-directories "/target")
        (add-to-list 'treemacs-litter-directories "/.cargo")
        (add-to-list 'treemacs-litter-directories "/result")
        (add-to-list 'treemacs-litter-directories "/node_modules")

        ;; We need to customize the faces of treemacs here because
        ;; we change the height of the variable pitch font

        (dolist (face '(treemacs-root-face
                        treemacs-git-unmodified-face
                        treemacs-git-modified-face
                        treemacs-git-renamed-face
                        treemacs-git-ignored-face
                        treemacs-git-untracked-face
                        treemacs-git-added-face
                        treemacs-git-conflict-face
                        treemacs-directory-face
                        treemacs-directory-collapsed-face
                        treemacs-file-face
                        treemacs-tags-face))
        (set-face-attribute face nil :family "Fira Code" :height 115))
  )
