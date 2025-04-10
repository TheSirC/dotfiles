;; treemacs -- Treemacs relaated configuration
;;
(after! treemacs
        ;; Adding common directories to litter
        '(dolist (pattern (list "/target" "/.cargo" "/result" "/node_modules"))
           (add-to-list 'treemacs-litter-directories pattern))

        (setq +treemacs-file-ignore-extensions
                        '(;; LaTeX
                        "aux" "ptc" "fdb_latexmk" "fls" "synctex.gz" "toc"
                        ;; LaTeX - bibliography
                        "bbl"
                        ;; LaTeX - glossary
                        "glg" "glo" "gls" "glsdefs" "ist" "acn" "acr" "alg"
                        ;; LaTeX - pgfplots
                        "mw"
                        ;; LaTeX - pdfx
                        "pdfa.xmpi"
                        ;; Python
                        "pyc")

                 +treemacs-file-ignore-globs
                        '(;; LaTeX
                        "*/_minted-*"
                        ;; AucTeX
                        "*/.auctex-auto"
                        "*/_region_.log"
                        "*/_region_.tex"
                        ;; Python
                        "*/__pycache__")
                 doom-themes-treemacs-theme "doom-colors")

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
