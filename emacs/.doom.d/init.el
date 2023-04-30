;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input

       :completion
       company                                             ; the ultimate code completion backend
       (ivy +prescient)                                    ; a search engine for love and life

       :ui
       deft                                                ; notational velocity for Emacs
       doom                                                ; what makes DOOM look the way it does
       doom-dashboard                                      ; a nifty splash screen for Emacs
       doom-quit                                           ; DOOM quit-message prompts when you quit Emacs
       hl-todo                                             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       modeline                                            ; snazzy, Atom-inspired modeline, plus API
       nav-flash                                           ; blink cursor line after big motions
       ophints                                             ; highlight the region an operation acts on
       (popup +defaults)                                   ; tame sudden yet inevitable temporary windows
       ligatures                                           ; ligatures or substitute text with pretty symbols
       treemacs                                            ; a project drawer, like neotree but cooler
       unicode                                             ; extended unicode support for various languages
       vc-gutter                                           ; vcs diff in the fringe
       vi-tilde-fringe                                     ; fringe tildes to mark beyond EOB
       window-select                                       ; visually switch windows
       workspaces                                          ; tab emulation, persistence & separate workspaces
       zen                                                 ; distraction-free coding or writing

       :editor
       (evil +everywhere)                                  ; come to the dark side, we have cookies
       file-templates                                      ; auto-snippets for empty files
       fold                                                ; (nigh) universal code folding
       (format +onsave)                                    ; automated prettiness
       multiple-cursors                                    ; editing in many places at once
       rotate-text                                         ; cycle region at point between text candidates
       snippets                                            ; my elves. They type so I don't have to

       :emacs
       (dired                                              ; making dired pretty [functional]
        +icons)                                            ; colorful icons for dired-mode

       electric                                            ; smarter, keyword-based electric-indent
       ibuffer                                             ; interactive buffer management
       (undo +tree)                                        ; persistent, smarter undo for your inevitable mistakes
       vc                                                  ; version-control and Emacs, sitting in a tree

       :term
       vterm                                               ; the best terminal emulation in Emacs

       :checkers
       syntax                                              ; tasing you for every semicolon you forget
       spell                                               ; tasing you for misspelling mispelling
       grammar                                             ; tasing grammar mistake every you make

       :tools
       biblio
       ansible
       ;(debugger +lsp)                                     ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig                                        ; let someone else argue about tabs vs spaces
       ein                                                 ; tame Jupyter notebooks with emacs
       (eval +overlay)                                     ; run code, run (also, repls)
       (lookup +dictionnary +offline)                      ; navigate your code and its documentation
       (lsp +peek)
       magit                                               ; a git porcelain for Emacs
       pdf                                                 ; pdf enhancements
       rgb                                                 ; creating color strings
       tree-sitter                                         ; syntax and parsing, sitting in a tree...

       :lang
       assembly                                           ; assembly for fun or debugging
       data                                               ; config/data formats
       (dart +flutter)                                    ; paint ui and not much else
       emacs-lisp                                         ; drown in parentheses
       ess                                                ; emacs speaks statistics
       (javascript +tree-siter)                           ; all(hope(abandon(ye(who(enter(here))))))
       (julia +tree-sitter)                               ; a better, faster MATLAB
       latex                                              ; writing papers in Emacs has never been so fun
       markdown                                           ; writing docs for people to ignore
       (nix +tree-sitter)                                 ; I hereby declare "nix geht mehr!"
       (org +dragndrop +gnuplot +pandoc +present +pretty) ; organize your plain life in plain text
       plantuml                                           ; diagrams for confusing people more
       (python +lsp)                                      ; beautiful is better than ugly
       qt                                                 ; the 'cutest' gui framework ever
       rest                                               ; Emacs as a REST client
       (rust +lsp +tree-sitter)
       sh                                                 ; she sells {ba,z,fi}sh shells on the C xor
       web                                                ; the tubes
       (yaml +tree-sitter)                                ; JSON, but readable

       :perso
       rust
       maxima
       nix
       treemacs
       latex
       theme
       org

       :config
       (default +bindings +smartparens))
