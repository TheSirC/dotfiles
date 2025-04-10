;;; evil/config.el -*- lexical-binding: t; -*-

;; TODO: Define `evil-respect-visual-line-mode` to t : https://stackoverflow.com/a/76050702 and https://evil.readthedocs.io/en/latest/settings.html#elispobj-evil-respect-visual-line-mode

(after! which-key
  ;; Symbolic replacement for too long repeating sequences of functions in which-key
  (pushnew! which-key-replacement-alist
   '(("" . "\\`zt\\^")                                 . (nil . "Scroll line to top"))
   '(("" . "\\`+?lookup/\\(.*\\)")                                 . (nil . "⇧\\1"))
   '(("" . "\\`+?lookup/\\(.*\\)")                                 . (nil . "⇧\\1"))
   '(("" . "\\`+?vimish-fold\\(.*\\)")                             . (nil . "⒡/\\1"))
   '(("" . "\\`+?evil[-:]?vimish-\\(.*\\)")                        . (nil . "⒱\\1"))
   '(("" . "\\`+?evil-tex-\\(.*\\)")                               . (nil . "⒯\\1"))
   '(("" . "\\`+?vimish-\\(.*\\)")                                 . (nil . "⒱\\1"))
   '(("" . "\\`+?fold/\\(.*\\)")                                   . (nil . "⒡/\\1"))
   '(("" . "\\`+?i?spell-?/?\\(.*\\)")                             . (nil . "⒮/\\1"))
   '(("" . "\\`+?evil[-:]mc-\\(.*\\)")                             . (nil . "⥌\\1"))
   '(("" . "\\`+?multiple-cursors/evil-mc-\\(.*\\)")               . (nil . "⥌\\1"))
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)")                    . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)")                    . (nil . "◃\\1"))
   '(("" . "\\`+?function[-:]?evil[-:]?\\(?:a-\\)?\\(.*\\)")       . (nil . "▿\\1"))
   '(("" . "\\`+?evil[-:]?textobj-tree-sitter-function--\\(.*\\)") . (nil . "˕\\1"))
   ))
(general-define-key
 :states '(normal visual insert emacs)
 :keymaps 'override
 :prefix "g z"
 :non-normal-prefix "C-SPC" "" '(:ignore t :which-key "evil-multi-cursor"))

(general-define-key
 :states '(normal visual insert emacs)
 :keymaps 'override
 :prefix "g s"
 :non-normal-prefix "C-SPC" "" '(:ignore t :which-key "evil-search"))

;; TODO: Define operator surround math (g S m) and display math (g s M) for org mode

(map!
 :prefix "g z"
 :desc "evil-mc-insert numbers" :n "n" #'mc/insert-numbers
 :desc "evil-mc-insert letters" :n "l" #'mc/insert-letters)

;; I have no idea why this is not t by default, allow to see the available surrounding objects as which-key results
(setq which-key-show-operator-state-maps t)

(use-package-hook! evil
  :pre-init
  (setq evil-respect-visual-line-mode t))           ;; Sane j and k behavior

(after! evil-snipe
      (setq evil-snipe-scope           'whole-line ;; Setting up evil-snipe to be more efficient
            evil-snipe-spillover-scope 'whole-visible))

;; Switch to the new window after splitting
(setq evil-split-window-below   t
      evil-vsplit-window-right  t)

;; Prompt for buffer after split
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (switch-to-buffer))

;; Shortcuts for doom configuration files
(defvar doom-root
  (concat (getenv "OUROBOROS")
   "/dotfiles/emacs/.doom.d/"))

(defun find-doom-config ()
  (find-file (concat doom-root "config.org")))

(defun find-doom-init ()
  (find-file (concat doom-root "init.el")))

(defun find-doom-packages ()
  (find-file (concat doom-root "packages.el")))

(after! evil
  (evil-ex-define-cmd "cfg"       'find-config)
  (evil-ex-define-cmd "conf[ig]"  'find-config)
  (evil-ex-define-cmd "pkg"       'find-doom-packages)
  (evil-ex-define-cmd "pack[age]" 'find-doom-packages)
  (evil-ex-define-cmd "ini[t]"    'find-doom-init))
