;; theme -- Doom theme related configuration
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t  ; if nil, italics is universally disabled
      doom-font                (font-spec :family "Fira Code"   :size 16 :weight 'regular)
      doom-big-font            (font-spec :family "Fira Code"   :size 24)
      doom-variable-pitch-font (font-spec :family "Inria Sans"  :size 26)
      doom-symbol-font         (font-spec :family "Noto Sans Symbols")
      doom-serif-font          (font-spec :family "Inria Serif" :size 22 :weight 'light))

(setq display-line-numbers      'visual            ;; Relative line number for faster Vim movement
      display-line-numbers-type 'relative)

;; Do different themes if in terminal than GUI
(if (display-graphic-p)
    ;; If in GUI
    (setq doom-theme 'doom-solarized-dark)
    ;; If in terminal
    (setq doom-theme 'doom-challenger-deep
      ;; Change the color of comment, they are barely readable
      ;; when they should be the thing you see, not hide !
      doom-challenger-deep-brighter-comments t
      doom-challenger-deep-brighter-modeline t))
