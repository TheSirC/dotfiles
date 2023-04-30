;; theme -- Doom theme related configuration
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t  ; if nil, italics is universally disabled
      doom-font (font-spec :family "Fira Code" :size 12 :weight 'regular))

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
