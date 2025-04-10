;; -*- no-byte-compile: t; -*-
;;; perso/spell/keymaps.el

(map! :leader :prefix "S"
 :desc "Next spelling error"    :n "j" #'evil-next-flyspell-error
 :desc "Prev spelling error"    :n "k" #'evil-prev-flyspell-error
 :desc "Correct spelling error" :n "c" #'flyspell-correct-word
 )
