(map!
 :prefix "g"
 :desc "Cycle through case"
 :v "C" #'string-inflection-all-cycle)

(map!
 :prefix "g"
 :desc "Sort lines"
 :v "TAB" #'sort-lines)

(map!
 :leader
 :prefix "c"
 :desc "Toggle comment line"
 :nv ";" #'evilnc-comment-or-uncomment-lines)

(map!
 :leader
 :prefix "w"
 :desc "Quick-select windows"
 :n "TAB" #'ace-select-window)

;; Quick resizing windows
(defhydra doom-window-resize-hydra (:hint nil)
  "
              _k_ + height
  _h_ - width    _l_ + width
              _j_ - height
"
  ("h" evil-window-decrease-width)
  ("j" evil-window-increase-height)
  ("k" evil-window-decrease-height)
  ("l" evil-window-increase-width)

  ("q" nil))

(map!
 :leader
 :desc "Hydra resize"
 :prefix "w"
 :n "SPC" #'doom-window-resize-hydra/body)

(map!
 :leader
 :desc "Select last window"
 :n "²" #'other-window)

(map!
 :leader
 :prefix "TAB"
 :n "j" #'+workspace/switch-right
 :n "k" #'+workspace/switch-left
 :n "SPACE" #'+workspace/switch-to)

(setq expand-region-contract-fast-key ">")         ;; Expand region key to contract
(map! :desc "Expand region"
      :v "g <"
      'er/expand-region)
