(map!
 :prefix "g"
 :desc "Cycle through case" :v "C"   #'string-inflection-all-cycle
 :desc "Sort lines"         :v "TAB" #'sort-lines
 :desc "Jump last position" :n "("   #'evil-jump-backward
 :desc "Jump next position" :n ")"   #'evil-jump-forward)

(map!
 :leader
 :prefix "c"
 :desc "Toggle comment line"
 :nv ";" #'evilnc-comment-or-uncomment-lines)

(map!
 :prefix "g"
 :desc "Replace with register"
 :nv "SPC" #'evil-replace-with-register)

(map!
 :leader
 :prefix "w"
 :desc "Quick-select windows"
 :n "TAB" #'ace-select-window)

;; TODO: Command for inserting references in project (see consult ref.)

;; TODO: Hydra for SIUnitx
;; (defun ask-for-value-and-create-hydra ()
;;   "Prompt the user for a value and create a hydra based on that value."
;;   (interactive)
;;   (let ((value (read-string "Enter the quantity's value: ")))
;;     (eval `(defhydra my-hydra (:hint nil)
;;              "
;; Value: %s(value)

;; ^Options^
;; ^^^^^^^^--------------------------
;; _a_: Option A
;; _b_: Option B
;; _q_: Quit
;; "
;;              ("a" (message "Option A chosen with value: %s" ,value))
;;              ("b" (message "Option B chosen with value: %s" ,value))
;;              ("q" nil "Quit"))))
;;   (my-hydra/body))


;; { Quick resizing windows
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
;;}

(map!
 :leader
 :prefix "t"
 :desc "Toogle tabs"
 :n "t" #'centaur-tabs-mode)

(map!
 :leader
 :prefix "p"
 :desc "Direnv allow"
 :n "A" #'envrc-allow)

(map!
 :leader
 :desc "Select last window"
 :n "²" #'other-window)

(map!
 :prefix "g z"
 :desc "Iedit → MC"
 :n "M" #'iedit-switch-to-mc-mode)

(map!
 :leader
 :prefix "s"
 :desc "Search TODO in current file"
 :n "c" #'hl-todo-occur)

(map!
 :leader
 :prefix "TAB"
 :desc "Next workspace"     :n "j"   #'+workspace/switch-right
 :desc "Previous workspace" :n "k"   #'+workspace/switch-left
 :desc "Select workspace"   :n "SPC" #'+workspace/switch-to)

(setq expand-region-contract-fast-key ">")         ;; Expand region key to contract
(map! :desc "Expand region"
      :v "g <"
      'er/expand-region)
