;;; evil-replace-with-register.el --- Port of vim plugin ReplaceWithRegister

;; Copyright (C) 2014 by Dewdrops

;; Author: Dewdrops <v_v_4474@126.com>
;; URL: https://github.com/Dewdrops/evil-ReplaceWithRegister
;; Version: 0.1
;; Keywords: evil, plugin
;; Package-Requires: ((evil "1.0.8"))

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Port of vim plugin ReplaceWithRegister
;; (http://www.vim.org/scripts/script.php?script_id=2703)
;;
;; Installation:
;;
;; put evil-replace-with-register.el somewhere in your load-path and add these
;; lines to your .emacs:
;; (require 'evil-replace-with-register)
;; ;; change default key bindings (if you want) HERE
;; ;; (setq evil-replace-with-register-key (kbd "gr"))
;; (evil-replace-with-register-install)

;;; Code:

(require 'evil)

(defgroup evil-replace-with-register nil
  "Replacing an existing text with the contents of a register"
  :prefix "evil-replace-with-register"
  :group 'evil)

(defcustom evil-replace-with-register-key (kbd "gR")
  "Default binding for evil-replace-with-register."
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-replace-with-register)

(defcustom evil-replace-with-register-indent nil
  "If non-nil, the newly added text will be indented."
  :group 'evil-replace-with-register
  :type  'boolean)

;;;###autoload
(autoload 'evil-replace-with-register "evil-replace-with-register"
  "Replacing an existing text with the contents of a register" t)

(evil-define-operator evil-replace-with-register (count beg end type register)
  "Replacing an existing text with the contents of a register"
  :move-point nil
  (interactive "<vc><R><x>")
  ;; TODO: It would be possible to use `evil-paste-pop' after `evil-replace-with-register' if the latter was called without a register.
  ;; BUG: Previous command was not an evil-paste: evil-use-register
  ;; BUG: Previous command was not an evil-paste: evil-paste
  ;; Ensure 'count' is set, defaulting to 1 if not provided.
  (setq count (or count 1))

  ;; Determine the text to paste:
  (let ((text (if register
                  ;; If a register is provided, get its content;
                  (evil-get-register register)
                  ;; Otherwise, use the latest entry from the kill ring.
                  (current-kill 0))))

  ;; Move the cursor to the starting position 'beg'.
  (goto-char beg)

  ;; Check if the operation is to be applied on a block.
  (if (eq type 'block)

          ;; Apply the function on each block line.
          (evil-apply-on-block
                  (lambda (begcol endcol)

                          ;; Calculate the maximum column on the current line.
                          (let ((maxcol (evil-column (line-end-position))))

                                  ;; Ensure the beginning column is less than the maximum column.
                                  (when (< begcol maxcol)

                                          ;; Adjust the end column to the minimum between its value or maxcol.
                                          (setq endcol (min endcol maxcol))

                                          ;; Move to the calculated beginning and end columns.
                                          (let ((beg (evil-move-to-column begcol nil t))
                                                (end (evil-move-to-column endcol nil t)))

                                          ;; Delete the region between beg and end.
                                          (delete-region beg end)

                                          ;; Insert the text 'count' times using 'evil-paste-after'.
                                          (dotimes (_ count) (evil-paste-after text 'yank)))))
                  beg end t)

          ;; For non-block type, delete the region between 'beg' and 'end'.
          (delete-region beg end)

          ;; Insert the text 'count' times using 'evil-paste-after'.
          (dotimes (_ count) (evil-paste-after text 'yank))

          ;; If indenting is enabled and the insertion spans multiple lines,
          ;; re-indent the inserted region.
          (when (and evil-replace-with-register-indent (/= (line-number-at-pos beg) (line-number-at-pos)))
                  ;; Save the excursion to restore the cursor position after indenting.
                  (save-excursion
                          (evil-indent beg (point))))))

  (when (evil-get-register register) (evil-paste-pop 0))))

;;;###autoload
(defun evil-replace-with-register-install ()
  "Setting evil-replace-with-register key bindings."
  (define-key evil-normal-state-map
    evil-replace-with-register-key 'evil-replace-with-register)
  (define-key evil-visual-state-map
    evil-replace-with-register-key 'evil-replace-with-register))

(provide 'evil-replace-with-register)
;;; evil-replace-with-register.el ends here
