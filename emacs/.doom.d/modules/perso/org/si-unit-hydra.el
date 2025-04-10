(defgroup si-unit nil
  "Customisation for `si-unit-hydra`"
  :tag "si-unit")

(defun si-unit/insert-number ()
  "Prompt the user to enter a number and return that number."
  (interactive)
  (hydra-push '(si-unit/quantity/body))
  (setq si-unit/number (read-number "Enter a number: "))
  (hydra-pop))

(defun si-unit/insert-si-prefix ()
  "Prompt the user to enter a number and return that number."
  (interactive)
  (let ((number (read-number "Enter a number: ")))
    number))

(defvar si-unit/number nil
  "Number for the quantity")
(defvar si-unit/unit ""
  "Unit for the quantity")

(defun si-unit/add-unit (unit)
  "Add a new unit to si-unit/unit."
  (setq si-unit/unit (concat si-unit/unit unit))
  (hydra-pop))

(defun si-unit/format-quantity ()
  (format "\\qty{%s}{%s}" si-unit/number si-unit/unit))

(defun si-unit/quit ()
  (interactive)
  (progn
    (insert (si-unit/format-quantity))
    nil))

(defvar hydra-stack nil)

(defun hydra-push (expr)
  (push `(lambda () ,expr) hydra-stack))

(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x (funcall x))))

(defun si-unit/visit (hydra)
  (progn
    (hydra-push '(si-unit/quantity/body))
    (funcall hydra)))

(defhydra si-unit/length (:hint nil
                          :color teal
                          :post (si-unit/format-quantity))
  "
^Common^                        ^Actions^
^^^^^^^^------------------------------------
_p_m                            _q_: Quit
_n_m
_u_m
_ù_m
_c_m
_d_m
_m_
_k_m
"

  ("p" (si-unit/add-unit "\\pm"))
  ("n" (si-unit/add-unit "\\nm"))
  ("u" (si-unit/add-unit "\\um"))
  ("ù" (si-unit/add-unit "\\mm"))
  ("c" (si-unit/add-unit "\\cm"))
  ("d" (si-unit/add-unit "\\dm"))
  ("m" (si-unit/add-unit "\\m "))
  ("k" (si-unit/add-unit "\\km"))
  ("q" hydra-pop "quit"))

(defhydra si-unit/quantity (:color teal
                            :hint nil)
  "
Common             Composite          Actions
------------------------------------------
_<SPC>_: Number    _/_: Per           _q_: Quit
_m_    : Length    _²_: Squared
                   _3_: Cubed
                   _
"
  ("<SPC>" (si-unit/insert-number))
  ("m"     (si-unit/visit 'si-unit/length/body))
  ("q"      si-unit/quit))


(defun si-unit/insert ()
  (interactive)
  (setq si-unit/number nil
        si-unit/unit "")
  (si-unit/quantity/body))
