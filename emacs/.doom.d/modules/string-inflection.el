(use-package! string-inflection
  :commands
  (my-hydra-string-inflection/body)
  :init
  (progn
    (defhydra my-hydra-string-inflection
      (:hint nil)
      "
[_i_] cycle"
      ("i" string-inflection-all-cycle))
    (map!
     :leader
     (:prefix ("z" . "Inflection")
      "c" 'string-inflection-lower-camelcase
      "C" 'string-inflection-camelcase
      :desc "String Inflection Hydra" "i" 'my-hydra-string-inflection/body
      "-" 'string-inflection-kebab-case
      "k" 'string-inflection-kebab-case
      "_" 'string-inflection-underscore
      "u" 'string-inflection-underscore
      "U" 'string-inflection-upcase))))
