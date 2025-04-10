;;; perso/rlang/config.el -*- lexical-binding: t; -*-

(after! ess-r-mode
  (appendq! +ligatures-extra-symbols
            '(:assign   "⟵"
              :multiply "×"))
  (set-ligatures! 'ess-r-mode
    ;; Functional
    :def      "function"
    ;; Types
    :null     "NULL"
    :true     "TRUE"
    :false    "FALSE"
    :int      "int"
    :floar    "float"
    :bool     "bool"
    ;; Flow
    :not      "!"
    :and      "&&"
    :or       "||"
    :for      "for"
    :in       "%in%"
    :return   "return"
    :assign   "<-"
    :multiply "%*%"))
