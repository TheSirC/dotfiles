;;; perso/python/config.el -*- lexical-binding: t; -*-

(after! python
  (set-ligatures! 'python-mode :merge t
    :equal     "=="
    :not-equal "!="
    :lte       "<="
    :gte       ">="
    :is        "is"
    :isnt      "is not"
    :subseteq  "issubset"
    ;; Functional
    :def       "def"
    :lambda    "lambda"
    ;; Types
    :null      "None"
    :true      "True"
    :false     "False"
    :int       "int"
    :str       "str"
    :float     "float"
    :bool      "bool"
    :tuple     "tuple"
    ;; Flow
    :not       "not"
    :in        "in"
    :not-in    "not in"
    :and       "and"
    :or        "or"
    :for       "for"
    :return    "return"
    :yield     "yield"))
