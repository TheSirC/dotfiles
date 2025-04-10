;; (after! PACKAGE
;;  (set-ligatures! 'MAJOR-MODE
;;    :symbol "keyword"))
(plist-put! +ligatures-extra-symbols
  ;; Org
  :quote         "“"
  :quote_end     "”"
  ;; Functional
  :lambda        "λ"
  :def           "ƒ"
  :composition   "∘"
  :map           "↦"
  ;; Types
  :null          "∅"
  :true          "𝕋"
  :false         "𝔽"
  :int           "ℤ"
  :float         "ℝ"
  :str           "𝕊"
  :bool          "𝔹"
  :list          "𝕃"
  ;; Flow
  :not           "￢"
  :in            "∈"
  :not-in        "∉"
  :and           "∧"
  :or            "∨"
  :for           "∀"
  :some          "∃"
  :return        "⟼"
  :yield         "⟻"
  ;; Equality
  :equal         "≡"
  :not-equal     "≠"
  :is            "≣"
  :isnt          "≢"
  :lte           "≤"
  :gte           "≥"
  ;; Sets
  :subseteq      "⊆"
  :union         "⋃"
  :intersect     "∩"
  :diff          "∖"
  :tuple         "⨂"
  :pipe          "⥤"
  :dot           "•")  ;; you could also add your own if you want
