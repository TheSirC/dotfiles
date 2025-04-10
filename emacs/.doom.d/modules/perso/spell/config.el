;;; perso/spell/config.el -*- lexical-binding: t; -*-

;; It's crazy how many messages this produce : SILENCE
(defun suppress-messages (old-fun &rest args)
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply old-fun args)
      (advice-remove 'message #'silence))))

(advice-add 'ispell-init-process :around #'suppress-messages)
(advice-add 'ispell-kill-ispell :around #'suppress-messages)

;; Stolen without shame from https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
;; Other inspirations :
;; - https://github.com/khinshankhan/dotfiles/blob/d40b3ed4deb4d013b6fca3c8dfa83a9118eeae94/emacs/.emacs.d/modules/checkers/spell.el#L7
(after! ispell
  ;; Configure `LANG` if not set, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured in next line.
  (unless (getenv "LANG") (setenv "LANG" "fr_FR.UTF-8"))
  (setq! ispell-program-name "enchant-2") ;; Configure french and two variants of English.
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq! ispell-personal-dictionary "~/.local/.hunspell_personal");; Set the aspell dictionanries
  ;; ispell-dictionary          "~/.config/aspell/fr_FR.pws")

  ;; Ignored patterns
  (let ()
    (--each
        '(("ATTR_LATEX" nil)
          ("AUTHOR" nil)
          ("BLOG" nil)
          ("CREATOR" nil)
          ("DATE" nil)
          ("DESCRIPTION" nil)
          ("EMAIL" nil)
          ("EXCLUDE_TAGS" nil)
          ("HTML_CONTAINER" nil)
          ("HTML_DOCTYPE" nil)
          ("HTML_HEAD" nil)
          ("HTML_HEAD_EXTRA" nil)
          ("HTML_LINK_HOME" nil)
          ("HTML_LINK_UP" nil)
          ("HTML_MATHJAX" nil)
          ("INFOJS_OPT" nil)
          ("KEYWORDS" nil)
          ("LANGUAGE" nil)
          ("LATEX_CLASS" nil)
          ("LATEX_CLASS_OPTIONS" nil)
          ("LATEX_HEADER" nil)
          ("LATEX_HEADER_EXTRA" nil)
          ("NAME" t)
          ("OPTIONS" t)
          ("POSTID" nil)
          ("RESULTS" t)
          ("SETUPFILE" nil)
          ("SELECT_TAGS" nil)
          ("STARTUP" nil)
          ("TITLE" nil))
      (add-to-list
       'ispell-skip-region-alist
       (let ((special (concat "#[+]" (car it) ":")))
         (if (cadr it)
             (cons special "$")
           (list special))))))
  ;; Some org elements
  (pushnew! ispell-skip-region-alist
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")
            '("cite:" . "[[:space:]]")
            '("label:" . "[[:space:]]")
            '("ref:" . "[[:space:]]")
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("=" "=")
            '("[^\000-\377]+")
            '("```" "```")
            '("~" "~")
            '("~" . "~")
            '("=" . "=")
            '("^#\\+ATTR_.+$")
            '("cite:[a-zA-Z_-]+")
            '(org-link-any-re)
            '(org-property-drawer-re)
            '(org-latex-math-environments-re))
  ;; Latex Math
  (pushnew! ispell-skip-region-alist
            '("\\$" . "\\$")
            '("\\\\(" . "\\\\)")
            '("\\\[" . "\\\]")
            '("\\\\begin{\\(?:align\\(?:at\\)?\\|d\\(?:array\\|group\\|isplaymath\\|math\\|series\\)\\|e\\(?:mpheq\\|q\\(?:narray\\|uation\\)\\)\\|flalign\\|gather\\|m\\(?:ath\\|ultline\\)\\|subequations\\|x\\(?:x?alignat\\)\\)\\*?}"
              . "\\\\end{\\(?:align\\(?:at\\)?\\|d\\(?:array\\|group\\|isplaymath\\|math\\|series\\)\\|e\\(?:mpheq\\|q\\(?:narray\\|uation\\)\\)\\|flalign\\|gather\\|m\\(?:ath\\|ultline\\)\\|subequations\\|x\\(?:x?alignat\\)\\)\\*?}")
            '("\\\\begin\{align\*\}" . "\\\\end\{align\*\}")
            '("\\\\begin\{equation\}" . "\\\\end\{equation\}")
            '("\\\\begin\{equation\*\}" . "\\\\end\{equation\*\}")
            '("\\\\begin\{eqnarray\*\}" . "\\\\end\{eqnarray\*\}")
            )
  ;; Latex references
  (pushnew! ispell-skip-region-alist
            '("\\\\ref\{" . "\}")
            '("\\\\cref\{" . "\}")
            '("\\\\[a-z]?cite\{". "\}")
            '("\\\\eqref\{" . "\}")
            '("\\\\label\{" . "\}")
            '("\\\\printbibliography\\[" . "\\]")
            )

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))

  (unless (file-exists-p ispell-dictionary)
    (write-region "" nil ispell-dictionary nil 0)))


(after! flyspell
  (setq flyspell-lazy-idle-seconds 5)

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))

  (unless (file-exists-p ispell-dictionary)
    (write-region "" nil ispell-dictionary nil 0))

  (defun +latex-flyspell-word-p ()
    "Return t if point is on a word that should be spell checked.
    Return nil if the word is in a context that should not be spell checked."
    ;; Retrieve the faces at the current cursor position
    (let* ((faces (doom-enlist (get-text-property (point) 'face)))
           ;; List of faces and patterns that indicate contexts where spell checking should be ignored
           (unsafe-faces '(font-lock-variable-name-face
                           font-lock-keyword-face
                           font-lock-constant-face
                           font-lock-comment-face
                           font-latex-math-face
                           font-latex-verbatim-face
                           font-latex-warning-face
                           font-latex-sedate-face)))

      ;; Check if the current word has any face that matches the unsafe-faces list
      (not (cl-loop for face in faces
                    if (memq face unsafe-faces)
                    return t))))
  (after! LaTeX (set-flyspell-predicate! '(LaTeX-mode) #'+latex-flyspell-word-p))

  (defun +md-flyspell-word-p ()
    "Return t if point is on a word that should be spell checked.
    Return nil if the word is in a context that should not be spell checked."
    ;; Retrieve the faces at the current cursor position
    (let* ((faces (doom-enlist (get-text-property (point) 'face)))
           ;; List of faces and patterns that indicate contexts where spell checking should be ignored
           (unsafe-faces '(markdown-reference-face
                           markdown-url-face
                           markdown-markup-face
                           markdown-comment-face
                           markdown-html-attr-name-face
                           markdown-html-attr-value-face
                           markdown-html-tag-name-face
                           markdown-code-face)))

      ;; Check if the current word has any face that matches the unsafe-faces list
      (not (cl-loop for face in faces
                    if (memq face unsafe-faces)
                    return t))))
  (after! markdown (set-flyspell-predicate! '(markdown-mode) #'+md-flyspell-word-p))

  (defun +org-flyspell-word-p ()
    "Return t if point is on a word that should be spell checked.
    Return nil if the word is in a context that should not be spell checked."

    ;; Retrieve the faces at the current cursor position
    (let* ((faces (doom-enlist (get-text-property (point) 'face)))
           ;; List of faces and patterns that indicate contexts where spell checking should be ignored
           (unsafe-faces '("\\`[ 	]*\\\\begin{\\(?:align*\\|equation*\\|eqnarray*\\)\\*?}"
                           font-lock-comment-delimiter-face
                           font-lock-comment-face
                           font-lock-constant-face
                           font-lock-function-name-face
                           font-lock-keyword-face
                           font-lock-string-face
                           org-block
                           org-block-begin-line
                           org-block-end-line
                           org-code
                           org-column
                           org-date
                           org-document-info
                           org-document-info-keyword
                           org-formula
                           org-latex-and-related
                           org-latex-math-environments-re
                           org-link
                           org-list-dt
                           org-meta-line
                           org-modern-tag
                           org-property-drawer-re
                           org-property-value
                           org-ref-cite-face
                           org-ref-cite-re
                           org-ref-label-re
                           org-ref-ref-re
                           org-roam-olp
                           org-special-keyword
                           org-table
                           org-tag
                           org-todo
                           org-todo-keyword-done
                           org-todo-keyword-habt
                           org-todo-keyword-kill
                           org-todo-keyword-outd
                           org-todo-keyword-todo
                           org-todo-keyword-wait
                           org-verbatim
                           org-verbatim)))

      ;; Check if the current word is within any Org mode heading
      (or (cl-loop for face in faces
                   thereis (string-match-p "^org-level-[0-9]+$" (symbol-name face)))
          ;; Check if the current word has any face that matches the unsafe-faces list
          (not (cl-loop for face in faces
                        if (memq face unsafe-faces)
                        return t)))))
  (after! org (set-flyspell-predicate! '(org-mode) #'+org-flyspell-word-p)))

;; (add-hook 'emacs-startup-hook #'global-jinx-mode)
