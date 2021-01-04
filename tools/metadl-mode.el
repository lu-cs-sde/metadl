(setq metadl-mode-syntax-table
  (let ((table (make-syntax-table)))
     (modify-syntax-entry ?# "<" table)
     (modify-syntax-entry ?\n ">" table)
     (modify-syntax-entry ?< "(" table)
     (modify-syntax-entry ?> ")" table)
    table))

(set-syntax-table metadl-mode-syntax-table)

(setq metadl-highlights
      '(("EDB\\|OUTPUT\\|IMPORT\\|ID\\|DECL\\|TYPE\\|GENERIC\\|java\\|metadl" . font-lock-builtin-face)
	(":-\\|\\.\\|'\\|<:\\|:>\\|NOT" . font-lock-keyword-face)
	("`\\([[:alpha:]]\\|_\\)[[:alnum:]]*" . font-lock-variable-name-face)))


(define-derived-mode metadl-mode fundamental-mode "MetaDL"
  "major mode for editing MetaDL datalog files."
  :syntax-table metadl-mode-syntax-table
  (setq font-lock-defaults '(metadl-highlights))
  (setq-local comment-start "#")
  (setq-local comment-end "\n")
)

(provide 'metadl-mode)
