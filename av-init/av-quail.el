;;; Armenian mapping

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)    
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(quail-define-package
 "armenian" "Armenian" "am" t
 "Armenian"
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("A" ?Ա)
 ("a" ?ա)
 ("B" ?Բ)
 ("b" ?բ)
 ("S" ?Ս)
 ("s" ?ս)
 ("D" ?Դ)
 ("d" ?դ)
 ("F" ?Ֆ)
 ("f" ?ֆ)
 ("G" ?Ք)
 ("g" ?ք)
 ("Q" ?Խ)
 ("q" ?խ)
 ("W" ?Ւ)
 ("w" ?ւ)
 ("E" ?Է)
 ("e" ?է)
 ("R" ?Ր)
 ("r" ?ր)
 ("T" ?Տ)
 ("t" ?տ)
 
 
 )

(provide 'av-quail)

