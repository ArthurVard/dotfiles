;;; Web-mode
; с какими файлами ассоциировать web-mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

; настройка отступов
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

; сниппеты и автозакрытие парных скобок
(setq web-mode-extra-snippets '(("erb" . (("name" . ("beg" . "end"))))
                                ))
(setq web-mode-extra-auto-pairs '(("erb" . (("open" "close")))
                                  ))

; подсвечивать текущий элемент
(setq web-mode-enable-current-element-highlight t)
