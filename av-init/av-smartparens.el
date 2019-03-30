;https://ebzzry.io/en/emacs-pairs/
; https://github.com/Denommus/emacs-config/blob/master/smartparens.el201~

;;; Smartparens

(ensure-package-installed 'smartparens)

(require 'smartparens)

(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">"))

(provide 'av-smartparens)
