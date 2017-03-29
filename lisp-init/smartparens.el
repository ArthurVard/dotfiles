;https://gist.github.com/mordocai/ee13c414712a00c02855

;;; Smartparens
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">"))
