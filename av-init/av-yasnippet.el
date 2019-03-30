(ensure-package-installed
 'yasnippet
)

(require 'yasnippet)

(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-global-mode t)
(add-hook 'text-mode-hook 'yas-minor-mode)


(provide 'av-yasnippet)
