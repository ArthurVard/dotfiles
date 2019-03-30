(ensure-package-installed
 'fill-column-indicator
)

(require 'fill-column-indicator)

(setq-default fill-column 80)
(setq fci-rule-width 1)
(setq fci-rule-color "blue")
(add-hook 'emacs-lisp-mode-hook (lambda ()
    (fci-mode 1)
  ))
(setq fci-mode t)

(provide 'av-fci)
