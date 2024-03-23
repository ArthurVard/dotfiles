
(use-package fill-column-indicator)

  (setq-default fill-column 80)
  (setq fci-rule-width 1)
  (setq fci-rule-color "blue")
  (setq fci-mode t)

  (add-hook 'emacs-lisp-mode-hook (lambda ()
      (fci-mode 1)
      ))

  

(provide 'av-fci)
