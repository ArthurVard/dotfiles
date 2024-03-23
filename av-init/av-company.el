;; Use company-capf as a completion provider.
(use-package company
  :hook
  (scala-mode . company-mode)
  (js2-mode . company-mode)
  :config
  (defvar lsp-completion-provider :capf))


(provide 'av-company)
