;; Dev config
(use-package markdown-mode)
(use-package yaml-mode)
(use-package web-mode
  :mode "\\.ejs\\'")
(use-package groovy-mode)
(use-package gradle-mode)
(use-package haskell-mode)
(use-package php-mode)
(use-package git-modes)
(use-package rainbow-delimiters
  :hook
  (haskell-mode . rainbow-delimiters-mode)
  (scheme-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))
(use-package yasnippet)
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")
(use-package typescript-mode)

(provide 'av-modes)
