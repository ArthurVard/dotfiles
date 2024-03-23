(use-package lsp-mode
  :bind-keymap
  ("C-c l" . lsp-command-map)
  ;; Optional - enable lsp-mode automatically in scala files
  :hook
  (scala-mode . lsp)
  ((js2-mode typescript-mode) . lsp)
  (lsp-mode . lsp-lens-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  ;; Tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (defvar lsp-completion-provider :capf)
  (defvar lsp-prefer-flymake nil)
  ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
  (setq lsp-keep-workspace-alive nil)

  ;; JS config
  (defvar lsp-clients-typescript-server 'node)
  (defvar lsp-typescript-server-args '("--stdio" "--tsserver-logFile" "/tmp/tsserver.log"))
  (defvar lsp-eslint-enable t)
  (defvar lsp-eslint-auto-fix-on-save t))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-hover t))

(use-package lsp-metals)
(use-package lsp-java
  :hook
  (java-mode . lsp))
(use-package lsp-haskell
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp))
(use-package helm-lsp)
(use-package lsp-treemacs)

(use-package treemacs
  :bind ("C-t" . treemacs)
  :config
  (setq treemacs-width 30))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))


(provide 'av-lsp)
