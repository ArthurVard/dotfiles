;; https://github.com/chrisdone/emacs-magit-config/blob/master/config/haskell.el


(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
;
(custom-set-variables '(haskell-stylish-on-save t))

;; https://github.com/haskell/haskell-mode/issues/1553

(setq haskell-process-args-ghci
          '("-ferror-spans" "-fshow-loaded-modules"))

(setq haskell-process-args-cabal-repl
          '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

(setq haskell-process-args-stack-ghci
          '("--ghci-options=-ferror-spans -fshow-loaded-modules"
            "--no-build" "--no-load"))

(setq haskell-process-args-cabal-new-repl
          '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map [f7] 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
