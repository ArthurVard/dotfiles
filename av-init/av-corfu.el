;; https://github.com/Gavinok/emacs.d/blob/main/init.el
(use-package corfu
;;  :disabled t
  :ensure t
  ;; Optional customizations 
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.8)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

(provide 'av-corfu)
