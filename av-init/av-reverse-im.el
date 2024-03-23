;; Use keybindings when Armenian layout is on
(use-package reverse-im
  :custom
  (reverse-im-input-methods '("armenian-computer")) ;; figure out how add Armenian mapping
  :config
  (reverse-im-mode t))

(provide 'av-reverse-im)
