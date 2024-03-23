;;; https://pastebin.com/Gtx9a1N0

;; Flex for fuzzy matching
(use-package flx-ido    ; Flx matching for ido (includes flx)
  :ensure t)
 

(use-package ivy :demand
      :config
      (setq ivy-use-virtual-buffers t
            ivy-count-format "%d/%d "))
 
;; provide the configuration
(provide 'av-ivy)
