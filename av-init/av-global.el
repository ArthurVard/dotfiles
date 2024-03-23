

;;(cua-mode)


;;(global-linum-mode 1) ;; Add line numbers
(setq linum-format "%d: ") ;; format line numbers with spaces
(fset 'yes-or-no-p 'y-or-n-p) ;; Set yes/no prompts to y/n
(global-set-key (kbd "C-c n") 'comment-region) ;; make the region comments
(global-set-key (kbd "C-c m") 'uncomment-region) ;; uncomment highlighted area


(setq visible-bell t) ; switch off annoying bell sound, instead bell is visible

;; (set-face-attribute 'default nil
;; 		    :family "Source Code Pro"
;; 		    :foundry "ADBO"
;; 		    :slant 'normal
;; 		    :weight 'normal
;; 		    :height 140
;; 		    :width 'normal)

(column-number-mode)
(global-display-line-numbers-mode t)
(fringe-mode 10)

(size-indication-mode t)
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(menu-bar-mode -1)   ; Disable the menu bar


(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(provide 'av-global)
