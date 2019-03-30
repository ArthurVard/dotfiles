(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(set-frame-font "Anonymous Pro-12")


(setq ad-redefinition-action 'accept)
(menu-bar-mode -1)
(setq-default truncate-lines t)

(setq line-number-mode t)
(setq column-number-mode t) ;;Show column numbers
(kill-buffer "*scratch*") ;; no scratch buffer
(display-time) ;; show the current time
(setq make-backup-files nil) ;; disable ~ backup files
(global-set-key (kbd "C-c i") 'indent-region) ;; indent highlighted text
(global-linum-mode 1) ;; Add line numbers
(setq linum-format "%d: ") ;; format line numbers with spaces
(fset 'yes-or-no-p 'y-or-n-p) ;; Set yes/no prompts to y/n
(global-set-key (kbd "C-c n") 'comment-region) ;; make the region comments
(global-set-key (kbd "C-c m") 'uncomment-region) ;; uncomment highlighted area

;; Unbind Suspend/Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
;; Windows Style Undo
;(global-set-key [(control z)] 'undo)

(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)  ;; only type `y` instead of `yes`
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)


(provide 'av-global)
