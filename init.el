;;; Initialize package system
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; === CUSTOM CHECK FUNCTION ===
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
     packages)
)

;; === List my packages ===
;; add package names to the list, on start will install missing pacakges in new machine
(ensure-package-installed
 'web-mode
 'projectile
 'ido
 'ido-yes-or-no
 'ido-ubiquitous
 'ido-vertical-mode
 'smartparens
 'fill-column-indicator
 'haskell-mode
 'projectile-rails
; 'haskell-indentation
 'hindent
 'smex
 'magit
 'git-gutter)

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
;; Windows Style Undo
;(global-set-key [(control z)] 'undo)

(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)  ;; only type `y` instead of `yes`
(set-frame-font "Anonymous Pro-9") ;; Mmm. Delicious fonts.
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)


;;; Themes
;;(custom-enabled-themes (quote (tsdh-dark)))
;(load-theme 'solarized-light t)
(load-theme 'tsdh-dark t)


;;; Packages to require
(require 'ruby-mode)
(require 'web-mode)
(require 'projectile)
(require 'projectile-rails)
(require 'smartparens-config)
(require 'ido) ; buffer switching ido mode
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(require 'ido-yes-or-no)
(require 'haskell-indentation)
(require 'fill-column-indicator)
(require 'hindent)
(require 'smex)
(require 'magit)
(require 'git-gutter)

;; Load init files
(mapcar
 (lambda (f) (load-file f))
 (file-expand-wildcards "~/.emacs.d/init/*.el"))



(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)


(setq ad-redefinition-action 'accept)
(menu-bar-mode -1)
(setq-default truncate-lines t)

(setq line-number-mode t)
(setq column-number-mode t)
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

;;; Ruby Mode
;;; Hooks
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'ruby-mode-hook 'projectile-rails-on)
;(add-hook 'js-mode-hook #'smartparens-mode)

;;; Web-mode
; с какими файлами ассоциировать web-mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

; настройка отступов
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

; сниппеты и автозакрытие парных скобок
(setq web-mode-extra-snippets '(("erb" . (("name" . ("beg" . "end"))))
                                ))
(setq web-mode-extra-auto-pairs '(("erb" . (("open" "close")))
                                  ))

; подсвечивать текущий элемент
(setq web-mode-enable-current-element-highlight t)


;;; Smartparens
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  )

;;; == ido ==
;; make buffer switch command auto suggestions, also for find-file command
(ido-mode 1)
(ido-yes-or-no-mode 1)
(setq
 ido-create-new-buffer    'always
 ido-enable-flex-matching t ;; display any item that contains the chars you typed
 ido-everywhere           t)

;(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
			   "*Messages*" "Async Shell Command"))

(ido-ubiquitous-mode 1) ; use IDO everywhere

;to use up and down to navigate the options, or:
;to use left and right to move through the history/directories.
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-vertical-mode 1) ; display IDO vertically



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (git-gutter magit smex semx web-mode smartparens projectile-rails ido-yes-or-no ido-vertical-mode ido-ubiquitous hindent haskell-mode fill-column-indicator))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
