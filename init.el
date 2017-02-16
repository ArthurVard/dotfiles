;;;; Created       : ...
;;;; Last Modified : 2017 Feb 16 (Thu) 09:12:26 AM by Arthur Vardanyan.

;;; Initialize package system
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defun user-full-name () "Arthur Vardanyan")

;;; === CUSTOM CHECK FUNCTION ===
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
     packages)
)

;;; === List my packages ===
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
 'yasnippet

 'magit
 'git-gutter
 'racket-mode
 'exec-path-from-shell

 'elm-mode
 'elm-yasnippets
 'elixir-mode
 'elixir-yasnippets)


;; Unbind Suspend/Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])
;; Windows Style Undo
;(global-set-key [(control z)] 'undo)

(add-hook 'text-mode-hook 'yas-minor-mode)

(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)  ;; only type `y` instead of `yes`
(set-frame-font "Anonymous Pro-9") ;; Mmm. Delicious fonts.
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)


;;; Themes
;;(custom-enabled-themes (quote (tsdh-dark)))
;(load-theme 'solarized-light t)
(load-theme 'tsdh-dark t)
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
;(load-theme 'solarized t)


;;; Packages to require listed
(require 'yasnippet)
(require 'uniquify)
(require 'ruby-mode)
(require 'web-mode)
(require 'projectile)
(require 'projectile-rails)
(require 'smartparens-config)
(require 'smartparens-ruby)

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
(require 'racket-mode)
(require 'elm-mode)
(require 'elixir-mode)
;(require 'elm-yasnippets)
;(require 'elixir-yasnippets)

;;; Load init files
(mapcar
 (lambda (f) (load-file f))
 (file-expand-wildcards "~/.emacs.d/init/*.el"))


;;; exec shell
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")


;;; yasnippet
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-global-mode t)


(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


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

;;; Ruby Mode
;;; Hooks
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'ruby-mode-hook 'projectile-rails-on)
;(add-hook 'js-mode-hook #'smartparens-mode)

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
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (solarized-theme yasnippet uniquify git-modes racket-mode racket-moe git-gutter magit smex semx web-mode smartparens projectile-rails ido-yes-or-no ido-vertical-mode ido-ubiquitous hindent haskell-mode fill-column-indicator)))
 '(safe-local-variable-values
   (quote
    ((haskell-indent-spaces . 4)
     (haskell-indent-spaces . 2)
     (hindent-style . "chris-done")
     (hindent-style . "gibiansky")
     (hindent-style . "johan-tibell")
     (haskell-process-type . cabal-repl)
     (shm-lambda-indent-style . leftmost-parent)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
