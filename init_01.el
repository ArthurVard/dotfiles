;;; init.el --- Initialization file for Emacs.
;;; Commentary:
;; bebyx initialization file for Emacs

;;;; https://bebyx.co.ua/en/log/emacs-haskell-lsp.html
;;;; https://github.com/bebyx/dotfiles/blob/master/.emacs.d/init.el
;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.config/emacs/av-init/")
(add-to-list 'load-path "~/.config/emacs/av-init/themes")

;; Glue GHCup path to Haskell LSP
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.ghcup/bin")))
(setq exec-path (append exec-path '(expand-file-name "~/.ghcup/bin")))

(cua-mode)

(setq visible-bell t) ; switch off annoying bell sound, instead bell is visible

(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :foundry "ADBO"
		    :slant 'normal
		    :weight 'normal
		    :height 140
		    :width 'normal)

(column-number-mode)
(global-display-line-numbers-mode t)
(fringe-mode 10)

(size-indication-mode t)
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(menu-bar-mode -1)   ; Disable the menu bar

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Initialize package sources

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))



(package-initialize) ; activate all the packages

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00"))

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;;(require 'av-complition)
;;(require 'av-ido)
;;(require 'av-selectrum)
;;(require 'av-ivy)
;;(require 'av-smex)
;;(require 'av-helm)
;;(require 'av-corfu)

;;(require 'av-vertico)

(require 'av-quail)
(require 'av-keycast)
(require 'av-lsp)
(require 'av-company)
(require 'av-projectile)

;; themes: 'leuven | 'zenburn | monokai | blackboard
(require 'monokai)

;(use-package vertico)
;(vertico-mode 1)

;;(require 'av-reverse-im)
;;(require 'av-misc)


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




(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-icon t))

;; Beautiful dark theme
;(use-package solarized-theme
;  :init
;  (setq solarized-distinct-fringe-background t)
;  (load-theme 'solarized-dark-high-contrast))



(use-package which-key
  :init
  (which-key-mode)
  (setq which-key-idle-delay 1))

(provide 'init)
;;; init.el ends here
