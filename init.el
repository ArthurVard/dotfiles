;;; init.el --- Initialization file for Emacs.
;;; Commentary:
;; bebyx initialization file for Emacs  

;;;; https://bebyx.co.ua/en/log/emacs-haskell-lsp.html
;;;; https://github.com/bebyx/dotfiles/blob/master/.emacs.d/init.el

;;; Code:

;(defvar emacs-config-dir "~/.emacs.d/")
(defvar emacs-config-dir "~/dotfiles")


(setq custom-file (concat emacs-config-dir "/custom.el"))
(load custom-file 'noerror)

(add-to-list 'load-path (concat emacs-config-dir "/lisp/"))1
(add-to-list 'load-path (concat emacs-config-dir "/av-init/"))
(add-to-list 'load-path (concat emacs-config-dir "/av-init/themes"))

;; Glue GHCup path to Haskell LSP
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.ghcup/bin")))
(setq exec-path (append exec-path '(expand-file-name "~/.ghcup/bin")))

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

;; skip for windows
;;(use-package exec-path-from-shell)
;;(exec-path-from-shell-initialize)

(use-package better-shell
    :ensure t
    :bind (("C-\"" . better-shell-shell)
           ("C-:" . better-shell-remote-open)))

(require 'av-global)
(require 'av-modes)

;;(require 'av-complition)
;;(require 'av-ido)
;;(require 'av-selectrum)
;;(require 'av-ivy)
;;(require 'av-smex)
;;(require 'av-helm)
;;(require 'av-corfu)
;;(require 'av-reverse-im)
;;(require 'av-misc)

(require 'av-fci)
(require 'av-git)
(require 'av-vertico)

(require 'av-lsp)
(require 'av-quail)
;(require 'av-keycast)
(require 'av-lsp)
(require 'av-company)
(require 'av-projectile)
(require 'av-pdfreading)
(require 'av-dashboard)
;; org-mode config done using org-mode
(org-babel-load-file  (concat emacs-config-dir "/av-init/av-org-config.org"))

(setq org-file-apps
    (quote
        ((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . "/run/current-system/sw/bin/firefox %s")
        ("\\.pdf\\'" . default))))

;; themes: 'leuven | 'zenburn | monokai | blackboard
(require 'monokai)

;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   (setq doom-modeline-checker-simple-format t)
;;   (setq doom-modeline-icon t))

(use-package which-key
  :init
  (which-key-mode)
  (setq which-key-idle-delay 1))

(provide 'init)
;;; init.el ends here
