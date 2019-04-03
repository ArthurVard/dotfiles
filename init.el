;;; init.el -- config
;;; Commentary:
;;; Code:

;;; Last Modified : 2019 Mar 30 (Sat) 06:01:45 PM by Arthur Vardanyan.

;;; Initialize package system

;; =============================================================================
;; Initialize and install Packages
;; =============================================================================

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile  (require 'use-package))
(require 'bind-key)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(defun user-full-name () "Arthur Vardanyan")


(defvar av-emacs-config-path "~/.emacs.d")

(push (expand-file-name "~/.emacs.d/av-init") load-path)


(defun av-load (filename) "FILENAME."
;  (interactive)
  (load-file filename))


;; =============================================================================
;; Packages and Configs
;; =============================================================================
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


(require 'av-yasnippet)
(require 'av-ido)
(require 'av-web)
(require 'av-fci)
(require 'av-smartparens)
(require 'av-smex)
(require 'av-haskell)
(require 'av-git)
(require 'av-racket)
(require 'av-dir-tree)
(require 'av-nix)



(require 'av-global)



;; =============================================================================
;; Custom set variables section
;; =============================================================================

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
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (nix-mode all-the-icons-dired dired-sidebar smex smartparens ido-vertical-mode ido-yes-or-no use-package)))
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
