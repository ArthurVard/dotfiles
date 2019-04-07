;;; init.el -- config
;;; Commentary:
;;; Code:

;;; Last Modified : 2019 Mar 30 (Sat) 06:01:45 PM by Arthur Vardanyan.

;;; Initialize package system

;; =============================================================================
;; Initialize and install Packages
;; =============================================================================


;; Configure package.el to include MELPA.
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

(require 'av-engine) ;; search the web



(require 'av-global)

;; Highlight the current line
(global-hl-line-mode)

(ensure-package-installed
 'diff-hl
)
(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))



(org-babel-load-file "~/.emacs.d/av-init/config.org")

(setq org-file-apps
    (quote
        ((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . "/run/current-system/sw/bin/firefox %s")
        ("\\.pdf\\'" . default))))


;;;; customize bits goes in another file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
