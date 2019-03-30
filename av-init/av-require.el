
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


(ensure-package-installed
; 'intero
 'web-mode
 'coffee-mode
 'emmet-mode
 'projectile
 'ido
 'ido-yes-or-no
; 'ido-ubiquitous - redudant
 'ido-vertical-mode
 'smartparens
 'fill-column-indicator
; 'flycheck
; 'flycheck-haskell
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
 'elixir-yasnippets
 'alchemist ; elixir toolchain https://github.com/tonini/alchemist.el
 )

(provide 'av-require)
