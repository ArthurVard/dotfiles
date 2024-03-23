;; https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:686F7A63-013E-48ED-AC56-DF39BD398E20


(use-package marginalia
  :after vertico
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode)
  (with-eval-after-load 'projectile
    (add-to-list 'marginalia-command-categories '(projectile-find-file . file)))
  )

(use-package embark
  :after vertico
  :general
  (general-nmap "C-l" 'embark-act)
  (vertico-map
   "C-l" #'embark-act
   )
  (:keymaps 'embark-file-map
            ;; "o" 'find-file-other-window
            "x" 'lc/dired-open-externally
            ) 
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)
  )

(use-package wgrep
  :general
  (grep-mode-map "W" 'wgrep-change-to-wgrep-mode)
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  )

(use-package consult
  :commands (consult-ripgrep)
  :general
  (general-nmap
    :states '(normal insert)
    "C-p" 'consult-yank-pop)
  (lc/leader-keys
    "s i" '(consult-isearch :wk "isearch")
    "s o" '(consult-outline :which-key "outline")
    "s s" 'consult-line
    "s p" '(consult-ripgrep :wk "ripgrep project")
    "b b" 'consult-buffer
    ;; TODO consult mark
    "f r" 'consult-recent-file
    ;; "s !" '(consult-flymake :wk "flymake")
    )
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)  
  ;; (setq consult-preview-key "C-l")
  ;; (setq consult-narrow-key ">")
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (with-eval-after-load 'selectrum
    (require 'consult-selectrum))
  )

(use-package embark-consult
  :after (embark consult)
  ;; :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  ;; :hook
  ;; (embark-collect-mode . embark-consult-preview-minor-mode)
  )


(use-package vertico
  ;; :straight (vertico :type git :host github :repo "minad/vertico")
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                ;; vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :demand
  :hook
  ((minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved for `vertico-repeat'
   (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
   ) 
  :general
  (:keymaps 'vertico-map
            "C-j" #'vertico-next
            "C-k" #'vertico-previous
            "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
            ;; "C-;" #'kb/vertico-multiform-flat-toggle
            "M-<backspace>" #'vertico-directory-delete-word
            )
  (:keymaps '(normal insert visual motion)
            "M-." #'vertico-repeat) ; Perfectly return to the state of the last Vertico minibuffer usage
  ;; :bind (:map vertico-map
  ;;             ("C-j" . vertico-next)
  ;;             ("C-k" . vertico-previous)
  ;;             ("<escape>" . minibuffer-keyboard-quit)
  ;;             )
  :init
  ;; (setq vertico-resize t)
  
  ;; multiform extension
  (setq vertico-grid-separator "       ")
  (setq vertico-grid-lookahead 50)
  (setq vertico-buffer-display-action '(display-buffer-reuse-window))
  (setq vertico-multiform-categories
        '((file indexed)
          (consult-grep buffer)
          (consult-location)
          (imenu buffer)
          (library reverse indexed)
          (org-roam-node reverse indexed)
          (t reverse)
          ))
  (setq vertico-multiform-commands
        '(("flyspell-correct-*" grid reverse)
          (org-refile grid reverse indexed)
          (consult-yank-pop indexed)
          (consult-flycheck)
          (consult-lsp-diagnostics)
          ))
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun lc/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun lc/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 lc/basic-remote-try-completion lc/basic-remote-all-completions nil))

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  :config
  ;; (vertico-multiform-mode) 
  (vertico-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))

  
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


(use-package dabbrev
  :general
  (python-mode-map
   :states 'insert
   "<backtab>" 'dabbrev-completion
   ;; ("C-M-/" . dabbrev-expand)
   )
  )


;; Configure corfu
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  ;; :hook (after-init . corfu-global-mode)
  :hook ((prog-mode . corfu-mode)
         (org-mode . corfu-mode))
  :bind
  (:map corfu-map
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous))
  :general
  (evil-insert-state-map "C-k" nil)
  :init
  (setq corfu-auto nil)                 ;; Enable auto completion
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-min-width 80)
  (setq corfu-max-width corfu-min-width)       ; Always have the same width
   (setq corfu-preselect-first t)   
   
   (defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
              (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
    (corfu-mode 1)))
   (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  ;; :custom
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
   )



(use-package kind-icon
  :straight (kind-icon :type git :host github :repo "jdtsmith/kind-icon")
  :after corfu :demand
  :init
  (setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (setq kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (setq kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; refresh kind icon cache to match theme 
  (with-eval-after-load 'modus-themes
    (add-hook 'modus-themes-after-load-theme-hook #'(lambda () (interactive) (kind-icon-reset-cache))))

)
