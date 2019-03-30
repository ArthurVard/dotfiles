(ensure-package-installed
 'magit
 'git-gutter
)
(require 'magit)
(require 'git-gutter)

(use-package magit
  :config
  (progn
    (setq git-commit-summary-max-length 80)
    (setq git-commit-fill-column        80)
    ))


(use-package git-gutter
  :config
  (progn
    (global-git-gutter-mode t)
    ;; HC: enabling this breaks git-gutter
    ;; to use git-gutter and linum-mode
    ;;(git-gutter:linum-setup)
    ))

(provide 'av-git)
