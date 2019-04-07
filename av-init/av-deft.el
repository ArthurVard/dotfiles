;; https://jblevins.org/projects/deft/

(ensure-package-installed
 'deft
)

(require 'deft)

(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/documents/")

(provide 'av-deft)
