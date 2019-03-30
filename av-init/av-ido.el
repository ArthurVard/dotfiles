; buffer switching ido mode
(ensure-package-installed
 'ido
 'ido-yes-or-no
; 'ido-ubiquitous - redudant
 'ido-vertical-mode
)

(require 'ido)
(require 'ido-vertical-mode)
(require 'ido-yes-or-no)

;; == ido ==
;; make buffer switch command auto suggestions, also for find-file command
(ido-mode 1)
(ido-yes-or-no-mode 1)
(setq
 ido-create-new-buffer    'always
 ido-enable-flex-matching t ;; display any item that contains the chars you typed
 ido-everywhere           t)

;(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ "
                           "*Completions*"
                           "*Shell Command Output*"
			   "*Messages*"
                           "Async Shell Command"))

(ido-ubiquitous-mode 1) ; use IDO everywhere

;to use up and down to navigate the options, or:
;to use left and right to move through the history/directories.
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-vertical-mode 1) ; display IDO vertically

(provide 'av-ido)
