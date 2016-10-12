(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

; make completion buffers disappear after 3 seconds.
(add-hook 'completion-setup-hook
  (lambda () (run-at-time 3 nil
    (lambda () (delete-windows-on "*Completions*")))))

;; run a few shells.
;(shell "*shell5*")
;(shell "*shell6*")
;(shell "*shell7*")

; C-5, 6, 7 to switch to shells
;(global-set-key [(control \5)]
;  (lambda () (interactive) (switch-to-buffer "*shell5*")))
;(global-set-key [(control \6)]
;  (lambda () (interactive) (switch-to-buffer "*shell6*")))
;(global-set-key [(control \7)]
;  (lambda () (interactive) (switch-to-buffer "*shell7*")))
