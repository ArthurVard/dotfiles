;;; https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:2F4C0A6C-96BE-4818-B794-D1593C23FB00
;;; 
 ;; (defun lc/get-font-size ()
 ;;    "font size is calculated according to the size of the primary screen"
 ;;    (let* (;; (command "xrandr | awk '/primary/{print sqrt( ($(nf-2)/10)^2 + ($nf/10)^2 )/2.54}'")
 ;;           (command "osascript -e 'tell application \"finder\" to get bounds of window of desktop' | cut -d',' -f3")
 ;;           (screen-width (string-to-number (shell-command-to-string command))))  ;;<
 ;;      (if (> screen-width 2560) lc/laptop-font-size lc/laptop-font-size))) 

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))
(use-package dashboard
  :init
  (defun lc/is-after-17-or-weekends? ()
    (or (thread-first (nth 3 (split-string (current-time-string) " ")) ;; time of the day e.g. 18
            ;; (substring 0 2)
            (string-to-number)   ;;<
            (> 16))
        (thread-first (substring (current-time-string) 0 3) ;; day of the week e.g. Fri
            (member  '("Sat" "Sun")))))

  (defun lc/dashboard-get-next ()
    "Get agenda items for today or for a week from now."
    (org-compile-prefix-format 'agenda)
    (org-map-entries 'lc/dashboard-agenda-entry-format
                     dashboard-match-next-entry
                     'agenda))
  (defun lc/dashboard-insert-next (list-size)
    "Add the list of LIST-SIZE items of next tasks"
    (require 'org-agenda)
    (let ((next (lc/dashboard-get-next)))
      (dashboard-insert-section
       "Next tasks"
       next
       list-size
       "n"
       `(lambda (&rest ignore)
          (let ((buffer (find-file-other-window (nth 2 ',el))))
            (with-current-buffer buffer
              (goto-char (nth 1 ',el))
              (switch-to-buffer buffer))))
       (format "%s" (nth 0 el)))))
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "cogito, ergo sum")
  ;;(setq dashboard-startup-banner "~/Downloads/")
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `((;; Github
           (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Github"
            "Go to wondercast"
            (lambda (&rest _) (browse-url "https://github.com/Maersk-Global/wondercast")))
           ;; Codebase
           (,(all-the-icons-faicon "briefcase" :height 1.1 :v-adjust -0.1)
            "JIRA"
            "Go to Kanban"
            (lambda (&rest _) (browse-url "https://jira.maerskdev.net/secure/RapidBoard.jspa?rapidView=6378&projectKey=AVOC&quickFilter=15697")))
           ;; Perspectives
           (,(all-the-icons-octicon "history" :height 1.1 :v-adjust 0.0)
            "Restore"
            "Restore"
            (lambda (&rest _) (persp-state-load persp-state-default-file)))
           )))
  ;; exclude work items after 17 and on weekends
  (setq dashboard-match-next-entry "TODO=\"NEXT\"-work")
  (run-at-time "00:00" (* 60 60 24)
               (lambda ()
                 (if (lc/is-after-17-or-weekends?)
                     (setq dashboard-match-agenda-entry "life|habits"
                           dashboard-match-next-entry "TODO=\"NEXT\"-work")
                   (setq dashboard-match-agenda-entry "work|life|habits"
                         dashboard-match-next-entry "TODO=\"NEXT\""
                         ))))
  (dashboard-setup-startup-hook)
  (set-face-attribute 'dashboard-items-face nil :height 12) ;;(lc/get-font-size))
  ;; do not show tags in agenda view
;  (advice-add 'dashboard-get-agenda :override #'lc/dashboard-get-agenda)
  ;; show next tasks in dashboard
 ; (add-to-list 'dashboard-item-generators  '(next . lc/dashboard-insert-next))
  (setq dashboard-items '(
			  
			  (agenda . 5)
;                          (next . 10)
                          (bookmarks . 5)
                          (recents  . 5)
                          (projects . 5)))
  
)



(provide 'av-dashboard)
