(use-package org)

;(ensure-package-installed
; 'org-bullets
;)

;(use-package org-bullets
;    :init
;    (add-hook 'org-mode-hook 'org-bullets-mode))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(defun concat-to-org-dir (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(defun concat-to-orgzly-dir (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory orgzly-directory) filename))

(defun concat-to-notes-dir (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory notes-directory) filename))

;; paths

 ;; "~/orgs"
 (defvar org-root-dir "/mnt/e/==From_NIXOS/orgs")
 (setq org-directory org-root-dir)
 (setq orgzly-directory (concat org-root-dir "/orgzly"))
 (setq notes-directory (concat org-root-dir  "/notes"))

 ;; 
'(org-id-locations-file "~/orgs/.org-id-locations")

 ;; main files
 (setq org-inbox-file   (concat-to-org-dir    "inbox.org"))

 (setq org-index-file   (concat-to-orgzly-dir "index.org"   ))
 (setq org-mylearn-file (concat-to-orgzly-dir "mylearn.org" ))
 (setq org-mywork-file  (concat-to-orgzly-dir "mywork.org"  ))

 (setq org-blog-idea-file  (concat-to-notes-dir "blog-ideas.org"  ))
 (setq org-books-read-file  (concat-to-notes-dir "books-read.org"  ))
 (setq org-to-read-file  (concat-to-notes-dir "to-read.org"  ))

 (setq org-rss-urls-file   (concat-to-org-dir    "rss/urls"))
 (setq org-code-gists-file   (concat-to-org-dir    "code/gists.org"))


;; Agenda files
 (setq org-agenda-files (list org-index-file org-mylearn-file org-mywork-file))

;; Archives
    ;(setq org-archive-location
     ;     (concat (concat-to-org-dir "archive.org") "::* From %s"))

  ;; YYYY-MM-filename.org
   (setq org-archive-location
     (concat org-directory
	(concat "/archive/" (format-time-string "%Y-%m" (current-time))) "-%s::* "(format-time-string "%Y-%m-%d" (current-time)))
     )

;;    (setq org-archive-location
  ;;        (concat org-directory  "/archive/%s::datetree/"))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; a new parent that you create on-the-fly
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-refile-targets
    '((nil :maxlevel . 3)
      (org-agenda-files :maxlevel . 3)))

(defun hrs/copy-tasks-from-inbox ()
  (when (file-exists-p org-inbox-file)
    (save-excursion
      (find-file org-index-file)
      (goto-char (point-max))
      (insert-file-contents org-inbox-file)
      (delete-file org-inbox-file))))

(defun hrs/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)

(setq org-log-done 'time)

(use-package deft)

(require 'deft)

(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory org-directory)

  (setq org-capture-templates
	'( 
	  ("p" "Private Tempaltes")
	  ("pb" "Blog idea"
	   entry
	   (file org-blog-idea-file)
	   "* %?\n"
	   )
	  ("pe" "Email" entry
	   (file+headline org-index-file "Index")
	   "* TODO %?\n\n%a\n\n")

	  ("pf" "Finished book"
	   table-line (file org-books-read-file)
	   "| %^{Title} | %^{Author} | %u |")

	  ("pr" "Reading"
	   checkitem
	   (file org-to-read-file)
	     )

	  ("ps" "Subscribe to an RSS feed"
	   plain
	   (file org-rss-urls-file)
	   "%^{Feed URL} \"~%^{Feed name}\"")

	  ("pt" "Todo"
	   entry
	   (file+headline org-index-file "Index")
	   "* TODO %?\n")


	  ("t" "Todo"
	   entry
	   (file+headline org-index-file "Index")
	   "* TODO %?\n")
	  ("g" "Code trick from chat"
	   entry
	   (file org-code-gists-file)
	   "* %^{Gist Title} \n#+BEGIN_SRC haskell \n%?  \n#+END_SRC \n :CODEGIST: \n  - Added: %U \n :END:")

	  )
	)

;  (add-hook 'org-capture-mode-hook 'evil-insert-state)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map (kbd "<f6>") 'org-capture)

(defun hrs/open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (hrs/copy-tasks-from-inbox)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'hrs/open-index-file)

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)
(add-hook 'gfm-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))
(add-hook 'haskell-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))

(require 'ox-md)
(require 'ox-beamer)

(use-package graphviz-dot-mode)

(use-package gnuplot)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (ruby . t)
       (dot . t)
       (gnuplot . t)))

(setq org-confirm-babel-evaluate nil)

(use-package graphviz-dot-mode)
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-export-with-smart-quotes t)

(setq org-html-postamble nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setenv "BROWSER" "firefox")

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
