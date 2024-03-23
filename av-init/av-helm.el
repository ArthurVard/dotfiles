;; Helm config
(use-package helm
  :bind (
	 ("C-x b" . helm-buffers-list)
	 ("M-x" . helm-M-x)
 	 ("C-x C-f" . helm-find-files)
	 ([tab] . helm-next-line)
         ([backtab] . helm-previous-line)
	 ("<C-tab>" . helm-select-action)
	 ("C-S-SPC" . helm-next-source)
	 ("C-s" . helm-occur))
  :config
  (helm-mode 1))

(provide 'av-helm)
