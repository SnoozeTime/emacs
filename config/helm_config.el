(require 'helm)

;; rebind native emacs to helm
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; Fuzzy matching for faster search
(setq helm-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)


(helm-mode 1)
