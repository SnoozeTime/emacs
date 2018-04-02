;; init.el -- Global settings

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))

;; -----------------------------------------
;; PACKAGES
;; ----------------------------------------
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(defvar local-packages '(helm
			 projectile
			 auto-complete
			 epc
			 jedi
			 magit
			 auctex
			 gradle-mode
			 ssh-agency
			 apel
			 flim
			 semi))

(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p)
		  (if (package-installed-p p nil) nil p))
		packages)))

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

;; --------------------------------
;; BASIC CUSTOMIZATION
;; --------------------------------

(setenv "HOME" "c:/Users/Linus")

;; Easy access to init.el
(defun find-user-init-file()
  "Edit the user init file in another window"
  (interactive)
  (find-file-other-window user-init-file))

;; PROJECTILE - Easy way to navigate in projects
(require 'projectile)
(projectile-global-mode)

;; AUTOCOMPLETE
(require 'auto-complete-config)
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)

(setenv "SSH_ASKPASS" "git-gui--askpass")
(setenv "GIT_ASKPASS" "git-gui--askpass")

;; ------------------------------------------
;; HELM
;; ----------------------------------------

(require 'helm)

;; rebind native emacs to helm
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(helm-mode 1)

;; ----------------------------------------
;; JEDI
;; ----------------------------------------

(require 'jedi)
;; Hook up to autocomplete
(add-to-list 'ac-sources 'ac-source-jedi-direct)
;; Enable for python mode
(add-hook 'python-mode-hook 'jedi:setup)

;; Where all the virtual envs are
;; (defvar virtualenv-path "/home/benoit/.local/share/virtualenvs")

;;(setq jedi:server-args
;;      '("--virtual-env" "/home/benoit/.local/share/virtualenvs/triarbitrage-bot-JykzT8-1"))

(setq jedi:complete-on-dot t)

;; ------------------------------------
;; TeX configuration
;; -----------------------------------

;; For AucTex Tex mode
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)

;; --------------------------------------
;; JAVA stuff
;; --------------------------------------
(require 'gradle-mode)

(gradle-mode 1)

;; -------------------------------------------
;; Org mode configuration
;; ------------------------------------------
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; -------------------------------------------
;; Org mode configuration
;; ------------------------------------------
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Decrease the garbage collection to default
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes (quote (adwaita)))
 '(custom-safe-themes
   (quote
    ("47744f6c8133824bdd104acc4280dbed4b34b85faa05ac2600f716b0226fb3f6" "291588d57d863d0394a0d207647d9f24d1a8083bb0c9e8808280b46996f3eb83" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
