;; init.el -- Global settings

;; Increase the garbage collection threshold to 500 MB to ease startup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq gc-cons-threshold (* 500 1024 1024))

;;; Load the config
(org-babel-load-file (concat user-emacs-directory "configuration.org"))
