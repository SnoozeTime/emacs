;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)

;; highlight trailing whitespaces in all modes
(setq-default show-trailing-whitespace t)

;; Show column numbers by default
(setq column-number-mode t)

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Hide the scroll bar
(scroll-bar-mode -1)

;; Remove the toolbar to reclaim some space
(tool-bar-mode -1)

;; Easy access to init.el
(defun find-user-init-file()
  "Edit the user init file in another window"
  (interactive)
  (find-file-other-window user-init-file))

;; Auto-save file will be in emacs directory to avoid polluting other dir
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t))) 

(setenv "SSH_ASKPASS" "git-gui--askpass")
(setenv "GIT_ASKPASS" "git-gui--askpass")

;; Each command that set bookmark will also save them.
;; That way, if emacs crash the bookmark will not be lost
(setq bookmark-save-flag t)
