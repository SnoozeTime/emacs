;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Nicer bullets
(add-hook 'org-mode-hook
	  (lambda ()
	    (org-bullets-mode t)))
(setq org-hide-leading-stars t)

;; Set up org capture to take notes on the fly
(setq org-default-notes-file "C:/Users/Linus/Nextcloud/notes/notes.org")
(define-key global-map "\C-cc" 'org-capture)


(setq org-agenda-files '("~/Nextcloud/gtd/inbox.org"
			 "~/Nextcloud/gtd/gtd.org"
			 "~/Nextcloud/gtd/tickler.org"
			 "~/Nextcloud/gtd/agenda_entry.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Nextcloud/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Nextcloud/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/Nextcloud/gtd/gtd.org" :maxlevel . 3)
                           ("~/Nextcloud/gtd/someday.org" :level . 1)
                           ("~/Nextcloud/gtd/tickler.org" :maxlevel . 2)))

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))


;; ---------------------------------------------
;; Quickly create project notes and put them in
;; the correct folder
(require 'helm)

(defvar my/project-path "~/Nextcloud/notes/projects")

(defun my/pick-project ()
  "Prompt user to pick a choice from a list."
  (let ((choices (directory-files my/project-path)))
    (message "%s" (completing-read "Open bookmark:" choices ))))

(defun my/choose-note-name ()
  "Prompt user to choose a note name"
  (read-string "Choose the note name: "))


(defun my/create-note-name ()
  (let ((project-name (my/pick-project))
	(note-name (my/choose-note-name)))
    (concatenate 'string
		 my/project-path
		 "/"
		 project-name
		 "/"
		 note-name
		 ".org")))

(defun my/create-new-project-note ()
  (interactive)
  (let ((filename (my/create-note-name)))
    (find-file-other-window filename)
    (org-mode)))
