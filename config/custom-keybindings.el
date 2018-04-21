;; Define the custom keybindings (C-c [azAZ] are for user)
(global-set-key (kbd "C-c n") 'my/create-new-project-note)
(global-set-key (kbd "C-c u") 'find-user-init-file)

;; Duplicate line (override delete char)
(global-set-key (kbd "C-d") 'duplicate-line)
