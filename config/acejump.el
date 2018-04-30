;;
;; ace jump mode major function
;;
(add-to-list 'load-path "/home/benoit/.emacs.d/manual-install/ace-jump-mode.el")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
