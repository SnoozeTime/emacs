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
