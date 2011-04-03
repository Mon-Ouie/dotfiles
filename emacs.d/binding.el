(global-set-key (kbd "ESC <down>")  'windmove-down)
(global-set-key (kbd "ESC <up>")    'windmove-up)
(global-set-key (kbd "ESC <right>") 'windmove-right)
(global-set-key (kbd "ESC <left>")  'windmove-left)

(global-set-key (kbd "M-<down>")  'windmove-down)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>")  'windmove-left)

(global-set-key (kbd "C-c i") 'magit-status)

(global-set-key (kbd "C-c p") 'gist-region-or-buffer)
(global-set-key (kbd "s-p") 'gist-region-or-buffer)

(defun rake-command (task)
  (concat (executable-find "ruby") " -S rake " task))

(defun rake-run-test-task()
  (interactive)
  (compile (rake-command "test")))

(defun rake-run-ext-task()
  (interactive)
  (compile (rake-command "ext")))

(defun rake-run-ext-rebuild-task()
  (interactive)
  (compile (rake-command "ext:rebuild")))

(global-set-key (kbd "C-c t") 'rake-run-test-task)
(global-set-key (kbd "s-t") 'rake-run-test-task)

(global-set-key (kbd "C-c e") 'rake-run-ext-task)
(global-set-key (kbd "s-e") 'rake-run-ext-task)

(global-set-key (kbd "C-c E") 'rake-run-ext-rebuild-task)
(global-set-key (kbd "s-E") 'rake-run-ext-rebuild-task)

(defun run-inf-ruby-in-current-dir()
  (interactive)
  (cd (file-name-directory (buffer-file-name (current-buffer))))
  (inf-ruby))

(global-set-key (kbd "s-i") 'inf-ruby)
(global-set-key (kbd "C-c I") 'inf-ruby)

;; Useful on a keyboard that has two option keys.
(defun map-meta-to-custom()
  (setq mac-right-option-modifier 'none)
  (setq mac-option-modifier 'meta))

(defun map-meta-to-default()
  (setq mac-right-option-modifier 'meta)
  (setq mac-option-modifier 'none))

(when on-osx
  (map-meta-to-default))
