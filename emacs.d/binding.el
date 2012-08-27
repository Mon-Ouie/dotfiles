(global-set-key (kbd "ESC <down>")  'windmove-down)
(global-set-key (kbd "ESC <up>")    'windmove-up)
(global-set-key (kbd "ESC <right>") 'windmove-right)
(global-set-key (kbd "ESC <left>")  'windmove-left)

(global-set-key (kbd "M-<down>")  'windmove-down)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>")  'windmove-left)

(global-set-key (kbd "M-»") 'windmove-right)
(global-set-key (kbd "M-«") 'windmove-left)
(global-set-key (kbd "M-p") 'windmove-up)
(global-set-key (kbd "M-n") 'windmove-down)

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(global-set-key (kbd "M-≤") 'previous-buffer)
(global-set-key (kbd "M-≥") 'next-buffer)

(global-set-key (kbd "C-c i") 'magit-status)

(global-set-key (kbd "<backtab> <tab>") 'yas/expand-temp-snippet)
(global-set-key (kbd "<backtab> s") 'yas/save-temp-snippet)

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  (interactive)
  (if mark-active (indent-region)
    (indent-buffer)))

(global-set-key (kbd "C-M-/") 'indent-region-or-buffer)

;; (require 'gist)
;; (global-set-key (kbd "C-c p") 'gist-region-or-buffer)
;; (global-set-key (kbd "s-p") 'gist-region-or-buffer)

(global-set-key (kbd "<M-return>") 'align)

(global-set-key (kbd "C-c c") 'org-capture)

(defun dictionary-list (word)
  (interactive
   (list
    (read-string (format "Search word (%s): " (current-word))
                 nil nil (current-word))))
  (compile (concat (executable-find "ruby")
                   " "
                   (getenv "HOME") "/code/rb/dict.rb"
                   " "
                   "list"
                   " "
                   "\"" word "\"")))

(defun dictionary-def (word)
  (interactive
   (list
    (read-string (format "Define word (%s): " (current-word))
                 nil nil (current-word))))

  (compile (concat (executable-find "ruby")
                   " "
                   (getenv "HOME") "/code/rb/dict.rb"
                   " "
                   "def"
                   " "
                   "\"" word "\"")))

(global-set-key (kbd "C-c s") 'dictionary-list)
(global-set-key (kbd "C-c d") 'dictionary-def)

(defun pry ()
  (interactive)
  (comint-run (executable-find "pry")))

(global-set-key (kbd "s-i") 'pry)
(global-set-key (kbd "C-c I") 'pry)

(global-set-key [f6] 'poem)
(global-set-key [f7] 'doc)

(global-set-key (kbd "C-x B") 'buffer-menu)

(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(require 'expand-region)
(global-set-key (kbd "C-c C-y") 'er/expand-region)

(require 'multiple-cursors)
(global-set-key (kbd "C-ù") 'mc/mark-prev-like-this)
(global-set-key (kbd "C-’") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-’") 'mc/mark-all-like-this)

(define-key org-mode-map (kbd "C->") 'org-metaright)
(define-key org-mode-map (kbd "C-<") 'org-metaleft)

(define-key org-mode-map (kbd "C-”") 'org-shiftmetaright)
(define-key org-mode-map (kbd "C-“") 'org-shiftmetaleft)

(define-key org-mode-map (kbd "M-&") 'org-shiftmetaup)
(define-key org-mode-map (kbd "M-|") 'org-shiftmetadown)

;; a keyboard that has two option keys.
(defun map-meta-to-custom()
  (setq mac-right-option-modifier 'none)
  (setq mac-option-modifier 'meta))

(defun map-meta-to-default()
  (setq mac-right-option-modifier 'meta)
  (setq mac-option-modifier 'none))

(when on-osx
  (map-meta-to-default))
