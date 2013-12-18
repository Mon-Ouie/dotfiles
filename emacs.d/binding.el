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

(global-set-key (kbd "M-è") 'delete-indentation)

(global-set-key (kbd "C-c i") 'magit-status)

(global-set-key (kbd "<backtab> <tab>") 'yas/expand-temp-snippet)
(global-set-key (kbd "<backtab> s") 'yas/save-temp-snippet)

(defun my-helm ()
  (interactive)
  (require 'helm-files)
  (require 'helm-eval)
  (require 'helm-imenu)
  (require 'helm-info)
  (require 'helm-man)
  (require 'helm-misc)
  (require 'helm-org)
  (require 'helm-ls-git)
  (helm-other-buffer `(helm-source-buffers-list
                       helm-source-ls-git
                       helm-source-recentf
                       helm-source-occur
                       helm-source-google-suggest
                       ,(if (eq major-mode 'org-mode)
                            'helm-source-org-headline
                          'helm-source-imenu)
                       helm-source-info-emacs
                       helm-source-info-elisp
                       helm-source-man-pages
                       helm-source-fixme
                       helm-source-buffer-not-found
                       helm-source-evaluation-result)
                     "*helm*"))

(global-set-key (kbd "C-c h") 'my-helm)

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  (interactive)
  (if (use-region-p) (indent-region (region-beginning) (region-end))
    (indent-buffer)))

(global-set-key (kbd "C-M-/") 'indent-region-or-buffer)
(global-set-key (kbd "<M-return>") 'align)

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

(global-set-key [f6] 'poem)
(global-set-key [f7] 'doc)

(require 'ibuffer)
(global-set-key (kbd "C-x B") 'ibuffer)
(define-key ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file)
(define-key ibuffer-mode-map (kbd "C-x b") 'ido-switch-buffer)

(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(require 'expand-region)
(global-set-key (kbd "C-c C-y") 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'multiple-cursors)
(global-set-key (kbd "C-ù") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-’") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-’") 'mc/mark-all-like-this)

(define-key org-mode-map (kbd "C->") 'org-metaright)
(define-key org-mode-map (kbd "C-<") 'org-metaleft)

(define-key org-mode-map (kbd "C-”") 'org-shiftmetaright)
(define-key org-mode-map (kbd "C-“") 'org-shiftmetaleft)

(define-key org-mode-map (kbd "M-&") 'org-shiftmetaup)
(define-key org-mode-map (kbd "M-|") 'org-shiftmetadown)

(global-set-key (kbd "C-c c") 'org-capture)

(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map [return] nil)

(require 'paredit)
(define-key paredit-mode-map (kbd "<M-up>") nil)
(define-key paredit-mode-map (kbd "<M-down>") nil)

(require 'inf-haskell)
(define-key haskell-mode-map (kbd "C-c C-c") 'inferior-haskell-reload-file)

;; a keyboard that has two option keys.
(defun map-meta-to-custom()
  (setq mac-right-option-modifier 'none)
  (setq mac-option-modifier 'meta))

(defun map-meta-to-default()
  (setq mac-right-option-modifier 'meta)
  (setq mac-option-modifier 'none))

(when on-osx
  (map-meta-to-default))

;; Capital U for public to avoid accidentally making public something I don't
;; want.
(require 'gist-paste)
(global-set-key (kbd "C-c C-u") 'gist-region-or-buffer-private-paste)
(global-set-key (kbd "C-c C-U") 'gist-region-or-buffer-paste)

(require 'visual-regexp)
(global-set-key (kbd "M-%") 'vr/query-replace)
(global-set-key (kbd "M-/") 'vr/replace)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c p"))
(guide-key-mode t)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-kbd-macro)

(global-set-key (kbd "M-g") 'goto-line)
