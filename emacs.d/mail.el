(require 'gnus)

(setq gnus-select-method '(nnnil))
(setq gnus-secondary-select-methods
      '((nnmaildir "Mon_Ouie"
         (directory "~/mail/Mon_Ouie/")
         (directory-files nnheader-directory-files-safe)
         (expire-age never))

        (nnmaildir "Private"
         (directory "~/mail/Private/")
         (directory-files nnheader-directory-files-safe)
         (expire-age never))

        (nnmaildir "School"
         (directory "~/mail/School/")
         (directory-files nnheader-directory-files-safe)
         (expire-age never))))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date))

(setq gnus-summary-to-prefix "→"
      gnus-summary-newsgroup-prefix "⇶"
      ;; Marks
      gnus-ticked-mark ?⚑
      gnus-dormant-mark ?⚐
      gnus-expirable-mark ?♻
      gnus-read-mark ?✓
      gnus-del-mark ?✗
      gnus-killed-mark ?☠
      gnus-replied-mark ?↺
      gnus-forwarded-mark ?↪
      gnus-cached-mark ?☍
      gnus-recent-mark ?✩
      gnus-unseen-mark ?★
      gnus-unread-mark ?✉
      gnus-score-over-mark ?↑           ; ↑ ☀
      gnus-score-below-mark ?↓         ; ↓ ☂
      gnus-sum-thread-tree-false-root " ◌ "
      gnus-sum-thread-tree-single-indent "◎ "
      gnus-sum-thread-tree-indent "   "
      gnus-sum-thread-tree-root "● "
      gnus-sum-thread-tree-leaf-with-other "├─▶ "
      gnus-sum-thread-tree-single-leaf     "└─▶ " ; "╰─►"
      gnus-sum-thread-tree-vertical        "│ ")

(setq user-mail-address "mon.ouie@gmail.com")
(setq user-full-name    "Mon ouïe")

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-sendmail-envelope-from 'header)
(setq sendmail-program "/usr/bin/msmtp")

(require 'org-contacts)
(org-contacts-gnus-insinuate)
