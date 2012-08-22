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

(setq user-mail-address "mon.ouie@gmail.com")
(setq user-full-name    "Mon ouïe")

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-sendmail-envelope-from 'header)
(setq sendmail-program "/usr/bin/msmtp")

(require 'bbdb)

(setq bbdb-file "~/doc/bbdb")
(bbdb-initialize)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
