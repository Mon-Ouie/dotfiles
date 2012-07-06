(require 'gnus)
(setq gnus-select-method '(nnnil))

(setq gnus-secondary-select-methods
      '((nnmaildir ""
         (directory "~/mail/")
         (directory-files nnheader-directory-files-safe)
         (expire-age never)
         (get-new-mail t))))

(setq mail-sources '((maildir :path    "~/mail"
                              :subdirs ("cur" "new" "inbox"))))

(setq user-mail-address "mon.ouie@gmail.com")
(setq user-full-name    "Mon ouïe")

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-sendmail-envelope-from 'header)
(setq sendmail-program "/usr/bin/msmtp")

(require 'bbdb)

(setq bbdb-file "~/doc/bbdb")
(bbdb-initialize)
