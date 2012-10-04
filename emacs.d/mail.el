(require 'mu4e)

(setq mu4e-maildir "~/mail")

(setq mu4e-maildir-shortcuts
      '( ("/Mon_Ouie/INBOX" . ?m)
         ("/School/INBOX"   . ?s)
         ("/Private/INBOX"  . ?p)))

(setq mu4e-get-mail-command "offlineimap")

(setq user-mail-address "mon.ouie@gmail.com")
(setq user-full-name    "Mon ouïe")

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-sendmail-envelope-from 'header)
(setq sendmail-program "/usr/bin/msmtp")

(require 'org-contacts)

;; Switch to gnu's HTML renderer
(defun mail-shr-html2text ()
  (let* ((doc (with-temp-buffer
                (insert html)
                (libxml-parse-xml-region (point-min) (point-max))))
         (content (with-temp-buffer
                    (shr-insert-document doc)
                    (buffer-string))))
    (erase-buffer)
    (insert content)))

(fset 'html2text 'mail-shr-html2text)
