(require 'jabber)

(setq jabber-account-list
      '(("lifted@yandex-team.ru"
	 (:network-server . "xmpp.yandex-team.ru")
	 (:port . 5222)))

      jabber-history-enabled t
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30
      jabber-chat-buffer-show-avatar nil
      jabber-vcard-avatars-retrieve nil)

(defvar libnotify-program "notify-send")

(let ((notify-send-prog (executable-find libnotify-program)))
  (when notify-send-prog

    (defun notify-send (title message)
      (start-process "notify" " notify"
		     libnotify-program "--expire-time=4000" title message))


    (defun libnotify-jabber-notify (from buf text proposed-alert)
      "(jabber.el hook) Notify of new Jabber chat messages via libnotify"
      (when (or jabber-message-alert-same-buffer
		(not (memq (selected-window) (get-buffer-window-list buf))))
	(if (jabber-muc-sender-p from)
	    (notify-send (format "(PM) %s"
				 (jabber-jid-displayname (jabber-jid-user from)))
			 (format "%s: %s" (jabber-jid-resource from) text)))
	(notify-send (format "%s" (jabber-jid-displayname from))
		     text)))

    (add-hook 'jabber-alert-message-hooks 'libnotify-jabber-notify)))

(provide 'my-jabber)
