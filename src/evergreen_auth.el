;;; -*- lexical-binding: t; -*-

(provide 'evergreen_auth)

(defun mdb/read-credentials-from-file ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/.evergreen.yml")
    (goto-char (point-min))
    (if (search-forward-regexp "api_key: \"?\\([a-z0-9]*\\)\"?$")
        (setenv "EVG_API_KEY" (match-string 1))
      (error "api key not included in ~/.evergreen.yml"))
    (goto-char (point-min))
    (if (search-forward-regexp "user: \"?\\(.*\\)\"?$")
        (setenv "EVG_API_USER" (match-string 1))
      (error "api user not included in ~/.evergreen.yml"))
    )
  )



(defun mdb/set-credentials ()
  (interactive)
  (setq api_key (getenv "EVG_API_KEY"))
  (setq api_user (getenv "EVG_API_USER"))
  (if (or
       (= (length api_key) 0)
       (= (length api_user) 0)
       )
      (mdb/read-credentials-from-file)
      )
  )
