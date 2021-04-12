(defun mdb/read-credentials-from-file ()
  (interactive)
  ;; TODO make file path customizable
  (setq credentials (json-read-file "~/.evergreen_credentials"))
  (if (or
       (not (assoc 'EVG_API_KEY credentials))
       (not (assoc 'EVG_API_USER credentials))
       )
      (progn
        (message "Can't find either EVG_API_KEY or EVG_API_USER entries in ~/.evergreen_credentials")
        nil
        )
    (progn
      (setenv "EVG_API_KEY" (assoc-default 'EVG_API_KEY credentials))
      (setenv "EVG_API_USER" (assoc-default 'EVG_API_USER credentials))
      )
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
