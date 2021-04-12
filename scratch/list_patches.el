
(defun mdb/evg-list-patches ()
  (interactive)
  (setq patch_id "")
  (mdb/set-credentials)
  (setq api_key (getenv "EVG_API_KEY"))
  (setq api_user (getenv "EVG_API_USER"))
  (progn
    (setq header (list
                  (cons "Api-User" api_user)
                  (cons "Api-Key" api_key)
                  )
          )
    (request
      (concatenate 'string "https://evergreen.mongodb.com/rest/v2/users/" api_user "/patches?limit=10")
      :headers header
      :parser 'json-read
      :sync 't
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (progn
                    (setq choices ())
                    (loop for e across data do
                          (progn
                            (setq patch_id
                                  (assoc-default 'patch_id e))
                            (setq description
                                  (assoc-default 'description e))
                            (setq number
                                  (assoc-default 'patch_number e))
                            (setq projectq
                                  (assoc-default 'project_id e))
                            (setq choices
                                  (append choices
                                          (list
                                           (cons
                                            (concatenate 'string project " - " (number-to-string number) " - "  description " - "  status)
                                            patch_id)
                                           )
                                          )
                                  )
                            )
                          )
                    ;; TODO this gives issues if the user exits
                    (setq choice (completing-read
                                  "Evergreen patch: " choices nil t "")
                          )
                    (setq id (assoc-default choice choices))
                    (shell-command (message (concat "open https://spruce.mongodb.com/version/" id )))
                    )
                  )
                )
      )
    )
  )
