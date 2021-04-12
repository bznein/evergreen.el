(defun mdb/create-patch ()
  (interactive)
  (mdb/set-credentials)
  (setq api_key (getenv "EVG_API_KEY"))
  (setq api_user (getenv "EVG_API_USER"))
  (setq task_to_run (mdb/evg-get-task))
  (setq build_variants (mdb/evg-get-matching-build-variants-for-task task_to_run))
  (setq description_command "")
  (setq description (read-string "Description: "))
  (if
      (not (= (length description) 0))
      (setq description_command (concatenate 'string " -d " description))
    )
  (setq priority (read-string "Priority: "))
  (shell-command
   (concatenate 'string
                "evergreen patch"
                " -p " (replace-regexp-in-string "\n\\'" "" (mdb/get-current-repo))
                " -t " task_to_run
                " -v " (s-join " -v " build_variants)
                description_command
                " -f -y -u -browse"
                )
   )
  (if
      (not (= (length priority) 0))
      (mdb/bump-priority  priority)
      )
  )


(defun mdb/bump-priority (priority)
  (setq api_key (getenv "EVG_API_KEY"))
  (setq api_user (getenv "EVG_API_USER"))
  (setq patch_id "")
  (if (or
       (= (length api_key) 0)
       (= (length api_user) 0)
       )
      (message "Please make sure to set the env variables $EVG_API_KEY and $EVG_API_USER")
    (progn
      (setq header (list
                    (cons "Api-User" api_user)
                    (cons "Api-Key" api_key)
                    )
            )
      (request
        (concatenate 'string "https://evergreen.mongodb.com/rest/v2/users/" api_user "/patches?limit=1")
        :headers header
        :parser 'json-read
        :sync 't
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (progn
                      (setq patch_id
                            (assoc-default 'patch_id
                                           (svref data 0)
                                           )
                            )
                      (request
                        (concatenate 'string "https://evergreen.mongodb.com/rest/v2/patches/" patch_id)
                        :type "PATCH"
                        :data (json-encode
                               (list
                                (cons "priority" (string-to-number priority))
                                )
                               )
                        :headers header
                        :parser 'json-read
                        )
                      )
                    )
                  )
        )
      )
    )
  )
