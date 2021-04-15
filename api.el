(defun mdb/evg-get-version (version_id success-callback)
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
      (concatenate 'string "https://evergreen.mongodb.com/rest/v2/versions/" version_id)
      :headers header
      :parser 'json-read
      :success success-callback
      :sync 't
      )
    )
  )

(defun mdb/evg-get-tasks-for-build-variant (build_variant success-callback)
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
      (concatenate 'string "https://evergreen.mongodb.com/rest/v2/builds/" build_variant "/tasks")
      :headers header
      :parser 'json-read
      :success success-callback
      :sync 't
      )
    )
  )

(defun mdb/evg-get-task (task handler)
  (setq handler_call handler)
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
      (concatenate 'string "https://evergreen.mongodb.com/rest/v2/tasks/" task "?fetch_all_executions")
                   :headers header
                   :parser 'json-read
                   :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler_call data)))
                   )
    )
  )

(defun mdb/evg-get-logs-for-task (task_name handler)
  (mdb/set-credentials)
  (setq api_key (getenv "EVG_API_KEY"))
  (setq api_user (getenv "EVG_API_USER"))
  (setq header (list
                (cons "Api-User" api_user)
                (cons "Api-Key" api_key)
                )
        )
  (setq handler_call handler)
  (setq url (concatenate 'string "https://evergreen.mongodb.com/task_log_raw/" task_name "/0?type=T&text=true"))
  (message url)
  (request
    url
    :headers header
    :parser 'buffer-string
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler_call data)))
    )
  )


(defun mdb/get-text-file (url handler)
  (interactive)
  (setq handler_call handler)
  (request
    url
    :headers header
    :parser 'buffer-string
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler_call data)))
    )
  )
