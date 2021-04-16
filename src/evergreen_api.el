;;; -*- lexical-binding: t; -*-

(provide 'evergreen_api)

(require 'evergreen_auth)


(defun mdb/evg-get-header ()
  (mdb/set-credentials)
  (list
   (cons "Api-User" (getenv "EVG_API_USER"))
   (cons "Api-Key" (getenv "EVG_API_KEY"))
   )
  )

(defun mdb/evg-get-version (version_id success-callback)
  (progn
    (request
      (concat "https://evergreen.mongodb.com/rest/v2/versions/" version_id)
      :headers (mdb/evg-get-header)
      :parser 'json-read
      :success success-callback
      :sync 't
      )
    )
  )

(defun mdb/evg-get-tasks-for-build-variant (build_variant success-callback)

    (request
      (concat "https://evergreen.mongodb.com/rest/v2/builds/" build_variant "/tasks")
      :headers (mdb/evg-get-header)
      :parser 'json-read
      :success success-callback
      :sync 't
      )
    )


(defun mdb/evg-get-task (task handler)
  (setq handler_call handler)
    (request
      (concat "https://evergreen.mongodb.com/rest/v2/tasks/" task "?fetch_all_executions")
      :headers (mdb/evg-get-header)
      :parser 'json-read
      :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler_call data)))
      )
    )


(defun mdb/evg-get-logs-for-task (task_name handler)
  (setq handler_call handler)
  (setq url (concat "https://evergreen.mongodb.com/task_log_raw/" task_name "/0?type=T&text=true"))
  (message url)
  (request
    url
    :headers (mdb/evg-get-header)
    :parser 'buffer-string
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler_call data)))
    )
  )


(defun mdb/get-text-file (url handler)
  (interactive)
  (setq handler_call handler)
  (request
    url
    :headers (mdb/evg-get-header)
    :parser 'buffer-string
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler_call data)))
    )
  )

(defun mdb/evg-get-all-projects (handler)
  (interactive)
  (setq handler_call handler)
  (request
    "https://evergreen.mongodb.com/rest/v2/projects?limit=10000"
    :headers (mdb/evg-get-header)
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler_call data)))
    )
  )

(defun mdb/evg-get-project-versions (id handler)
  (interactive)
  (setq handler_call handler)
  (request
    ;; TODO parametrize limit
    (concat "https://evergreen.mongodb.com/rest/v2/projects/" id "/versions?limit=10")
    :headers (mdb/evg-get-header)
    :parser 'json-read
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler_call data)))
    )
  )

(defun mdb/evg-get-all-patches (limit success-callback)
  (setq patch_id "")
  (request
    (concat "https://evergreen.mongodb.com/rest/v2/users/" api_user "/patches?limit=" (number-to-string limit))
    :headers (mdb/evg-get-header)
    :parser 'json-read
    :success success-callback
    )
  )



(defun mdb/bump-priority (priority)
    (request
      (concat "https://evergreen.mongodb.com/rest/v2/users/" api_user "/patches?limit=1")
      :headers (mdb/evg-get-header)
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
                      (concat "https://evergreen.mongodb.com/rest/v2/patches/" patch_id)
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
