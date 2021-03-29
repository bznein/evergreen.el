(require 'request)

(setq api_key (getenv "EVG_API_KEY"))
(setq api_user (getenv "EVG_API_USER"))
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
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (progn
                    (setq patch_id (assoc-default 'patch_id (svref data 0)) ))))
      (request
        (concatenate 'string "https://evergreen.mongodb.com/rest/v2/patches/" patch_id)
        :type "PATCH"
        :data (json-encode '(("priority" . 30) ))
        :headers header
        :parser 'json-read
        )
      )
    )
  )
