;;; -*- lexical-binding: t; -*-
(require 'evergreen)
(require 'evergreen_api)
(require 'evergreen_patch)
(require 'evergreen_auth)

(defun mdb/evg-list-patches ()
  (interactive)
  (mdb/evg-get-all-patches 10 (cl-function
                        (lambda (&key data &allow-other-keys)
                          (progn
                            (setq choices ())
                            (seq-do
                             (lambda (e)
                                 (progn
                                    (setq patch_id
                                          (assoc-default 'patch_id e))
                                    (setq description
                                          (assoc-default 'description e))
                                    (setq number
                                          (assoc-default 'patch_number e))
                                    (setq project
                                          (assoc-default 'project_id e))
                                    (setq status
                                          (assoc-default 'status e))
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
                             data
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





(defun mdb/evg-next-patches ()
  (interactive)
  (setq start (+ start 10))
  (setq limit (+ start 10))
  (mdb/evg-show-all-patches 't)
  )

(defun mdb/evg-prev-patches ()
  (interactive)
  (setq start (- start 10))
  (if (< start 0)
      (setq start 0)
    )
  (setq limit (+ start 10))
  (mdb/evg-show-all-patches 't)
  )


;;;###autoload
(defun mdb/evg-show-all-patches (&optional skip_val)
  (interactive)
  (switch-to-buffer (get-buffer-create "EVG"))
  (mdb-evg-mode)
  (if (not skip_val)
      (progn
        (setq start 0)
        (setq limit 10)
        )
    )
  (mdb/evg-get-all-patches limit (cl-function
                            (lambda (&key data &allow-other-keys)
                              (with-current-buffer "EVG"
                                (read-only-mode 0)
                                (erase-buffer)
                                (insert "Recent Patches:")
                                (newline)
                                (seq-do
                                 (lambda (patch)
                                   (insert
                                    (propertize
                                     (concat
                                      (format "  %9s" (alist-get 'status patch))
                                      " "
                                      (concat
                                       "\""
                                       (let ((description (alist-get 'description patch)))
                                         (if (> (length description) 0)
                                             description
                                           "no description"))
                                       "\"")
                                      (format " #%s" (alist-get 'patch_number patch))
                                      )
                                     'evg-patch patch)
                                    )
                                   (newline))
                                 (seq-drop data start))
                                (read-only-mode)
                                )
                              )
                            )
                           )
  )
