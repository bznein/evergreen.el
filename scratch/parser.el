(defun mdb/evg-get-config-file ()
  (concatenate 'string (replace-regexp-in-string "\n\\'" "" (mdb/get-current-repo-root-path)) "/.evergreen.yml")
  )

(defun mdb/evg-get-evergreen-dict ()
  (setq json_evg (shell-command-to-string (concatenate 'string "yq eval -j " (mdb/evg-get-config-file))))
  (json-read-from-string json_evg)
  )

(defun mdb/evg-get-task ()
  (completing-read "Task " (mdb/evg-parse-tasks)  nil t "")
  )

(defun mdb/evg-parse-tasks ()
  ;; build task name lists
  (setq dict (mdb/evg-get-evergreen-dict))
  (setq tasks())
  (loop for bv across (assoc-default 'tasks dict) do
        (progn
          (push (assoc-default 'name bv) tasks
                )
          )
        )
  tasks
  )



(defun mdb/evg-parse-build-variants ()
  ;; build build_variants name lists
  (setq dict (mdb/evg-get-evergreen-dict))
  (setq variants())
  (loop for bv across (assoc-default 'buildvariants dict) do
        (progn
          (push (assoc-default 'name bv) variants
                )
          )
        )
  variants
  )



(defun mdb/evg-parse-task-groups ()
  ;; build task_groups name lists
  (setq task_groups())
  (loop for bv across (assoc-default 'task_groups dict) do
        (progn
          (push (assoc-default 'name bv) task_groups
                )
          )
        )
  task_groups
  )


(defun mdb/get-current-repo ()
  (shell-command-to-string "basename `git rev-parse --show-toplevel`"))

;; get all bvs that contain this. Need to consider either direct task or groups!
;;(message "%s" task)
(defun mdb/evg-get-matching-build-variants-for-task (task)
  (setq dict (mdb/evg-get-evergreen-dict))
  (setq matching_bvs ())
  (setq build_variants (assoc-default 'buildvariants dict))
  (setq task_groups (mdb/evg-parse-task-groups))
  ;; Loop all build variants
  (loop for bv across build_variants do
        (progn
          (setq build_variant_name (assoc-default 'name bv))
          ;; For every build variant, loop the tasks
          (loop for task_name across (assoc-default 'tasks bv) do
                (progn
                  (setq task_real_name  (assoc-default 'name task_name))
                  ;; If the task matches the one selected, add the build variant to the list
                  (if (string= task_real_name task)
                      (push build_variant_name matching_bvs)
                    ;; Otherwise, see if this is a task group
                    (progn
                      (if (member task_real_name task_groups)
                          ;; If it is, loop inside it and search for the task selected
                          (loop for task_group across (assoc-default 'task_groups dict) do
                                (if
                                    (string=
                                     (assoc-default 'name task_group)
                                     task_real_name
                                     )
                                    ;; loop through its tasks
                                    (loop for task_in_task_group across (assoc-default 'tasks task_group) do
                                          (if (string= task task_in_task_group)
                                              (push build_variant_name matching_bvs)
                                            )
                                          )
                                  )
                                )
                        )
                      )
                    )
                  )
                )
          )
        )
  matching_bvs
  )
