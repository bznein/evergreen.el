(setq json_evg (shell-command-to-string "yq eval -j /Users/nikolas.de-giorgis/go/src/github.com/10gen/ops-manager-kubernetes/.evergreen.yml"))
(setq dict (json-read-from-string json_evg))


;; build build_variants name lists
(setq variants())
(loop for bv across (assoc-default 'buildvariants dict) do
      (progn
        (push (assoc-default 'name bv) variants
        )
        )
      )
;;(message "%s" variants)


;; build task name lists
(setq tasks())
(loop for bv across (assoc-default 'tasks dict) do
      (progn
        (push (assoc-default 'name bv) tasks
              )
        )
      )
;;(message "%s" tasks)

;; build task_groups name lists
(setq task_groups())
(loop for bv across (assoc-default 'task_groups dict) do
      (progn
        (push (assoc-default 'name bv) task_groups
              )
        )
      )


(setq task (completing-read "Task " tasks  nil t ""))

;; get all bvs that contain this. Need to consider either direct task or groups!
;;(message "%s" task)
(setq matching_bvs ())
(loop for bv across (assoc-default 'buildvariants dict) do
      (progn
        (loop for task_name across (assoc-default 'tasks bv) do
              (progn
                (setq task_real_name  (assoc-default 'name task_name))
                (if (string= task_real_name task)
                    (push (assoc-default 'name bv) matching_bvs)
                  ;; Here I need to check if this name is a task group
                  (progn
                    (if (member task_real_name task_groups)
                        ;; TODO here loop through the task and see it `task' is here!
                        (message "%s is a task group" task_name );; then
                      (message "%s is not a task group" task_name );; else
                      )
                    )
                  )
                )
              )
        )
      )

(message "%s" matching_bvs)
