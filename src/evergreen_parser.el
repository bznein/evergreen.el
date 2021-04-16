;;; -*- lexical-binding: t; -*-
(provide 'evergreen_parser)

(defun mdb/evg-get-config-file ()
  (concat (replace-regexp-in-string "\n\\'" "" (mdb/get-current-repo-root-path)) "/.evergreen.yml")
  )

(defun mdb/evg-get-evergreen-dict ()
  (setq json_evg (shell-command-to-string (concat "yq eval -j " (mdb/evg-get-config-file))))
  (json-read-from-string json_evg)
  )

(defun mdb/evg-get-task-to-create ()
  (setq choices ())
  (defun fill-choices (candidate)
    (seq-do
     (lambda (cand)
       (push cand choices)
       )
     (helm-marked-candidates)
     )
    )
  (setq tasks (mdb/evg-parse-tasks))
  (helm :sources '(((name . "Tasks")
                    (candidates . tasks)
                    (action . (("open" . fill-choices)))
                    )))
  choices
  )

(defun mdb/evg-parse-tasks ()
  ;; build task name lists
  (setq dict (mdb/evg-get-evergreen-dict))
  (setq tasks())
  (seq-do (lambda (bv) (push (assoc-default 'name bv) tasks ) ) (assoc-default 'tasks dict))
  tasks
  )

(defun mdb/evg-validate-file ()
  (interactive)
  ;; Get the current buffer filename
  (if
      (not
       (or
         (s-suffix? ".yaml" buffer-file-name)
         (s-suffix? ".yml" buffer-file-name)
        )
       )
      (message  (shell-command-to-string(concat "evergreen validate --path " (read-file-name "Evergreen file"))))
    (message (shell-command-to-string(concat "evergreen validate --path " buffer-file-name )))
    )
  )

(defun mdb/evg-parse-build-variants ()
  ;; build build_variants name lists
  (setq dict (mdb/evg-get-evergreen-dict))
  (setq variants())
  (seq-do (lambda (bv) (push (assoc-default 'name bv) variants) ) (assoc-default 'build_variants dict))
  variants
  )



(defun mdb/evg-parse-task-groups ()
  ;; build task_groups name lists
  (setq task_groups())
  (seq-do (lambda (bv) (push (assoc-default 'name bv) task_groups) )(assoc-default 'task_groups dict) )
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
  (seq-do
   (lambda (bv)
     (progn
       (setq build_variant_name (assoc-default 'name bv))
       ;; For every build variant, loop the tasks
       (seq-do
        (lambda (task_name)
          (progn
            (setq task_real_name  (assoc-default 'name task_name))
            ;; If the task matches the one selected, add the build variant to the list
            (if (string= task_real_name task)
                (push build_variant_name matching_bvs)
              ;; Otherwise, see if this is a task group
              (progn
                (if (member task_real_name task_groups)
                    ;; If it is, loop inside it and search for the task selected
                    (seq-do
                     (lambda (task_group)
                       (if
                           (string=
                            (assoc-default 'name task_group)
                            task_real_name
                            )
                           ;; loop through its tasks
                           (seq-do
                            (lambda (task_in_task_group)
                              (if (string= task task_in_task_group)
                                  (push build_variant_name matching_bvs)
                                )
                              )
                            (assoc-default 'tasks task_group)
                            )
                         )
                      )
                     (assoc-default 'task_groups dict)
                     )
                  )
                )
              )
            )
          )
        (assoc-default 'tasks bv)
        )
       )
     )
   build_variants
   )
  matching_bvs
  )
