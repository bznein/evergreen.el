;;; -*- lexical-binding: t; -*-
(defun mdb/create-patch ()
  (interactive)
  (mdb/set-credentials)
  (setq api_key (getenv "EVG_API_KEY"))
  (setq api_user (getenv "EVG_API_USER"))
  (setq tasks_to_run (mdb/evg-get-task-to-create))
  (setq tasks_for_command tasks_to_run)
  (setq build_variants (mdb/evg-get-matching-build-variants-for-task (pop tasks_to_run)))
  (seq-do
   (lambda (task_to_run)
     (setq build_variants (cl-union build_variants (mdb/evg-get-matching-build-variants-for-task task_to_run)))
     )
   tasks_to_run
   )
  (if (eq build_variants nil)
      (progn
        (message "The selected tasks do not share build variants")
        )
    (progn
      (setq build_variants_for_command ())
      (setq candidate_variants (delete-dups build_variants))
      (defun fill-choices (candidate)
        (seq-do
         (lambda (cand)
           (push cand build_variants_for_command)
           )
         (helm-marked-candidates)
         )
        )
      (helm :sources '(((name . "Build Variants")
                        (candidates . candidate_variants)
                        (action . (("open" . fill-choices)))
                        )))
      choices
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
                    " -t " (s-join " -t " tasks_for_command)
                    " -v " (s-join " -v " build_variants_for_command)
                    description_command
                    " -f -y -u -browse"
                    )
       )
      (if
          (not (= (length priority) 0))
          (mdb/bump-priority  priority)
        )
      )
    )
  )
