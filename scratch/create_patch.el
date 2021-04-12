(defun mdb/create-patch ()
  (interactive)
  (setq api_key (getenv "EVG_API_KEY"))
  (setq api_user (getenv "EVG_API_USER"))
  (setq tasks_to_run (mdb/evg-get-task))
  (setq tasks_for_command tasks_to_run)
  (setq build_variants (mdb/evg-get-matching-build-variants-for-task (pop tasks_to_run)))
  (loop for task_to_run in tasks_to_run do
        (setq build_variants (cl-intersection build_variants (mdb/evg-get-matching-build-variants-for-task task_to_run)))
        )
  (setq description_command "")
  (setq description (read-string "Description: "))
  (shell-command
   (concatenate 'string
                "evergreen patch"
                " -p " (replace-regexp-in-string "\n\\'" "" (mdb/get-current-repo))
                " -t " (s-join " -t " tasks_for_command)
                " -v " (s-join " -v " build_variants)
                " -d " description
                " -f -y -u -browse"
                )
   )
  )
