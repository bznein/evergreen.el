(defun mdb/create-patch ()
  (interactive)
  (setq api_key (getenv "EVG_API_KEY"))
  (setq api_user (getenv "EVG_API_USER"))
  (setq task_to_run (mdb/evg-get-task))
  (setq build_variants (mdb/evg-get-matching-build-variants-for-task task_to_run))
  (shell-command
   (concatenate 'string
                "evergreen patch"
                " -p " (replace-regexp-in-string "\n\\'" "" (mdb/get-current-repo))
                " -t " task_to_run
                " -v " (s-join " -v " build_variants)
                " -f -y -u -browse"
                )
   )
  )
