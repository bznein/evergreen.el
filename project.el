(defun mdb/evg-get-project ()
  (interactive)
  (setq list_projects ())
  (mdb/evg-get-all-projects
   (lambda (projects)
     (loop for project across projects do
           (push (assoc-default 'id project) list_projects)
           )
     (setq project_name (completing-read "Project: " list_projects))
     (message project_name)
     (setq buffer_name (concat "Waterfall - " project_name))
     (switch-to-buffer (get-buffer-create buffer_name))
     (mdb/evg-get-project-versions
      project_name
      (lambda (versions)
        (with-current-buffer buffer_name
          (loop for version across versions do
                (insert (alist-get 'message version))
                (newline)
                )
          )
        )
      )
     )
   )
  )
