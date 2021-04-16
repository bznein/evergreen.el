;;; -*- lexical-binding: t; -*-

(defun mdb/evg-get-project ()
  (interactive)
  (setq list_projects ())
  (mdb/evg-get-all-projects
   (lambda (projects)
     (seq-do
      (lambda (project)
        (push (assoc-default 'id project) list_projects)
        )
      projects
      )
     (setq project_name (completing-read "Project: " list_projects))
     (message project_name)
     (setq buffer_name (concat "Waterfall - " project_name))
     (switch-to-buffer (get-buffer-create buffer_name))
     (mdb/evg-get-project-versions
      project_name
      (lambda (versions)
        (with-current-buffer buffer_name
          (seq-do
           (lambda (version)
             (insert (alist-get 'message version))
             (newline)
             )
           versions
           )
          )
        )
      )
     )
   )
  )
