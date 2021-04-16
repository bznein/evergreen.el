;;; -*- lexical-binding: t; -*-
(provide 'evergreen_task)

(defun mdb/evg-show-task-at-point ()
  (interactive)
  (setq task (get-text-property (point) 'task))
  (if task
    (mdb/evg-show-task task)
    )
  )


(defun mdb/evg-show-task (task)
  (setq buffer_name (concat "Task " (alist-get 'display_name task)))
  (switch-to-buffer (get-buffer-create buffer_name))
  (with-current-buffer buffer_name
    (mdb-evg-task-mode)
    (read-only-mode 0)
    (erase-buffer)
    (insert (mdb/evg-task-header task))
    (newline)
    (newline)
    (setq task_called task)
    ;; TODO do this only if artifact is not empty
    (insert-button "Files" 'action (lambda(_) (mdb/evg-view-files task_called) ))
    (if (or
         (>
          (alist-get 'restarts task) 0
          )
         (>
          (alist-get 'execution task) 0
          )
         )
        (progn
          (newline)
          (newline)
          (insert-button "All executions" 'action (lambda(_) (mdb/evg-show-execution-summary task_called) )) ;;TODO call the actual one
          )
      )
    )
  )


(defun mdb/evg-task-header (task)
  (setq header "")
  (setq header (concat
                header
                "\nTask name: "
                (alist-get 'display_name task)
                "\nProject: "
                (alist-get 'project_id task)
                "\nBuild variant: "
                (alist-get 'build_variant task)
                "\nPriority: "
                (format "%s" (alist-get 'priority task))
                "\nStatus: "
                (alist-get 'display_status task)
                ))
  header
  )

(defun mdb/evg-show-execution-summary (task)
  (setq buffer_name (concat "Task " (alist-get 'display_name task) " - all executions"))
  (switch-to-buffer (get-buffer-create buffer_name))
  (with-current-buffer buffer_name
    (newline)
    (insert "NOT YET IMPLEMENTED")
    (insert (alist-get 'finish_time task))
    (newline)
    (mdb/evg-get-task (alist-get 'task_id task)
                      (lambda (returned_task)
                        (seq-do
                         (lambda (execution)
                           (insert (alist-get 'finish_time execution))
                           (newline)
                           )
                         (alist-get 'previous_executions returned_task)
                         )
                        )
                      )
    )
  )


(defun mdb/evg-open-task-at-point ()
  "Open the evg link to the patch"
  (interactive)
  (setq task (get-text-property (point) 'task))
  (if task
    (browse-url (concat "https://spruce.mongodb.com/task/" (alist-get 'task_id task)))
    )
  )

(defun mdb/evg-show-task-log-at-point ()
  "Open the evg link to the patch"
  (interactive)
  (setq task (get-text-property (point) 'task))
  (if task
    ;; TODO use directly the URL in the task
    (mdb/evg-get-logs-for-task (alist-get 'task_id task)
                               (lambda (logs)
                                 (switch-to-buffer (get-buffer-create "LOGS")) ;;TODO dynamic name
                                 (with-current-buffer "LOGS"
                                   (insert logs)
                                   (ansi-color-apply-on-region (point-min) (point-max))
                                   (read-only-mode)
                                   )
                                 )
                               )
    )
  )
