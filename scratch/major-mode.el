(require 'ansi-color)
(defvar mdb-evg-mode-map nil "Keymap for evg-status page")
(defvar mdb-evg-patch-mode-map nil "keymap for evg-patch-page")
(defvar mdb-evg-task-mode-map nil "keymap for evg-task-page")
(defvar mdb-evg-file-mode-map nil "keymap for evg-task-page")

(progn
  (setq mdb-evg-mode-map (make-sparse-keymap))
  (setq mdb-evg-patch-mode-map (make-sparse-keymap))
  (setq mdb-evg-task-mode-map (make-sparse-keymap))
  (setq mdb-evg-file-mode-map (make-sparse-keymap))


  (define-key mdb-evg-mode-map (kbd "b") 'mdb/evg-show-all-patches)
  (define-key mdb-evg-mode-map (kbd "o") 'mdb/evg-open-patch-at-point)
  (define-key mdb-evg-mode-map (kbd "n") 'mdb/evg-next-patches)
  (define-key mdb-evg-mode-map (kbd "p") 'mdb/evg-prev-patches)
  (define-key mdb-evg-mode-map (kbd "q") 'kill-this-buffer)
  (define-key mdb-evg-mode-map (kbd "<RET>") 'mdb/evg-show-patch-at-point)


  (define-key mdb-evg-patch-mode-map (kbd "o") 'mdb/evg-open-task-at-point)
  (define-key mdb-evg-patch-mode-map (kbd "l") 'mdb/evg-show-task-log-at-point)
  (define-key mdb-evg-patch-mode-map (kbd "q") 'kill-this-buffer)
  (define-key mdb-evg-patch-mode-map (kbd "<RET>") 'mdb/evg-show-task-at-point)


  (define-key mdb-evg-task-mode-map (kbd "q") 'kill-this-buffer)

  (define-key mdb-evg-file-mode-map (kbd "<RET>") 'mdb/evg-show-file-at-point)
  (define-key mdb-evg-file-mode-map (kbd "q") 'kill-this-buffer)
  )

(define-derived-mode
  mdb-evg-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-status page")


(define-derived-mode
  mdb-evg-patch-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-patch page")

(define-derived-mode
  mdb-evg-task-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-task page")

(define-derived-mode
  mdb-evg-file-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-file page")


(cl-defstruct evg-patch patch_id description patch_number status author create-time start-time finish-time variants_tasks version)

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

(defun mdb/evg-open-patch-at-point ()
  "Open the evg link to the patch"
  (interactive)
  (when-let ((patch (get-text-property (point) 'evg-patch)))
    (shell-command (message (concat "open https://spruce.mongodb.com/version/" (alist-get 'patch_id patch) )))
    )
  )

(defun mdb/evg-open-task-at-point ()
  "Open the evg link to the patch"
  (interactive)
  (when-let ((task (get-text-property (point) 'task)))
    (browse-url (concat "https://spruce.mongodb.com/task/" (alist-get 'task_id task)))
    )
  )

(defun mdb/evg-show-task-log-at-point ()
  "Open the evg link to the patch"
  (interactive)
  (when-let ((task (get-text-property (point) 'task)))
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

(defun mdb/evg-show-patch-at-point ()
  (interactive)
  (when-let ((patch (get-text-property (point) 'evg-patch)))
    (mdb/evg-show-patch patch)
    )
  )


(defun mdb/evg-patch-header (patch)
  (setq header "")
  (setq header (concat header "Patch: " (alist-get 'patch_id patch)))
  (setq header (concat header "\n" "Status: " (alist-get 'status patch)))
  (setq header (concat header "\n" "Description: " (alist-get 'description patch)))
  (setq header (concat header "\n" "Author: " (alist-get 'author patch)))
  header
  )



(defun mdb/evg-patch-show-tasks (patch buffer_name)
  (with-current-buffer buffer_name
    (insert "Build Variants")
    (newline)
    (newline)
    (mdb/evg-get-version (alist-get 'version patch) (cl-function
                                                     (lambda (&key data &allow-other-keys)
                                                       (loop for bv across (assoc-default 'build_variants_status data) do
                                                             (progn
                                                               (insert (assoc-default 'build_variant bv))
                                                               (newline)
                                                               (mdb/evg-get-tasks-for-build-variant
                                                                (assoc-default 'build_id bv)
                                                                (cl-function
                                                                 (lambda
                                                                   (&key data &allow-other-keys)
                                                                   (loop for task across data do
                                                                         (progn
                                                                           (insert
                                                                            (propertize
                                                                             (concat
                                                                              (assoc-default 'display_name task)
                                                                              " - "
                                                                              (assoc-default 'status task)
                                                                              )
                                                                             'task task
                                                                             )
                                                                            )
                                                                           (newline)
                                                                           )
                                                                         )
                                                                   )
                                                                 )
                                                                )
                                                               (newline)
                                                               (newline)
                                                               (newline)
                                                               )
                                                             )
                                                       )
                                                     )
                         )
    )
  )

(defun mdb/evg-show-diff (raw_url file_name)
  (interactive)
  (setq buffer_name file_name)
  (switch-to-buffer (get-buffer-create buffer_name))

  ;;TODO replace with a real one
  (mdb-evg-patch-mode)
  (with-current-buffer buffer_name
    (setq name file_name)
    (mdb/get-text-file
     raw_url
     (lambda (file)
       (setq found nil)
       (setq splitted_string (split-string file "\n"))
       (loop for line being the elements of splitted_string do
             (if found
                 (progn
                   (if (string-prefix-p "diff --git a/" line)
                       (setq found nil)
                     (progn
                       (if (or
                            (string-prefix-p "+++" line)
                            (string-prefix-p "---" line)
                            )
                           (insert (propertize line 'font-lock-face '(:weight bold)))
                         (if (string-prefix-p "+ " line)
                             (progn
                               (message "Starts with +")
                               (insert (propertize line 'font-lock-face '(:foreground "green")))
                               )
                           (if (string-prefix-p "- " line)
                               (progn
                                 (message "Starts with - ")
                                 (insert (propertize line 'font-lock-face '(:foreground "red") ))
                                 )
                             (insert line)
                             )
                           )
                         )
                       (newline)
                       )
                     )
                   )
               (if (string-prefix-p (concat "diff --git a/" name) line)
                   (progn
                     (insert line)
                     (newline)
                     (setq found 't)
                     )
                 )
               )
             )
       )
     )
    )
  )


(defun mdb/evg-show-patch (patch)
  (setq buffer_name (concat "Patch " (alist-get 'patch_id patch)))
  (switch-to-buffer (get-buffer-create buffer_name))
  (with-current-buffer buffer_name
    (mdb-evg-patch-mode)
    (read-only-mode 0)
    (erase-buffer)
    (insert (mdb/evg-patch-header patch))
    (newline)
    (newline)
    (mdb/evg-patch-show-tasks patch buffer_name)
    (newline)

    (loop for code_change across (alist-get 'module_code_changes  patch) do
          (newline)
           (insert
            (concat "Changes for branch " (alist-get 'branch_name code_change))
            )
           (newline)
           (setq html_url (alist-get 'html_link code_change))
           (setq raw_url (alist-get 'raw_link code_change))
           (insert-button "HTML" 'action (lambda (_) (browse-url html_url)))
           (newline)
           (insert-button "RAW" 'action (lambda (_) (browse-url raw_url)))
           (newline)
           (newline)
           (insert "Files changed:")
           (setq file_top_point (point))
           (newline)
           (newline)
           (setq tot_add 0)
           (setq tot_del 0)
           (loop for file across (alist-get 'file_diffs code_change) do
                 (setq add (alist-get 'additions file))
                 (setq del (alist-get 'deletions file))
                 (newline)
                 (setq raw_diff raw_url)
                 (insert-button (alist-get 'file_name file)
                                'name (alist-get 'file_name file)
                                'action (lambda (b)
                                          (mdb/evg-show-diff raw_diff (button-get b 'name))
                                          )
                                )
                 (insert
                  (propertize (concat "\t+" (number-to-string add)) 'font-lock-face '(:foreground "green") ))
                 (insert
                  (propertize (concat " -" (number-to-string del)) 'font-lock-face '(:foreground "red") ))
                 (setq tot_add (+ add tot_add))
                 (setq tot_del (+ del tot_del))
                 )
           (goto-char file_top_point)
           (insert (propertize (concat "\t+" (number-to-string tot_add)) 'font-lock-face '(:foreground "green") ))
           (insert (propertize (concat " -" (number-to-string tot_del)) 'font-lock-face '(:foreground "red") ))
           )
    (read-only-mode)
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
    (insert (alist-get 'finish_time task))
    (newline)
    (mdb/evg-get-task (alist-get 'task_id task)
                      (lambda (returned_task)
                        (loop for execution across (alist-get 'previous_executions returned_task) do
                              (insert (alist-get 'finish_time execution))
                              (newline)
                              )
                        )
                      )
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

;;TODO this assumes text files
(defun mdb/evg-show-file-at-point ()
  (interactive)
  (when-let ((name (get-text-property (point) 'name)))
    (setq buffer_name (concat "File: " name))
    )
  (when-let ((url (get-text-property (point) 'url)))
    (switch-to-buffer (get-buffer-create buffer_name))
    (mdb/get-text-file url
                       (lambda (file)
                         (insert file)
                        )
                       )
    )
  )

(defun mdb/evg-view-files (task)
   (setq buffer_name (concat "Task " (alist-get 'display_name task) " - Files"))
   (switch-to-buffer (get-buffer-create buffer_name))
   (with-current-buffer buffer_name
     (mdb-evg-file-mode)
     (loop for artifact across (assoc-default 'artifacts task) do
           (newline)
           (insert (propertize
                    (assoc-default 'name artifact)
                    'url (assoc-default 'url artifact)
                    'name (assoc-default 'name artifact)
                    )
                   )
           )
     )
   )

(defun mdb/evg-show-task-at-point ()
   (interactive)
   (when-let ((task (get-text-property (point) 'task)))
     (mdb/evg-show-task task)
    )
  )


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
