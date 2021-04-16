;;; -*- lexical-binding: t; -*-


(provide 'evergreen_patch)


(require 'evergreen_task)


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

    (seq-do
     (lambda (code_change)
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
           (seq-do
            (lambda (file)
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
            (alist-get 'file_diffs code_change)
            )
           (goto-char file_top_point)
           (insert (propertize (concat "\t+" (number-to-string tot_add)) 'font-lock-face '(:foreground "green") ))
           (insert (propertize (concat " -" (number-to-string tot_del)) 'font-lock-face '(:foreground "red") ))
       )
     (alist-get 'module_code_changes patch)
     )
    (read-only-mode)
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


(defun mdb/evg-show-patch-at-point ()
  (interactive)
  (setq patch (get-text-property (point) 'evg-patch))
  (if patch
    (mdb/evg-show-patch patch)
    )
  )


(defun mdb/evg-patch-show-tasks (patch buffer_name)
  (with-current-buffer buffer_name
    (insert "Build Variants")
    (newline)
    (newline)
    (mdb/evg-get-version (alist-get 'version patch) (cl-function
                                                     (lambda (&key data &allow-other-keys)
                                                       (seq-do
                                                        (lambda (bv)
                                                             (progn
                                                               (insert (assoc-default 'build_variant bv))
                                                               (newline)
                                                               (mdb/evg-get-tasks-for-build-variant
                                                                (assoc-default 'build_id bv)
                                                                (cl-function
                                                                 (lambda
                                                                   (&key data &allow-other-keys)
                                                                   (seq-do
                                                                    (lambda (task)
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
                                                                    data
                                                                    )
                                                                   )
                                                                 )
                                                                )
                                                               (newline)
                                                               (newline)
                                                               (newline)
                                                               )
                                                             )
                                                        (assoc-default 'build_variants_status data)
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
       (seq-do
        (lambda (line)
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
        splitted_string
        )
       )
     )
    )
  )

;;TODO this assumes text files
(defun mdb/evg-show-file-at-point ()
  (interactive)
  (setq name (get-text-property (point) 'name))
  (setq url (get-text-property (point) 'url))
  (if (and url name)
      (progn
        (setq buffer_name (concat "File: " name))
        (switch-to-buffer (get-buffer-create buffer_name))
        (mdb/get-text-file url
                           (lambda (file)
                             (insert file)
                        )
                           )
        )
    )
  )

(defun mdb/evg-view-files (task)
   (setq buffer_name (concat "Task " (alist-get 'display_name task) " - Files"))
   (switch-to-buffer (get-buffer-create buffer_name))
   (with-current-buffer buffer_name
     (mdb-evg-file-mode)
     (seq-do
      (lambda (artifact)
        (newline)
        (insert (propertize
                 (assoc-default 'name artifact)
                 'url (assoc-default 'url artifact)
                 'name (assoc-default 'name artifact)
                 )
                )
        )
      (alist-get 'artifacts task)
      )
     )
   )

(defun mdb/evg-open-patch-at-point ()
  "Open the evg link to the patch"
  (interactive)
  (setq patch (get-text-property (point) 'evg-patch))

  (if patch
    (shell-command (message (concat "open https://spruce.mongodb.com/version/" (alist-get 'patch_id patch) )))
    )
  )
