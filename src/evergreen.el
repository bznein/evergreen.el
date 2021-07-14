;;; -*- lexical-binding: t; -*-

(provide 'evergreen)

(require 'request)
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
