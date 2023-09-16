(defpackage :lem-desktop
  (:use :cl :lem :lem-base)
  (:export :desktop-restore
           :desktop-save))

(in-package :lem-desktop)

(defun buffer-info (buffer)
  (let* ((filename (lem:buffer-filename buffer))
         (point (lem:buffer-point buffer))
         (column (lem:point-charpos point))
         (line (lem:point-linum point)))
    (when (and filename point column line)
      (list :name filename :column column :line line))))

(defun save-buffers ()
  (let ((buffer-info-list
          (delete nil
                  (loop :for buffer :in (reverse (lem:buffer-list))
                        :collect (buffer-info buffer)))))
    (setf (lem:config :desktop) buffer-info-list)))

(lem:define-command desktop-save () ()
  (save-buffers))

(defun restore-buffer (plist)
  (alexandria:when-let*
      ((filename (getf plist :name))
       (column (getf plist :column))
       (line (getf plist :line)))
    (multiple-value-bind (buffer) (lem:find-file-buffer filename)
      (when (uiop/filesystem:file-exists-p filename)
        (lem:with-current-buffer buffer
          (lem:move-to-line (lem:current-point) line)
          (lem:move-to-column (lem:current-point) column)
          buffer)))))

(defun restore-buffers ()
  (loop :for plist :in (lem:config :desktop)
        :collect (restore-buffer plist)))

(lem:define-command desktop-restore () ()
  (restore-buffers))
                            