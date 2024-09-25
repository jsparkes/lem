(defpackage :lem-copilot
  (:use :cl
        :lem
        :lem-copilot/utils)
  (:local-nicknames (:client :lem-copilot/client)
                    (:logger :lem-copilot/logger)))
(in-package :lem-copilot)

(define-attribute suggestion-attribute
  (t :foreground "dark gray"))

(define-attribute cycling-attribute
  (t :foreground "green"))

(defvar *client* nil)

(defun client ()
  *client*)

(defun copilot-root ()
  (merge-pathnames "copilot/" (lem-home)))

(defun copilot-path ()
  (merge-pathnames "lib/node_modules/copilot-node-server/copilot/dist/language-server.js"
                   (copilot-root)))

(defun installed-copilot-server-p ()
  (uiop:file-exists-p (copilot-path)))

(defun run-process ()
  (async-process:create-process (list "node"
                                      (namestring (copilot-path))
                                      "--stdio")))

(defun kill-process ()
  (when (client)
    (async-process:delete-process (client:client-process (client)))))

(add-hook *exit-editor-hook* 'kill-process)

(defun enable-copilot-p ()
  (config :copilot))

(defun enable-copilot ()
  (setf (config :copilot) t))


(define-command copilot-install-server () ()
  (let* ((buffer (make-buffer "*copilot-install-server*"))
         (command (list "npm"
                        "-g"
                        "--prefix"
                        (namestring (copilot:copilot-root))
                        "install"
                        "copilot-node-server@1.40.0")))
    (erase-buffer buffer)
    (pop-to-buffer buffer)
    (with-point ((point (buffer-point buffer) :left-inserting))
      (with-open-stream (output (make-editor-output-stream point))
        (format output "~{~A ~}~%" command)
        (redraw-display)
        (uiop:run-program command
                          :output output
                          :error-output output)))))

(defun installed-copilot-server-p ()
  (uiop:file-exists-p (copilot:copilot-path)))

(defun reset-buffers ()
  (dolist (buffer (remove-if-not #'copilot-mode-p (buffer-list)))
    (setf (buffer-version buffer) 0)
    (notify-text-document/did-open buffer)))

(define-command copilot-restart () ()
  (async-process:delete-process (lem-copilot/internal::agent-process lem-copilot::*agent*))
  (setf *agent* nil)
  (handler-case (copilot-login) (already-sign-in ()))
  (reset-buffers)
  (message "copilot restarted"))

(defmethod copilot:copilot-dead ()
  (display-popup-message (format nil
                                 "~{~A~^~%~}"
                                 '("Copilot has issued a warning. "
                                   "If it does not work properly, please execute `M-x copilot-restart`."
                                   ""
                                   "To view the copilot log, execute `M-x test/copilot-log`."))
                         :style '(:gravity :top)
                         :timeout 10)
  #+(or)
  (copilot-restart))


;;; login
(defun make-verification-buffer (user-code verification-uri)
  (let* ((buffer (make-buffer "*GitHub Copilot Verification*" :temporary t))
         (point (buffer-point buffer)))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (erase-buffer buffer)
    (insert-string point
                   (format nil
                           "Code: ~A (Copied to clipboard) ~%please paste it into your browser.~%~A~2%"
                           user-code
                           verification-uri))
    (insert-string point "Authenticate... (Close this window with Escape or C-g.)")
    (buffer-start point)
    buffer))

(defvar *login-message* nil)

(defun start-login (user-code verification-uri)
  (setf *login-message* (display-popup-message
                         (make-verification-buffer user-code verification-uri)
                         :style '(:gravity :center)
                         :timeout nil))
  (add-hook *editor-abort-hook* 'abort-login))

(defun abort-login ()
  (delete-login-message)
  (remove-hook *editor-abort-hook* 'abort-login))

(defun delete-login-message ()
  (when *login-message*
    (delete-popup-message *login-message*)
    (setf *login-message* nil)))

(define-command copilot-login () ()
  (unless (installed-copilot-server-p)
    (copilot-install-server))
  (setf *agent* nil)
  (let* ((agent (agent))
         (response (copilot:sign-in-initiate agent))
         (status (gethash "status" response))
         (user-code (gethash "userCode" response))
         (verification-uri (gethash "verificationUri" response))
         (user (gethash "user" response)))
    (when (equal status "AlreadySignedIn")
      (error 'already-sign-in :message (format nil "Already sign in as ~A" user)))
    (copy-to-clipboard user-code)
    (start-login user-code verification-uri)
    (open-external-file verification-uri)
    (redraw-display)
    (let ((finished nil))
      (copilot:sign-in-confirm
       agent
       user-code
       :callback (lambda (response)
                   (send-event (lambda ()
                                 (assert (equal "OK" (gethash "status" response)))
                                 (show-message (format nil "Authenticated as ~A" (gethash "user" response))
                                               :style '(:gravity :center))
                                 (delete-login-message)
                                 (setf finished t)
                                 (redraw-display)))))
      (handler-bind ((editor-abort (lambda (c)
                                     (declare (ignore c))
                                     (delete-login-message))))

        (loop :until finished
              :do (sit-for 1)))
      (enable-copilot))))


;;; utils
(defvar *language-id-map* (make-hash-table :test 'eq))

(defun register-language-id (mode language-id)
  (setf (gethash mode *language-id-map*) language-id))

(defun buffer-language-id (buffer)
  (gethash (buffer-major-mode buffer) *language-id-map* "text"))

(defun buffer-uri (buffer)
  (lem-lsp-mode::buffer-uri buffer))

(defun buffer-version (buffer)
  (buffer-value buffer 'version 0))

(defun (setf buffer-version) (version buffer)
  (setf (buffer-value buffer 'version) version))

(defun buffer-last-version (buffer)
  (buffer-value buffer 'last-version))

(defun (setf buffer-last-version) (last-version buffer)
  (setf (buffer-value buffer 'last-version) last-version))

(defun buffer-update-version-p (buffer)
  (not (equal (buffer-version buffer)
              (buffer-last-version buffer))))

(defun buffer-completions-cache (buffer)
  (buffer-value buffer 'completions-cache))

(defun (setf buffer-completions-cache) (completions-cache buffer)
  (setf (buffer-value buffer 'completions-cache) completions-cache))

(defun buffer-showing-suggestions-p (buffer)
  (buffer-value buffer 'showing-suggestions-p))

(defun (setf buffer-showing-suggestions-p) (showing-suggestions-p buffer)
  (setf (buffer-value buffer 'showing-suggestions-p) showing-suggestions-p))

(defun point-to-lsp-position (point)
  (hash "line" (1- (line-number-at-point point))
        "character" (point-charpos point)))

(defun move-to-lsp-position (point position)
  (move-to-line point (1+ (gethash "line" position)))
  (line-offset point 0 (gethash "character" position)))

(defun text-document-params (buffer)
  (list :uri (buffer-uri buffer)
        :language-id (buffer-language-id buffer)
        :version (buffer-version buffer)
        :text (buffer-text buffer)))

(defun notify-text-document/did-open (buffer)
  (apply #'client:text-document/did-open
         (client)
         (text-document-params buffer)))

(defun notify-text-document/did-close (buffer)
  (client:text-document/did-close (client) :uri (buffer-uri buffer)))

(defun notify-text-document/did-focus (buffer)
  (client:text-document/did-focus (client) :uri (buffer-uri buffer)))

(defun notify-text-document/did-change (buffer content-changes)
  (let ((version (incf (buffer-version buffer))))
    (client:text-document/did-change (client)
                                      :uri (buffer-uri buffer)
                                      :version version
                                      :content-changes content-changes)))

(defun initialize (client then)
  (client:initialize client
                     :callback (lambda (response)
                                 (send-event (lambda ()
                                               (funcall then response))))))

(defun set-editor-info (client then)
  (client:set-editor-info client
                          :callback (lambda (response)
                                      (send-event (lambda ()
                                                    (funcall then response))))))


;;; copilot-mode
(define-minor-mode copilot-mode
    (:name "Copilot"
     :keymap *copilot-mode-keymap*
     :enable-hook 'copilot-mode-on
     :disable-hook 'copilot-mode-off))

(define-key *copilot-mode-keymap* "M-n" 'copilot-next-suggestion)
(define-key *copilot-mode-keymap* "M-p" 'copilot-previous-suggestion)

(defun setup-client-async (then)
  (let ((client (client:run-client :process (run-process))))
    (client:connect client)
    (initialize client
                (lambda (response)
                  (declare (ignore response))
                  (client:initialized client)
                  (set-editor-info client
                                   (lambda (response)
                                     (declare (ignore response))
                                     (setf *client* client)
                                     (funcall then)))))))

(defun copilot-mode-on ()
  (unless (installed-copilot-server-p)
    (copilot-install-server)
    (reset-buffers))
  (unless *agent*
    (handler-case (copilot-login) (already-sign-in ())))
  (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer)) 'on-kill-buffer)
  (add-hook (variable-value 'before-change-functions :buffer (current-buffer)) 'on-before-change)
  (add-hook *window-show-buffer-functions* 'on-window-show-buffer)
  (add-hook *switch-to-window-hook* 'on-switch-to-window)
  (add-hook *post-command-hook* 'on-post-command)
  (notify-text-document/did-open (current-buffer)))

(defun copilot-mode-off ()
  (remove-hook (variable-value 'kill-buffer-hook :buffer (current-buffer)) 'on-kill-buffer)
  (remove-hook (variable-value 'before-change-functions :buffer (current-buffer)) 'on-before-change)
  (remove-hook *window-show-buffer-functions* 'on-window-show-buffer)
  (remove-hook *switch-to-window-hook* 'on-switch-to-window))

(defun copilot-mode-p (buffer)
  (mode-active-p buffer 'copilot-mode))

(defun on-kill-buffer (buffer)
  (when (copilot-mode-p buffer)
    (notify-text-document/did-close buffer)))

(defun on-post-command ()
  (notify-rejected))

(defun before-change-arg-to-content-change (point arg)
  (etypecase arg
    (string
     (let ((position (point-to-lsp-position point)))
       (hash "range" (hash "start" position
                           "end" position)
             "text" arg)))
    (integer
     (with-point ((end point))
       (character-offset end arg)
       (hash "range" (hash "start" (point-to-lsp-position point)
                           "end" (point-to-lsp-position end))
             "text" "")))))

(defvar *inhibit-did-change-notification* nil)

(defvar *inhibit-did-change-notification* nil)

(defun on-before-change (point arg)
  (let ((buffer (point-buffer point)))
    (when (and (copilot-mode-p buffer)
               (not *inhibit-did-change-notification*))
      (notify-text-document/did-change
       buffer
       (vector (before-change-arg-to-content-change point arg))))))

(defun on-window-show-buffer (window)
  (let ((buffer (window-buffer window)))
    (when (copilot-mode-p buffer)
      (notify-text-document/did-focus buffer))))

(defun on-switch-to-window (previous-window current-window)
  (declare (ignore previous-window))
  (let ((buffer (window-buffer current-window)))
    (when (copilot-mode-p buffer)
      (notify-text-document/did-focus buffer))))

(defvar *delay-complete* 100)
(defvar *complete-timer* nil)

(defmethod execute :after ((mode copilot-mode) (command self-insert) argument)
  (cond (*delay-complete*
         (if *complete-timer*
             (stop-timer *complete-timer*)
             (setf *complete-timer* (make-idle-timer 'copilot-complete :name "Copilot Complete")))
         (start-timer *complete-timer* *delay-complete* :repeat nil))
        (t
         (copilot-complete))))


;;; complete
(defvar *completion* nil)

(defvar *copilot-completion-keymap* (make-keymap :name "Copilot Completion"))

(define-key *copilot-completion-keymap* "Tab" 'copilot-accept-suggestion)
(define-key *copilot-completion-keymap* "M-n" 'copilot-next-suggestion)
(define-key *copilot-completion-keymap* "M-p" 'copilot-previous-suggestion)

(defun check-completions (completions)
  (when (= 1 (length completions))
    (editor-error "No more completions")))

(defun show-next-completion (completions &key (index 0))
  (check-completions completions)
  (show-and-apply-completion
   completions
   :index index
   :next (lambda ()
           (show-next-completion completions
                                 :index (mod (1+ index) (length completions))))))

(defun show-previous-completion (completions &key (index 0))
  (check-completions completions)
  (show-and-apply-completion
   completions
   :index index
   :next (lambda ()
           (show-previous-completion completions
                                     :index (mod (1- index) (length completions))))))

(defun cycle-completion (next-or-previous-function)
  (alexandria:if-let (response (buffer-completions-cache (current-buffer)))
    (funcall next-or-previous-function (gethash "completions" response))
    (copilot:get-completions-cycling
     (agent)
     :doc (make-doc (current-point))
     :callback (lambda (response)
                 (send-event (lambda ()
                               (setf (buffer-completions-cache (current-buffer)) response)
                               (funcall next-or-previous-function
                                        (gethash "completions" response) :index 1)))))))

(defun next-completion ()
  (cycle-completion #'show-next-completion))

(defun previous-completion ()
  (cycle-completion #'show-previous-completion))

(defun replace-with-completion (point completion)
  (let* ((range (gethash "range" completion)))
    (with-point ((start point)
                 (end point))
      (move-to-lsp-position start (gethash "start" range))
      (move-to-lsp-position end (gethash "end" range))
      (delete-between-points start end)
      (insert-string start (gethash "text" completion)))))

(defun show-completion (display-text)
  (let ((*inhibit-did-change-notification* t))
    (lem-lsp-mode::reset-buffer-diagnostic (current-buffer))
    (unwind-protect
         (progn
           (buffer-undo-boundary (current-buffer))
           (save-excursion
             (insert-string (current-point)
                            display-text
                            :attribute 'copilot-suggestion-attribute))
           (loop :for v := (sit-for 10)
                 :while (eq v :timeout)
                 :finally (return-from show-completion v)))
      (buffer-undo (current-point)))))

(defun find-copilot-completion-command (key)
  (lookup-keybind key
                  :keymaps (append (lem-core::all-keymaps)
                                   (list *copilot-completion-keymap*))))

(defun show-and-apply-completion (completions &key (index 0)
                                                   (next #'next-completion)
                                                   (previous #'previous-completion))
  (let ((completion (elt completions index)))
    (setf *completion* completion)
    (copilot:notify-shown (agent) (gethash "uuid" completion))
    (let ((key (show-completion (gethash "displayText" completion))))
      (case (find-copilot-completion-command key)
        (copilot-accept-suggestion
         (read-key)
         (unshow-inline-completion point)
         (buffer-undo-boundary buffer)
         (replace-with-inline-completion point (elt-safety items index))
         (redraw-display))
        (copilot-next-suggestion
         (read-key)
         (inline-completion point
                            :trigger-kind client:+trigger-kind.invoked+
                            :index (mod (1+ index) (length items))
                            :cycling t
                            :show-loading-spinner t))
        (copilot-previous-suggestion
         (read-key)
         (inline-completion point
                            :trigger-kind client:+trigger-kind.invoked+
                            :index (mod (1- index) (length items))
                            :cycling t
                            :show-loading-spinner t))
        (self-insert
         (unshow-inline-completion point)
         (buffer-undo-boundary buffer)
         (self-insert 1 (insertion-key-p (read-key)))
         (inline-completion point))
        (otherwise
         (error 'editor-abort :message nil))))))

(defun inline-completion (point &key (trigger-kind 2) (index 0) cycling show-loading-spinner)
  (setf *completion-canceled* nil)
  (let* ((buffer (point-buffer point))
         (spinner (when show-loading-spinner
                    (lem/loading-spinner:start-loading-spinner :line :point point)))
         (request
           (client:text-document/inline-completion
            (client)
            :callback (lambda (response)
                        (send-event (lambda ()
                                      (when spinner
                                        (lem/loading-spinner:stop-loading-spinner spinner))
                                      (unshow-inline-completion point)
                                      (unless *completion-canceled*
                                        (prompt-inline-completion (buffer-point buffer)
                                                                  (gethash "items" response)
                                                                  :index index
                                                                  :cycling cycling)))))
            :error-callback (lambda (&rest args)
                              (declare (ignore args))
                              (unshow-inline-completion point)
                              (send-event (lambda ()
                                            (when spinner
                                              (lem/loading-spinner:stop-loading-spinner spinner)))))
            :uri (buffer-uri buffer)
            :position (point-to-lsp-position point)
            :insert-spaces (if (variable-value 'indent-tabs-mode
                                               :default buffer)
                               'yason:true
                               'yason:false)
            :tab-size (variable-value 'tab-width :default buffer)
            :trigger-kind trigger-kind)))
    (setf *inline-completion-request* request)))

(defun cancel-inline-completion ()
  (unshow-inline-completion (current-point))
  (when *inline-completion-request*
    (client:$/cancel-request (client) (jsonrpc:request-id *inline-completion-request*))
    (setf *inline-completion-request* nil
          *completion-canceled* t)))

(defun notify-rejected ()
  (when *completion*
    (copilot:notify-rejected (agent) (gethash "uuid" *completion*))
    (setf *completion* nil)))

(define-command copilot-complete () ()
  (inline-completion (current-point)))

(define-command copilot-accept-suggestion () ()
  ;; dummy command
  )

(define-command copilot-next-suggestion () ()
  ;; dummy command
  )

(define-command copilot-previous-suggestion () ()
  ;; dummy command
  )

(defparameter *delay-complete* 1)
(defvar *complete-timer* nil)

(defmethod execute :after ((mode copilot-mode) (command self-insert) argument)
  (cond (*delay-complete*
         (if *complete-timer*
             (stop-timer *complete-timer*)
             (setf *complete-timer* (make-idle-timer 'copilot-complete :name "Copilot Complete")))
         (start-timer *complete-timer* *delay-complete* :repeat nil))
        (t
         (copilot-complete))))
