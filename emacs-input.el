;;; emacs-input-fixed.el --- Minimal system-wide popup Emacs for quick edits -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: emacs-input
;; Version: 0.1.0
;; Keywords: convenience, frames
;; Package-Requires: ((emacs "26.3"))

;;; Commentary:

;; A minimal and efficient system-wide popup Emacs for quick text editing.
;; Optimized for performance with pre-created frames and Hammerspoon integration.

;;; Code:

(require 'server)

(defgroup emacs-input ()
  "Minimal system-wide popup Emacs for quick edits."
  :group 'convenience)

;;; Customization

(defcustom emacs-input-frame-parameters
  '((name . "emacs-input")
    (width . 80)
    (height . 20)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil))
  "Frame parameters for emacs-input frame."
  :type 'alist
  :group 'emacs-input)

(defcustom emacs-input-hammerspoon-script
  "~/.hammerspoon/emacs-input.lua"
  "Path to the Hammerspoon script for system integration."
  :type 'string
  :group 'emacs-input)

(defcustom emacs-input-auto-trigger-on-focus t
  "Whether to automatically trigger emacs-input when input fields gain focus."
  :type 'boolean
  :group 'emacs-input)

(defcustom emacs-input-auto-trigger-delay 0
  "Delay in seconds before auto-triggering emacs-input on focus."
  :type 'number
  :group 'emacs-input)

(defcustom emacs-input-excluded-apps
  '("Emacs")
  "List of application names to exclude from auto-trigger."
  :type '(repeat string)
  :group 'emacs-input)

;;; Internal variables

(defvar-local emacs-input-current-app nil
  "Current application information.")

(defvar-local emacs-input--original-content ""
  "Original content before editing.")

(defvar emacs-input--hs-command nil
  "Path to the hs command.")

(defvar emacs-input--app-info nil
  "Current application information.")

(defvar emacs-input--frame nil
  "Pre-created frame for emacs-input.")

(defvar emacs-input--app-info-cache nil
  "Cached application information.")

(defvar emacs-input--cache-time 0
  "Time when app info was last cached.")

(defvar emacs-input--buffer nil
  "Pre-created buffer for emacs-input.")

;;; Core functionality

(defun emacs-input--find-hs-command ()
  "Find the hs command path with caching."
  (unless emacs-input--hs-command
    (setq emacs-input--hs-command
          (or (executable-find "hs")
              ;; Try nix-darwin path
              (let ((nix-hs "/nix/store/2l8mcmysihrdbs85hv53ymhl1mh03kqs-hammerspoon-1.0.0/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"))
                (when (file-executable-p nix-hs) nix-hs))
              ;; Try standard macOS path
              (let ((mac-hs "/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs"))
                (when (file-executable-p mac-hs) mac-hs)))))
  emacs-input--hs-command)

;;; Auto-trigger functionality

(defun emacs-input-configure-auto-trigger ()
  "Configure auto-trigger settings in Hammerspoon."
  (when (and (file-exists-p emacs-input-hammerspoon-script)
             (emacs-input--find-hs-command))
    (let ((config-script (format "
local emacs_input = require('emacs-input')
local config = emacs_input.config or {}
config.auto_trigger_on_focus = %s
config.auto_trigger_delay = %s
config.excluded_apps = %s
emacs_input.config = config
"
                                 (if emacs-input-auto-trigger-on-focus "true" "false")
                                 emacs-input-auto-trigger-delay
                                 (format "{%s}"
                                         (mapconcat (lambda (app) (format "\"%s\"" app))
                                                   emacs-input-excluded-apps ", ")))))
      (call-process (emacs-input--find-hs-command) nil nil nil "-c" config-script))))

(defun emacs-input-reset-suppression ()
  "Reset auto-trigger suppression (allow immediate auto-trigger)."
  (interactive)
  (when (and (file-exists-p emacs-input-hammerspoon-script)
             (emacs-input--find-hs-command))
    (call-process (emacs-input--find-hs-command) nil nil nil "-c"
                  "require('emacs-input').resetSuppression()")
    (message "emacs-input auto-trigger suppression reset")))

;;; Frame and buffer management

(defun emacs-input--create-frame ()
  "Create and hide emacs-input frame."
  (unless (and emacs-input--frame (frame-live-p emacs-input--frame))
    (setq emacs-input--frame
          (make-frame (append emacs-input-frame-parameters
                             '((visibility . nil))))))  ; Start hidden
  emacs-input--frame)

(defun emacs-input--create-buffer ()
  "Create or reuse emacs-input buffer."
  (unless (and emacs-input--buffer (buffer-live-p emacs-input--buffer))
    (setq emacs-input--buffer (generate-new-buffer "*emacs-input*")))
  emacs-input--buffer)

(defun emacs-input--prepare-buffer ()
  "Prepare the emacs-input buffer for editing."
  (let ((buffer (emacs-input--create-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (setq emacs-input--original-content "")
      (text-mode)
      (emacs-input-mode 1)
      (setq-local emacs-input-current-app emacs-input--app-info-cache))
    buffer))

(defun emacs-input--show-frame ()
  "Show and focus the emacs-input frame."
  (let ((frame (emacs-input--create-frame))
        (buffer (emacs-input--prepare-buffer)))
    (select-frame frame)
    (switch-to-buffer buffer)
    (make-frame-visible frame)
    (raise-frame frame)
    ;; Focus frame (compatible with different Emacs versions)
    (when (fboundp 'focus-frame)
      (focus-frame frame))
    ;; Position cursor at end of buffer
    (goto-char (point-max))
    (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort")))

(defun emacs-input--show-frame-safe ()
  "Safely show frame, handling terminal vs GUI context."
  (condition-case err
      (if (display-graphic-p)
          (emacs-input--show-frame)
        ;; Fallback for terminal mode - use current frame
        (let ((buffer (emacs-input--prepare-buffer)))
          (switch-to-buffer buffer)
          (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort")))
    (error
     (message "Error showing emacs-input frame: %s" err)
     ;; Fallback to simple buffer
     (let ((buffer (emacs-input--prepare-buffer)))
       (switch-to-buffer buffer)
       (message "emacs-input ready (fallback mode) - Press C-c C-c to finish, C-c C-k to abort")))))

(defun emacs-input--get-app-info-async ()
  "Asynchronously get application information via Hammerspoon."
  (when (and (file-exists-p emacs-input-hammerspoon-script)
             (emacs-input--find-hs-command))
    (let ((process (start-process "emacs-input-app-info" nil
                                 (emacs-input--find-hs-command) "-c" "require('emacs-input').getAppInfo()")))
      (set-process-sentinel process #'emacs-input--app-info-sentinel))))

(defun emacs-input--app-info-sentinel (process event)
  "Handle app info process completion."
  (when (and event (string-match "finished" event))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (let ((info (string-trim (buffer-string))))
          (when (> (length info) 0)
            (condition-case err
                (progn
                  (setq emacs-input--app-info (read info))
                  ;; Try to get selected text if available
                  (emacs-input--get-selection-async))
              (error
               (message "Error processing app info: %s" err)))))))))

(defun emacs-input--get-selection-async ()
  "Asynchronously get selected text via Hammerspoon."
  (when (and (file-exists-p emacs-input-hammerspoon-script)
             (emacs-input--find-hs-command))
    (let ((process (start-process "emacs-input-selection" nil
                                 (emacs-input--find-hs-command) "-c" "require('emacs-input').getSelection()")))
      (set-process-sentinel process #'emacs-input--selection-sentinel))))

(defun emacs-input--selection-sentinel (process event)
  "Handle selection process completion."
  (when (and event (string-match "finished" event))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (let ((selection (string-trim (buffer-string))))
          (when (> (length selection) 0)
            (condition-case err
                (progn
                  ;; Insert selection into current buffer if it's empty
                  (when (string= (buffer-string) "")
                    (insert selection)
                    (setq emacs-input--original-content selection)))
              (error
               (message "Error processing selection: %s" err)))))))))

;;; Helper functions

(defun emacs-input--get-app-info ()
  "Get current application info synchronously."
  (when (emacs-input--find-hs-command)
    (let ((output (shell-command-to-string
                   (format "%s -c \"require('emacs-input').getAppInfo()\" 2>/dev/null"
                           (emacs-input--find-hs-command)))))
      (when (and (> (length (string-trim output)) 0)
                 (not (string-match-p "error:" output)))
        (condition-case nil
            (read output)
          (error nil))))))

(defun emacs-input--get-app-info-cached ()
  "Get app info with caching to avoid repeated calls."
  (let ((now (float-time)))
    (when (or (null emacs-input--app-info-cache)
              (> (- now emacs-input--cache-time) 2.0))  ; 2 second cache
      (setq emacs-input--app-info-cache (emacs-input--get-app-info)
            emacs-input--cache-time now))
    emacs-input--app-info-cache))

(defun emacs-input--create-temp-file (app-info)
  "Create a temporary file for editing."
  (let* ((temp-name (format "emacs-input-%s-%s"
                           (format-time-string "%Y%m%d-%H%M%S")
                           (or (and app-info (plist-get app-info :name)) "unknown")))
         (temp-file (expand-file-name temp-name temporary-file-directory)))
    temp-file))

(defun emacs-input--command-params (app-info temp-file)
  "Generate emacsclient command parameters."
  (delq nil
        (list
         (when (and (server-running-p) server-use-tcp)
           (concat "--server-file="
                   (shell-quote-argument
                    (expand-file-name server-name server-auth-dir))))
         (when (and (server-running-p) (not server-use-tcp))
           (concat "--socket-name="
                   (shell-quote-argument
                    (expand-file-name server-name server-socket-dir))))
         "-c" "-F"
         (prin1-to-string
          (cons (cons 'emacs-input-app app-info)
                emacs-input-frame-parameters))
         temp-file)))

;;; Public API

;;;###autoload
(defun emacs-input (&optional file)
  "Launch emacs-input frame from emacsclient.
This may open FILE if specified, otherwise creates a temporary file."
  (let* ((app-info (emacs-input--get-app-info))
         (temp-file (or file (emacs-input--create-temp-file app-info)))
         (params (emacs-input--command-params app-info temp-file)))
    (apply #'call-process "emacsclient" nil 0 nil params)))

;;;###autoload
(defun emacs-input-fast ()
  "Fast emacs-input using optimized approach (recommended)."
  (interactive)
  ;; Use cached app info to avoid blocking
  (let* ((app-info (emacs-input--get-app-info-cached))
         (temp-file (emacs-input--create-temp-file app-info))
         (params (emacs-input--command-params app-info temp-file)))
    ;; Start async app info update in background
    (emacs-input--get-app-info-async)
    ;; Launch emacsclient with frame parameters (like original emacs-input)
    (apply #'call-process "emacsclient" nil 0 nil params)
    "emacs-input-fast launched successfully"))

;;;###autoload
(defun emacs-input-instant ()
  "Instant emacs-input using current frame (for internal use)."
  (interactive)
  ;; If called from within Emacs, use the current frame
  (let ((current-frame (selected-frame))
        (buffer (emacs-input--create-buffer)))
    ;; Configure current frame for emacs-input
    (modify-frame-parameters current-frame emacs-input-frame-parameters)
    ;; Prepare buffer
    (with-current-buffer buffer
      (erase-buffer)
      (setq emacs-input--original-content "")
      (text-mode)
      (emacs-input-mode 1))
    ;; Switch to emacs-input buffer
    (switch-to-buffer buffer)
    ;; Cache app info asynchronously, don't block UI
    (emacs-input--get-app-info-async)
    ;; Get selection asynchronously after frame is shown
    (run-with-timer 0.1 nil #'emacs-input--get-selection-async)
    ;; Position cursor at end of buffer
    (goto-char (point-max))
    (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort")
    ;; Return a friendly message instead of timer object
    "emacs-input-instant launched successfully"))

;;;###autoload
(defun emacs-input-quick ()
  "Quick emacs-input without creating new process (for internal use)."
  (interactive)
  (let* ((app-info (emacs-input--get-app-info-cached))
         (temp-file (emacs-input--create-temp-file app-info)))
    (find-file temp-file)
    (setq-local emacs-input-current-app app-info)
    (text-mode)
    (emacs-input-mode 1)  ; Enable emacs-input-mode AFTER text-mode
    (setq emacs-input--original-content "")
    (emacs-input--get-selection-async)
    (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort")))

;;;###autoload
(defun emacs-input-initialize-frame ()
  "Initialize emacs-input frame and buffer for faster access."
  (interactive)
  (emacs-input--create-frame)
  (emacs-input--create-buffer)
  ;; Pre-cache hs command path
  (emacs-input--find-hs-command)
  (message "emacs-input frame initialized"))

(defun emacs-input--notify-completion ()
  "Notify Hammerspoon that emacs-input has completed."
  (when (and (file-exists-p emacs-input-hammerspoon-script)
             (emacs-input--find-hs-command))
    (call-process (emacs-input--find-hs-command) nil nil nil "-c"
                  "require('emacs-input').markCompleted()")))

(defun emacs-input-finish ()
  "Finish editing and paste content back."
  (interactive)
  (when emacs-input-mode
    (let ((content (buffer-string))
          (is-temp-file (and (buffer-file-name)
                            (emacs-input--temp-file-p (buffer-file-name))))
          (is-emacs-input-buffer (string= (buffer-name) "*emacs-input*")))
      (unless (string= content emacs-input--original-content)
        ;; Copy to clipboard first (this ensures content is available)
        (kill-new content)
        (gui-select-text content)
        ;; Try to paste via Hammerspoon
        (if (and (file-exists-p emacs-input-hammerspoon-script)
                 (emacs-input--find-hs-command))
            (progn
              ;; pasteContent will handle markCompleted internally after pasting
              (call-process (emacs-input--find-hs-command) nil nil nil "-c"
                           (format "require('emacs-input').pasteContent(%S)" content))
              (message "Content pasted via Hammerspoon"))
          ;; Fallback: content is already in clipboard, user can paste manually
          ;; In this case, we need to manually mark completion
          (progn
            (emacs-input--notify-completion)
            (message "Content copied to clipboard - paste with Cmd+V"))))
      ;; Only notify completion if we didn't use Hammerspoon (fallback case handled above)
      (when (not (and (file-exists-p emacs-input-hammerspoon-script)
                      (emacs-input--find-hs-command)))
        (emacs-input--notify-completion))
      ;; Handle different buffer types
      (cond
       (is-temp-file
        ;; Traditional temp file approach
        (server-buffer-done (current-buffer)))
       (is-emacs-input-buffer
        ;; Fast mode with *emacs-input* buffer - close client frame
        (server-buffer-done (current-buffer)))
       (t
        ;; Pre-created buffer approach - hide frame and clear buffer
        (when (and emacs-input--frame (frame-live-p emacs-input--frame))
          (make-frame-invisible emacs-input--frame))
        (erase-buffer)
        (emacs-input-mode -1))))))

(defun emacs-input-abort ()
  "Abort editing without pasting."
  (interactive)
  (when emacs-input-mode
    (let ((is-temp-file (and (buffer-file-name)
                            (emacs-input--temp-file-p (buffer-file-name))))
          (is-emacs-input-buffer (string= (buffer-name) "*emacs-input*")))
      ;; Notify completion to prevent auto-trigger
      (emacs-input--notify-completion)
      (set-buffer-modified-p nil)
      (cond
       (is-temp-file
        ;; Traditional temp file approach
        (server-buffer-done (current-buffer)))
       (is-emacs-input-buffer
        ;; Fast mode with *emacs-input* buffer - close client frame
        (server-buffer-done (current-buffer)))
       (t
        ;; Pre-created buffer approach - hide frame and clear buffer
        (when (and emacs-input--frame (frame-live-p emacs-input--frame))
          (make-frame-invisible emacs-input--frame))
        (erase-buffer)
        (emacs-input-mode -1))))))

;;; Minor mode

(defvar emacs-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'emacs-input-finish)
    (define-key map (kbd "C-c C-k") #'emacs-input-abort)
    (define-key map (kbd "C-x 5 0") #'emacs-input-finish)
    map)
  "Keymap for emacs-input-mode.")

(define-minor-mode emacs-input-mode
  "Minor mode for emacs-input buffers."
  :init-value nil
  :lighter " EI"
  :keymap emacs-input-mode-map
  (when emacs-input-mode
    (visual-line-mode 1)))

;;; Initialization

;;;###autoload
(defun emacs-input-initialise ()
  "Entry point for emacs-input when visiting a file."
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (when (and file (emacs-input--temp-file-p file))
      (let ((app-info (frame-parameter nil 'emacs-input-app)))
        (setq-local emacs-input-current-app app-info)
        (setq emacs-input--original-content "")
        ;; Set major mode first
        (text-mode)
        ;; Enable emacs-input-mode AFTER text-mode
        (emacs-input-mode 1)
        ;; Try to get selected text
        (emacs-input--get-selection-async)
        (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort")))))

;; Add alias for American spelling
;;;###autoload
(defalias 'emacs-input-initialize 'emacs-input-initialise)

(defun emacs-input--temp-file-p (file)
  "Check if FILE is an emacs-input temporary file."
  (string-match-p "emacs-input-[0-9]\\{8\\}-[0-9]\\{6\\}" (file-name-nondirectory file)))

;;;###autoload
(add-hook 'server-visit-hook #'emacs-input-initialise)
(add-hook 'server-done-hook #'emacs-input--cleanup)

;; Auto-initialize frame when server starts
(defun emacs-input--auto-initialize ()
  "Auto-initialize emacs-input frame when server starts."
  (when (server-running-p)
    (run-with-timer 1.0 nil #'emacs-input-initialize-frame)
    ;; Configure auto-trigger if enabled
    (when emacs-input-auto-trigger-on-focus
      (run-with-timer 2.0 nil #'emacs-input-configure-auto-trigger))))

;;;###autoload
(add-hook 'server-switch-hook #'emacs-input--auto-initialize)
(add-hook 'after-init-hook #'emacs-input--auto-initialize)

(defun emacs-input--cleanup ()
  "Clean up emacs-input temporary files."
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (when (and file (emacs-input--temp-file-p file))
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'emacs-input)
;;; emacs-input-fixed.el ends here
