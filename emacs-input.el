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
(require 'cl-lib)

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

;;; Internal variables

(defvar-local emacs-input-current-app nil
  "Current application information.")

(defvar-local emacs-input--original-content ""
  "Original content before editing.")

(defvar emacs-input--hs-command nil
  "Path to the hs command.")

(defvar emacs-input--app-info nil
  "Current application information.")

(defvar emacs-input--app-cache nil
  "Cache for application information.")

(defvar emacs-input--frame-pool nil
  "Pool of pre-created frames for fast access.")

(defvar emacs-input--hs-paths
  '("hs"
    "/Applications/Nix Apps/Hammerspoon.app/Contents/Frameworks/hs/hs"
    "/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs")
  "Possible paths for hs command, ordered by preference.")

(defvar emacs-input--context-loading nil
  "Flag indicating if context is currently being loaded.")

(defvar emacs-input--buffer-counter 0
  "Counter for creating unique buffer names.")

(defvar emacs-input--active-buffers nil
  "List of active emacs-input buffers.")

;;; Core functionality

(defun emacs-input--find-hs-command ()
  "Find the hs command path with improved caching."
  (or emacs-input--hs-command
      (setq emacs-input--hs-command
            (cl-find-if (lambda (path)
                          (if (string= path "hs")
                              (executable-find path)
                            (file-executable-p path)))
                        emacs-input--hs-paths))))

(defun emacs-input--get-context-async ()
  "Get app info asynchronously for better performance."
  (when (and (file-exists-p emacs-input-hammerspoon-script)
             (emacs-input--find-hs-command)
             (not emacs-input--context-loading))
    (setq emacs-input--context-loading t)
    ;; Only get app info, no selection needed
    (emacs-input--get-app-info-async)))

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
                  ;; Cache the app info for future use
                  (setq emacs-input--app-cache emacs-input--app-info)
                  (message "App info loaded: %s" (plist-get emacs-input--app-info :name)))
              (error
               (message "Error processing app info: %s" err)))))))))



(defun emacs-input--find-active-buffer ()
  "Find the currently active emacs-input buffer."
  (cl-find-if (lambda (buffer)
                (with-current-buffer buffer
                  (and emacs-input-mode
                       (or (bound-and-true-p emacs-input--is-memory-buffer)
                           (emacs-input--temp-file-p (or (buffer-file-name) ""))))))
              (buffer-list)))

;;; Frame pool management

(defun emacs-input--ensure-frame-pool ()
  "Ensure we have a ready-to-use frame in the pool."
  (unless (and emacs-input--frame-pool
               (frame-live-p emacs-input--frame-pool))
    (setq emacs-input--frame-pool
          (make-frame (append emacs-input-frame-parameters
                             '((visibility . nil)
                               (name . "emacs-input-pool")))))))

(defun emacs-input--get-frame-from-pool ()
  "Get a frame from the pool, creating one if necessary."
  (emacs-input--ensure-frame-pool)
  (let ((frame emacs-input--frame-pool))
    ;; Reset the pool
    (setq emacs-input--frame-pool nil)
    ;; Make the frame visible and focused
    (make-frame-visible frame)
    (select-frame frame)
    (raise-frame frame)
    ;; Prepare a new frame for the pool in background
    (run-with-idle-timer 0.1 nil #'emacs-input--ensure-frame-pool)
    frame))

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

(defun emacs-input--get-cached-app-info ()
  "Get app info with caching for repeated calls."
  (or emacs-input--app-cache
      (setq emacs-input--app-cache
            (emacs-input--get-app-info))))

(defun emacs-input--clear-app-cache ()
  "Clear the application info cache."
  (setq emacs-input--app-cache nil))

(defun emacs-input--create-temp-file (app-info)
  "Create a temporary file for editing."
  (let* ((temp-name (format "emacs-input-%s-%s"
                           (format-time-string "%Y%m%d-%H%M%S")
                           (or (and app-info (plist-get app-info :name)) "unknown")))
         (temp-file (expand-file-name temp-name temporary-file-directory)))
    temp-file))

(defun emacs-input--create-temp-file-fast ()
  "Create a temporary file quickly without waiting for app info."
  (let* ((temp-name (format "emacs-input-%s-fast"
                           (format-time-string "%Y%m%d-%H%M%S")))
         (temp-file (expand-file-name temp-name temporary-file-directory)))
    temp-file))

(defun emacs-input--create-memory-buffer ()
  "Create a memory buffer for emacs-input without file I/O."
  (let* ((buffer-name (format "*emacs-input-%d*"
                             (cl-incf emacs-input--buffer-counter)))
         (buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      ;; Mark this as an emacs-input buffer
      (setq-local emacs-input--is-memory-buffer t)
      ;; Add to active buffers list
      (push buffer emacs-input--active-buffers))
    buffer))

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
  "Launch emacs-input with immediate UI and memory buffer.
This may open FILE if specified, otherwise creates a memory buffer."
  (interactive)
  (if file
      ;; If file is specified, use the original method with caching
      (let* ((app-info (emacs-input--get-cached-app-info))
             (temp-file file)
             (params (emacs-input--command-params app-info temp-file)))
        (apply #'call-process "emacsclient" nil 0 nil params))
    ;; Check if we're being called from terminal (emacsclient -e)
    (if (and (not (display-graphic-p))
             (not (frame-parameter nil 'client)))
        ;; Terminal mode: use emacsclient with -c -F to create GUI client
        (let* ((app-info (emacs-input--get-cached-app-info))
               (temp-file (emacs-input--create-temp-file-fast))
               (params (emacs-input--command-params app-info temp-file)))
          (apply #'call-process "emacsclient" nil 0 nil params))
      ;; GUI mode: use memory buffer mode directly
      (let ((frame (emacs-input--get-frame-from-pool))
            (buffer (emacs-input--create-memory-buffer)))
        (with-selected-frame frame
          (switch-to-buffer buffer)
          (text-mode)
          (emacs-input-mode 1)
          (setq emacs-input--original-content "")
          ;; Start async context loading in background (app info only)
          (emacs-input--get-context-async)
          (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort"))))))

;;;###autoload
(defun emacs-input-quick ()
  "Quick emacs-input with memory buffer (for internal use)."
  (interactive)
  (let* ((app-info (emacs-input--get-cached-app-info))
         (buffer (emacs-input--create-memory-buffer)))
    (switch-to-buffer buffer)
    (setq-local emacs-input-current-app app-info)
    (text-mode)
    (emacs-input-mode 1)  ; Enable emacs-input-mode AFTER text-mode
    (setq emacs-input--original-content "")
    (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort")))



(defun emacs-input-finish ()
  "Finish editing and paste content back."
  (interactive)
  (when emacs-input-mode
    (let ((content (buffer-string))
          (is-memory-buffer (bound-and-true-p emacs-input--is-memory-buffer)))
      (unless (string= content emacs-input--original-content)
        ;; Copy to clipboard
        (kill-new content)
        (gui-select-text content)
        ;; Paste via Hammerspoon
        (when (and (file-exists-p emacs-input-hammerspoon-script)
                   (emacs-input--find-hs-command))
          (call-process (emacs-input--find-hs-command) nil nil nil "-c"
                       (format "require('emacs-input').pasteContent(%S)" content))))
      ;; Clean up buffer
      (if is-memory-buffer
          (emacs-input--cleanup-memory-buffer (current-buffer))
        (server-buffer-done (current-buffer))))))

(defun emacs-input-abort ()
  "Abort editing without pasting."
  (interactive)
  (when emacs-input-mode
    (let ((is-memory-buffer (bound-and-true-p emacs-input--is-memory-buffer)))
      (set-buffer-modified-p nil)
      (if is-memory-buffer
          (emacs-input--cleanup-memory-buffer (current-buffer))
        (server-buffer-done (current-buffer))))))

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
        ;; Start async context loading for better performance (app info only)
        (emacs-input--get-context-async)
        (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort")))))

;; Add alias for American spelling
;;;###autoload
(defalias 'emacs-input-initialize 'emacs-input-initialise)

(defun emacs-input--temp-file-p (file)
  "Check if FILE is an emacs-input temporary file."
  (string-match-p "^emacs-input-" (file-name-nondirectory file)))

;;;###autoload
(add-hook 'server-visit-hook #'emacs-input-initialise)
(add-hook 'server-done-hook #'emacs-input--cleanup)

(defun emacs-input--cleanup ()
  "Clean up emacs-input temporary files."
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (when (and file (emacs-input--temp-file-p file))
      (when (file-exists-p file)
        (delete-file file)))))

(defun emacs-input--cleanup-memory-buffer (buffer)
  "Clean up memory buffer and close frame if needed."
  (when (buffer-live-p buffer)
    ;; Remove from active buffers list
    (setq emacs-input--active-buffers
          (delq buffer emacs-input--active-buffers))
    ;; Kill the buffer
    (kill-buffer buffer)
    ;; Close frame if it was created for emacs-input
    (let ((frame (selected-frame)))
      (when (string= (frame-parameter frame 'name) "emacs-input")
        (delete-frame frame)))))

;;;###autoload
(defun emacs-input-initialize-frame-pool ()
  "Initialize the frame pool for faster startup."
  (interactive)
  (emacs-input--ensure-frame-pool)
  (message "emacs-input frame pool initialized"))

;;;###autoload
(defun emacs-input-cleanup-all ()
  "Clean up all emacs-input memory buffers."
  (interactive)
  (dolist (buffer emacs-input--active-buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (setq emacs-input--active-buffers nil)
  (message "All emacs-input buffers cleaned up"))

;; Clear cache periodically to ensure fresh app info
(run-with-idle-timer 30 t #'emacs-input--clear-app-cache)

(provide 'emacs-input)
;;; emacs-input-fixed.el ends here
