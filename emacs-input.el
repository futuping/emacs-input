;;; emacs-input.el --- Minimal system-wide popup Emacs for quick edits -*- lexical-binding: t; -*-

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
    (left-fringe . 8)
    (right-fringe . 8)
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

;;; Core functionality

(defun emacs-input--find-hs-command ()
  "Find the hs command path."
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

(defun emacs-input--get-app-info-async ()
  "Asynchronously get application information via Hammerspoon."
  (when (and (file-exists-p emacs-input-hammerspoon-script)
             (emacs-input--find-hs-command))
    (let ((process (start-process "emacs-input-app-info" nil
                                 (emacs-input--find-hs-command) "-c" "require('emacs-input').getAppInfo()")))
      (set-process-sentinel process #'emacs-input--app-info-sentinel))))

(defun emacs-input--app-info-sentinel (process event)
  "Handle app info process completion."
  (when (string-match "finished" event)
    (with-current-buffer (process-buffer process)
      (let ((info (string-trim (buffer-string))))
        (when (> (length info) 0)
          (setq emacs-input--app-info (read info))
          ;; Try to get selected text if available
          (emacs-input--get-selection-async))))))

(defun emacs-input--get-selection-async ()
  "Asynchronously get selected text via Hammerspoon."
  (when (and (file-exists-p emacs-input-hammerspoon-script)
             (emacs-input--find-hs-command))
    (let ((process (start-process "emacs-input-selection" nil
                                 (emacs-input--find-hs-command) "-c" "require('emacs-input').getSelection()")))
      (set-process-sentinel process #'emacs-input--selection-sentinel))))

(defun emacs-input--selection-sentinel (process event)
  "Handle selection process completion."
  (when (string-match "finished" event)
    (with-current-buffer (process-buffer process)
      (let ((selection (string-trim (buffer-string))))
        (when (and (> (length selection) 0)
                   emacs-input--frame
                   (frame-live-p emacs-input--frame))
          (with-selected-frame emacs-input--frame
            (when (string= (buffer-string) "")
              (insert selection)
              (setq emacs-input--original-content selection))))))))

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
(defun emacs-input-quick ()
  "Quick emacs-input without creating new process (for internal use)."
  (interactive)
  (let* ((app-info (emacs-input--get-app-info))
         (temp-file (emacs-input--create-temp-file app-info)))
    (find-file temp-file)
    (setq-local emacs-input-current-app app-info)
    (emacs-input-mode 1)
    (text-mode)
    (setq emacs-input--original-content "")
    (emacs-input--get-selection-async)
    (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort")))

(defun emacs-input-finish ()
  "Finish editing and paste content back."
  (interactive)
  (when emacs-input-mode
    (let ((content (buffer-string)))
      (unless (string= content emacs-input--original-content)
        ;; Copy to clipboard
        (kill-new content)
        (gui-select-text content)
        ;; Paste via Hammerspoon
        (when (and (file-exists-p emacs-input-hammerspoon-script)
                   (emacs-input--find-hs-command))
          (call-process (emacs-input--find-hs-command) nil nil nil "-c"
                       (format "require('emacs-input').pasteContent(%S)" content))))
      ;; Close the client
      (server-buffer-done (current-buffer)))))

(defun emacs-input-abort ()
  "Abort editing without pasting."
  (interactive)
  (when emacs-input-mode
    (set-buffer-modified-p nil)
    (server-buffer-done (current-buffer))))

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
    (visual-line-mode 1)
    (setq-local header-line-format
                (propertize " Press C-c C-c to finish, C-c C-k to abort"
                           'face 'mode-line-inactive))))

;;; Initialization

;;;###autoload
(defun emacs-input-initialise ()
  "Entry point for emacs-input when visiting a file."
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (when (and file (emacs-input--temp-file-p file))
      (let ((app-info (frame-parameter nil 'emacs-input-app)))
        (setq-local emacs-input-current-app app-info)
        (emacs-input-mode 1)
        (text-mode)
        (setq emacs-input--original-content "")
        ;; Try to get selected text
        (emacs-input--get-selection-async)
        (message "emacs-input ready - Press C-c C-c to finish, C-c C-k to abort")))))

(defun emacs-input--temp-file-p (file)
  "Check if FILE is an emacs-input temporary file."
  (string-match-p "emacs-input-[0-9]\\{8\\}-[0-9]\\{6\\}" (file-name-nondirectory file)))

;;;###autoload
(add-hook 'server-visit-hook #'emacs-input-initialise)
(add-hook 'server-done-hook #'emacs-input--cleanup)

(defun emacs-input--cleanup ()
  "Clean up emacs-input temporary files."
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (when (and file (emacs-input--temp-file-p file))
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'emacs-input)
;;; emacs-input.el ends here
