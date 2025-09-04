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
    (minibuffer . nil)
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

(defvar emacs-input--frame nil
  "The pre-created emacs-input frame.")

(defvar emacs-input--buffer nil
  "The emacs-input buffer.")

(defvar emacs-input--app-info nil
  "Current application information.")

(defvar emacs-input--original-content ""
  "Original content before editing.")

;;; Core functionality

(defun emacs-input--create-frame ()
  "Create and hide the emacs-input frame."
  (when (and emacs-input--frame (frame-live-p emacs-input--frame))
    (delete-frame emacs-input--frame))
  (setq emacs-input--frame
        (make-frame (append emacs-input-frame-parameters
                           '((visibility . nil)))))
  (with-selected-frame emacs-input--frame
    (setq emacs-input--buffer (get-buffer-create "*emacs-input*"))
    (switch-to-buffer emacs-input--buffer)
    (text-mode)
    (emacs-input-mode 1)))

(defun emacs-input--show-frame ()
  "Show and focus the emacs-input frame."
  (unless (and emacs-input--frame (frame-live-p emacs-input--frame))
    (emacs-input--create-frame))
  (make-frame-visible emacs-input--frame)
  (select-frame-set-input-focus emacs-input--frame)
  (with-selected-frame emacs-input--frame
    (switch-to-buffer emacs-input--buffer)
    (erase-buffer)
    (setq emacs-input--original-content "")
    ;; Position frame near cursor
    (emacs-input--position-frame)))

(defun emacs-input--hide-frame ()
  "Hide the emacs-input frame."
  (when (and emacs-input--frame (frame-live-p emacs-input--frame))
    (make-frame-invisible emacs-input--frame)))

(defun emacs-input--position-frame ()
  "Position frame near mouse cursor."
  (when (and emacs-input--frame (frame-live-p emacs-input--frame))
    (let* ((mouse-pos (mouse-absolute-pixel-position))
           (x (max 0 (- (car mouse-pos) 200)))
           (y (max 0 (- (cdr mouse-pos) 100))))
      (set-frame-position emacs-input--frame x y))))

(defun emacs-input--get-app-info-async ()
  "Asynchronously get application information via Hammerspoon."
  (when (file-exists-p emacs-input-hammerspoon-script)
    (let ((process (start-process "emacs-input-app-info" nil
                                 "hs" "-c" "require('emacs-input').getAppInfo()")))
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
  (when (file-exists-p emacs-input-hammerspoon-script)
    (let ((process (start-process "emacs-input-selection" nil
                                 "hs" "-c" "require('emacs-input').getSelection()")))
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

;;; Public API

;;;###autoload
(defun emacs-input ()
  "Show emacs-input frame for quick editing."
  (interactive)
  (emacs-input--show-frame)
  (emacs-input--get-app-info-async))

(defun emacs-input-finish ()
  "Finish editing and paste content back."
  (interactive)
  (let ((content (buffer-string)))
    (unless (string= content emacs-input--original-content)
      ;; Copy to clipboard
      (kill-new content)
      (gui-select-text content)
      ;; Paste via Hammerspoon
      (when (file-exists-p emacs-input-hammerspoon-script)
        (call-process "hs" nil nil nil "-c" 
                     (format "require('emacs-input').pasteContent(%S)" content))))
    (emacs-input--hide-frame)))

(defun emacs-input-abort ()
  "Abort editing without pasting."
  (interactive)
  (emacs-input--hide-frame))

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
(defun emacs-input-initialize ()
  "Initialize emacs-input by creating the frame."
  (interactive)
  (emacs-input--create-frame)
  (message "emacs-input initialized"))

;; Auto-initialize when server starts
(add-hook 'server-switch-hook #'emacs-input-initialize)

(provide 'emacs-input)
;;; emacs-input.el ends here
