;;; test-performance.el --- Performance testing for emacs-input -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script to measure emacs-input performance improvements

;;; Code:

(require 'emacs-input)

(defun test-emacs-input-performance ()
  "Test emacs-input performance with timing."
  (interactive)
  (let ((start-time (current-time)))
    (message "Testing emacs-input-fast performance...")
    
    ;; Initialize frame first
    (emacs-input-initialize-frame)
    
    ;; Measure fast version
    (let ((fast-start (current-time)))
      (emacs-input-fast)
      (let ((fast-time (float-time (time-subtract (current-time) fast-start))))
        (message "emacs-input-fast took: %.3f seconds" fast-time)))
    
    ;; Close the frame
    (when (and emacs-input--frame (frame-live-p emacs-input--frame))
      (delete-frame emacs-input--frame))
    
    (message "Performance test completed")))

(defun test-traditional-performance ()
  "Test traditional emacs-input performance."
  (interactive)
  (message "Testing traditional emacs-input performance...")
  (let ((start-time (current-time)))
    (emacs-input)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Traditional emacs-input took: %.3f seconds" elapsed))))

(defun benchmark-emacs-input ()
  "Benchmark both versions of emacs-input."
  (interactive)
  (message "=== emacs-input Performance Benchmark ===")
  
  ;; Test traditional version
  (message "\n1. Testing traditional version...")
  (test-traditional-performance)
  
  (sit-for 2)  ; Wait a bit
  
  ;; Test fast version
  (message "\n2. Testing fast version...")
  (test-emacs-input-performance)
  
  (message "\n=== Benchmark Complete ==="))

(provide 'test-performance)
;;; test-performance.el ends here
