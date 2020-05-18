;;; journalctl-mode.el --- sample major mode for  viewing output journalctl. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020, by Sebastian Meisel

;; Author: Sebastian Meisel (sebastian.meisel@gmail.com)
;; Version: 0.1
;; Created:  Mai 16, 2020
;; Keywords: system
;; Homepage: http://github.com/SebastianMeisel/journalctl-mode

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here

;;; Code:

;; customization

(defcustom journalctl-chunk-size
  1000
  "Number of lines of journalctl output that are loaded in the buffer. You can navigate "
  :group 'journalctl)
  
(defcustom journalctl-error-keywords
  '("Failed" "failed" "Error" "error" "critical" "couldn't" "Can't" "not" "Not" "unreachable")
  "Keywords that mark errors in journalctl output"
  :group 'journalctl)

(defcustom journalctl-warn-keywords
  '("Warning" "warn" "debug")
  "Keywords that mark warnings in journalctl output"
  :group 'journalctl)

(defcustom journalctl-starting-keywords
  '("Starting" "Activating" "Listening" "Reloading" "connect")
  "Keywords that mark start of processes or steps in journalctl output"
  :group 'journalctl)

(defcustom journalctl-finished-keywords
  '("Stopped" "Stopping" "Reached" "Closed" "finished" "Started" "Successfully activated"
    "Received" "opened" "success" "enabled" "removed" "active" "Created" "loaded" "detected")
  "Keywords that mark finished processes or steps in journalctl output"
  :group 'journalctl)

;;; faces
(defface journalctl-error-face
    '((t :foreground "red" :bold t)) 
    "Face to mark errors in journalctl's output."
    :group 'journalctl)

(defface journalctl-warning-face
    '((t :foreground "orange" :bold t)) 
    "Face to mark warnings in journalctl's output."
        :group 'journalctl)

(defface journalctl-starting-face
    '((t :foreground "green"  :bold nil)) 
    "Face to mark starting units in journalctl's output."
        :group 'journalctl)

(defface journalctl-finished-face
    '((t :foreground "green"  :bold t)) 
    "Face to mark finished units in journalctl's output."
        :group 'journalctl)

(defface journalctl-timestamp-face
    '((t :foreground "white"  :bold t)) 
    "Face for timestamps in journalctl's output."
        :group 'journalctl)

 (defface journalctl-host-face
    '((t :foreground "blue"  :bold nil)) 
    "Face for hosts in journalctl's output."
        :group 'journalctl)

(defface journalctl-process-face
    '((t :foreground "lightblue"  :bold nil)) 
    "Face for hosts in journalctl's output."
        :group 'journalctl)

;; variables
(defvar journalctl-current-chunk
  0
  "Counter for chunks of journalctl output loaded into the *journalctl*-buffer. ")

(defvar journalctl-current-params
  ""
  "Keeps the parametes of  the last call of journalctl.")

(defvar journalctl-disk-usage
  (concat
   "Disk-usage: "
   (shell-command-to-string "journalctl --disk-usage | egrep -o '[0-9.]+G'"))
  "Disk-usage of  journalctl.")

(defvar journalctl-boot-list
  '((name . "List of boot-logs")
    (candidates . (lambda ()
		    (split-string
		     (shell-command-to-string "journalctl --list-boots") "[\n]" t " ")))
    (action  . (lambda (candidate)
		 (message "%s" candidate) )) )
  "Helm query for  boot-logs available to journalctl.")
;; functions

(defun journalctl (&optional flags chunk)
  "Run journalctl with give FLAGS and present output in a special buffer."
  (interactive)
  (let* ((param (or flags (read-string "parameter: " nil nil "-xe ")))
	 (this-chunk (or chunk  0)) ;; if chunk is not explicit given, we assume this first (0) chunk
	 (lines (string-to-number (shell-command-to-string (concat "journalctl " param "|  wc -l"))))
	 (first-line (+ 1 (* this-chunk journalctl-chunk-size)))
	 (last-line (if (<= (+ first-line journalctl-chunk-size) lines)
			(+ first-line journalctl-chunk-size)
		      lines)))
    (with-current-buffer (get-buffer-create "*journalctl*")
      (setq buffer-read-only nil)
      (fundamental-mode)
      (erase-buffer))
    (shell-command  (concat "journalctl " param " | sed -ne '"  (int-to-string first-line) "," (int-to-string last-line) "p'") "*journalctl*" "*journalctl-error*")
  (switch-to-buffer "*journalctl*")
  (setq buffer-read-only t)
  (setq journalctl-current-params param)
  (journalctl-mode)))

(defun journalctl-next-chunk ()
  "Load the next chunk of journalctl output to the buffer."
  (interactive)
  (let* ((lines (string-to-number
		(shell-command-to-string (concat "journalctl " journalctl-current-params "|  wc -l"))))
	(chunk (if  (> (* (+ 2 journalctl-current-chunk) journalctl-chunk-size) lines)
		 journalctl-current-chunk
		 (+ journalctl-current-chunk 1) )))
	(setq journalctl-current-chunk chunk)
	(journalctl journalctl-current-params  chunk)))

(defun journalctl-previous-chunk ()
  "Load the previous chunk of journalctl output to the buffer."
  (interactive)
  (let ((chunk (if (>= journalctl-current-chunk 1) (- journalctl-current-chunk 1) 0)))
	(setq journalctl-current-chunk chunk)
	(journalctl journalctl-current-params  chunk)))


(defun journalctl-boot (&optional boot)
  "Select and show boot-log.

If BOOT is provided it is the number of the boot-log to be shown."
  (interactive)  
  (let ((boot-log (or boot (car (split-string 
 			    (helm :sources journalctl-boot-list))))))
    (journalctl (concat "-b " boot-log))))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq journalctl-font-lock-keywords
      (let* (
            ;; generate regex string for each category of keywords
	     (error-keywords-regexp (regexp-opt journalctl-error-keywords 'words))
	     (warn-keywords-regexp (regexp-opt journalctl-warn-keywords 'words))
	     (starting-keywords-regexp (regexp-opt journalctl-starting-keywords 'words))
	     (finished-keywords-regexp (regexp-opt journalctl-finished-keywords 'words)))
        `(
          (,warn-keywords-regexp . 'journalctl-warning-face)
          (,error-keywords-regexp . 'journalctl-error-face)
          (,starting-keywords-regexp . 'journalctl-starting-face)
          (,finished-keywords-regexp . 'journalctl-finished-face)
	  ("^\\([A-Z][a-z]+ [0-9]+ [0-9:]+\\)" . (1 'journalctl-timestamp-face))
	  ("^\\([A-Z][a-z]+ [0-9]+ [0-9:]+\\) \\([-a-zA-Z]+\\)" . (2 'journalctl-host-face))
	  ("^\\([A-Z][a-z]+ [0-9]+ [0-9:]+\\) \\([-a-zA-Z]+\\) \\(.*?:\\)" . (3 'journalctl-process-face))

          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;; keymap
(defvar journalctl-mode-map
  (let ((map (make-keymap "journalctl")))
    (define-key map (kbd "n") 'journalctl-next-chunk)
    (define-key map (kbd "p") 'journalctl-previous-chunk)
    map)
  "Keymap for journalctl mode.")

;;;###autoload
(define-derived-mode journalctl-mode c-mode "journalctl mode"
  "Major mode for viewing journalctl output"
  (setq major-mode 'journalctl-mode)
  (setq mode-name "journalctl")
  (setq mode-line-process journalctl-disk-usage)
  ;; code for syntax highlighting
  (setq font-lock-defaults '((journalctl-font-lock-keywords)))
    ;; Keymap 
  (use-local-map journalctl-mode-map))

;; add the mode to the `features' list
(provide 'journalctl-mode)

;;; journalctl-mode.el ends here
