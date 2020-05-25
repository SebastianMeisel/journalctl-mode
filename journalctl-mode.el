;;; journalctl-mode.el --- Sample major mode for  viewing output journalctl -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020, by Sebastian Meisel

;; Author: Sebastian Meisel <sebastian.meisel@gmail.com>
;; Version: 0.8
;; Created:  Mai 23, 2020
;; Keywords: unix
;; Homepage: http://github.com/SebastianMeisel/journalctl-mode
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; This is a major-mode for Emacs to view systemd's journalctl output in Emacs.
;; The output is split into chunks for performance reasons.
;; Fontification is provided and may be customized.
;; At the moment it is still in very early development.  Please give feedback on any problems that occure.

;; Put journalctl-mode.el in your load-path and add   ( require 'journalctl-mode)  to your .Emacs file.

;;; Code:

(require 'array)

;; customization

(defgroup journalctl nil
  "View journalctl output in a emacs buffer"
  :group 'external)

(defcustom journalctl-chunk-size
  250
 "Number of lines of journalctl output that are loaded in the buffer.  You can navigate."
  :group 'journalctl
  :type 'integer)
  
(defcustom journalctl-error-keywords
  '("Failed" "failed" "Error" "error" "critical" "couldn't" "Can't" "not" "Not" "unreachable")
  "Keywords that mark errors in journalctl output."
  :group 'journalctl
  :type 'string)

(defcustom journalctl-warn-keywords
  '("Warning" "warn" "debug")
  "Keywords that mark warnings in journalctl output."
  :group 'journalctl
  :type 'string)

(defcustom journalctl-starting-keywords
  '("Starting" "Activating" "Listening" "Reloading" "connect")
  "Keywords that mark start of processes or steps in journalctl output."
  :group 'journalctl
  :type 'string)

(defcustom journalctl-finished-keywords
  '("Stopped" "Stopping" "Reached" "Closed" "finished" "Started" "Successfully activated"
    "Received" "opened" "success" "enabled" "removed" "active" "Created" "loaded" "detected")
  "Keywords that mark finished processes or steps in journalctl output."
  :group 'journalctl
  :type 'string)

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
  "Counter for chunks of journalctl output loaded into the *journalctl*-buffer.")

(defvar journalctl-current-lines
  0
  "Number of lines  of journalctl output with current params.")


(defvar journalctl-current-params
  ""
  "Keeps the parametes of  the last call of journalctl.")

(defvar journalctl-current-filter
  ""
  "Keeps filters as grep that shall be applied to journalctl's output.")

(defvar journalctl-disk-usage
  (concat
   "Disk-usage: "
   (shell-command-to-string "journalctl --disk-usage | egrep -o '[0-9.]+G'"))
  "Disk-usage of  journalctl.")

;; functions

(defun journalctl (&optional flags chunk)
  "Run journalctl with give FLAGS and present CHUNK of  output in a special buffer."
  (interactive)
  (let ((param (or flags (read-string "parameter: " "-x  -n 1000" nil "-x "))))
    (setq journalctl-current-lines (string-to-number (shell-command-to-string (concat "journalctl " param "|  wc -l"))))
    (let* ((this-chunk (or chunk  0)) ;; if chunk is not explicit given, we assume this first (0) chunk
	         (first-line (+ 1 (* this-chunk journalctl-chunk-size)))
	         (last-line (if (<= (+ first-line journalctl-chunk-size)
                              journalctl-current-lines)
			                    (+ first-line journalctl-chunk-size)
		                    journalctl-current-lines)))
      (with-current-buffer (get-buffer-create "*journalctl*")
        (setq buffer-read-only nil)
        (fundamental-mode)
        (erase-buffer))
      (save-window-excursion
       (shell-command (concat "journalctl " param journalctl-current-filter
                              " | sed -ne '"  (int-to-string first-line) ","
                              (int-to-string last-line) "p'")
                      "*journalctl*" "*journalctl-error*"))
      (switch-to-buffer "*journalctl*")
      (setq buffer-read-only t)
      (setq journalctl-current-params param)
      (journalctl-mode))))


;;;;;; Moving and Chunks

(defun journalctl-next-chunk ()
  "Load the next chunk of journalctl output to the buffer."
  (interactive)
  (let* ((chunk (if  (> (* (+ 2 journalctl-current-chunk) journalctl-chunk-size) journalctl-current-lines)
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

(defun journalctl-scroll-up ()
  "Scroll up journalctl output or move to next chunk when bottom of frame is reached."
  (interactive)
  (let ((target-line  (+ (current-line) 25)))
    (if (>= target-line journalctl-current-lines)
	(message "%s" "End of journalctl output")
      (if (>= target-line journalctl-chunk-size)
	(journalctl-next-chunk)
      (forward-line 25)))))

(defun journalctl-scroll-down ()
  "Scroll down journalctl output or move to next chunk when bottom of frame is reached."
  (interactive)
  (let ((target-line  (- (current-line) 25)))
    (if (<= target-line 0)
	(if (<=  journalctl-current-chunk 0)
	    	(message "%s" "Beginn of journalctl output")
	(journalctl-previous-chunk)))
      (forward-line  -25)))

;;;;;;;; Special functions

(defun journalctl-boot (&optional boot)
  "Select and show boot-log.

If BOOT is provided it is the number of the boot-log to be shown."
  (interactive)
  (let ((boot-log (or boot (car (split-string
				 (completing-read "Boot: " (reverse (split-string
		     (shell-command-to-string "journalctl --list-boots") "[\n]" t " ")) nil t))))))
    (journalctl (concat "-b " boot-log))))

(defun journalctl-unit (&optional unit)
  "Select and show journal for UNIT."
  (interactive)
  (let ((unit (or unit (car (split-string
				 (completing-read "unit: " (split-string
		     (shell-command-to-string "systemctl list-units --all --quiet | awk '{print $1}' | head -n -7 | sed -ne '2,$p'| sed -e '/●/d'") "[\n]" t " ") nil t))))))
    (journalctl (concat "--unit='" unit "'"))))

(defun journalctl-user-unit (&optional unit)
  "Select and show journal for the user-unit UNIT."
  (interactive)
  (let ((unit (or unit (car (split-string
				 (completing-read "unit: " (split-string
		     (shell-command-to-string "systemctl list-units --all --user --quiet | awk '{print $1}' | head -n -7 | sed -ne '2,$p'| sed -e '/●/d'") "[\n]" t " ") nil t))))))
    (journalctl (concat "--user-unit='" unit "'"))))


(defun journalctl-add-param (&optional flags)
  "Add parameters to journalctl call.

If FLAGS is set, use these parameters."
  (interactive)
  (let ((param (or flags (read-string "parameter: " "" nil ))))
    (unless (string-match param journalctl-current-params)
    (setq journalctl-current-params (concat journalctl-current-params  " " param)))
    (journalctl journalctl-current-params journalctl-current-chunk)))

(defun journalctl-add-since (&optional date)
  "Add '--since' option with DATE or ask for date."
  (interactive)
  (let ((date (or  date
		   		   (if  (fboundp 'org-read-date)  (org-read-date t) (read-string "Date [yy-mm-dd [hh:mm[:ss]]]")))))
     (journalctl-add-param (concat " --since='" date "'"))))

(defun journalctl-add-until (&optional date)
  "Add '--until' option with DATE or ask for date."
  (interactive)
  (let ((date (or  date
		   (if  (fboundp 'org-read-date)  (org-read-date t) (read-string "Date [yy-mm-dd [hh:mm[:ss]]]")))))
     (journalctl-add-param (concat " --until='" date "'"))))

(defun journalctl-add-priority (&optional priority to-priority)
  "Add '--priority' option with PRIORITY.
If TO-PRIORITY is non-nil, call '--priority' with range
from PRIORITY  TO-PRIORITY.
If none is non-nil it will prompt for priority (range)."
  (interactive)
  (let* ((from-priority (or  priority (completing-read "Priority: "
						 '("emerg" "alert" "crit" "err" "warning" "notice" "info" "debug")
						 nil t "warning")))
	 (to-priority (if  priority  
			 (or to-priority nil)
			(or to-priority  (completing-read "Priority: "
						 '(("emerg" "alert" "crit" "err" "warning" "notice" "info" "debug"))
						 nil nil priority))))
	 (param (concat "--priority='" from-priority (when to-priority
						       (unless (string-equal from-priority to-priority) (concat ".." to-priority))) "'")))
     (journalctl-add-param param)))

(defun journalctl-grep (&optional pattern)
  "Run journalctl with -grep flag to search for PATTERN."
  (interactive)
  (let ((pattern (or pattern (read-string "grep pattern: " nil nil ))))
    (setq journalctl-current-filter (concat journalctl-current-filter  "| grep '" pattern "'" ))
    (journalctl journalctl-current-params journalctl-current-chunk)))

(defun journalctl-remove-filter ()
  "Remove all filters such as grep from journalctl output."
  (interactive)
  (setq journalctl-current-filter "")
    (journalctl journalctl-current-params journalctl-current-chunk))
  
(defun  journalctl-edit-params ()
  "Edit the value of 'journalctl-current-params'."
  (interactive)
  (let ((param (read-string "Parameters: " journalctl-current-params)))
    (setq journalctl-current-params param)
    (journalctl journalctl-current-params journalctl-current-chunk)))


;;;;;;;;;;;;;;;;; Fontlock


(defvar journalctl-font-lock-keywords
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
    ;; add params
    (define-key map (kbd "+ +")  'journalctl-add-param)
    (define-key map (kbd "+ r")  (lambda () (interactive) (journalctl-add-param "-r" )));; reverse output
    (define-key map (kbd "+ x")  (lambda () (interactive) (journalctl-add-param "-x" )));; add explanations
    (define-key map (kbd "+ s")  (lambda () (interactive) (journalctl-add-param "--system" )));; system-units only
    (define-key map (kbd "+ u")  (lambda () (interactive) (journalctl-add-param "--user" )));; user-units only
    (define-key map (kbd "+ k")  (lambda () (interactive) (journalctl-add-param "--dmesg" )));; user-units only
    (define-key map (kbd "+ S")  'journalctl-add-since)
    (define-key map (kbd "+ U")  'journalctl-add-until)
    (define-key  map (kbd "+ p")  'journalctl-add-priority)
    ;;  edit params
    (define-key map (kbd "e") 'journalctl-edit-params)
    ;; grep
    (define-key map (kbd "+ g")  'journalctl-grep)
    (define-key map (kbd "- -")  'journalctl-remove-filter)
    ;;
    (define-key map (kbd "C-v") 'journalctl-scroll-up)
    (define-key map (kbd "M-v") 'journalctl-scroll-down)
    (define-key map (kbd "q")  (lambda () (interactive) (kill-buffer  "*journalctl*")))
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
