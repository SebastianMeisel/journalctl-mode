;;; journalctl-mode.el --- Sample major mode for  viewing output journalctl -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020, by Sebastian Meisel

;; Author: Sebastian Meisel <sebastian.meisel@gmail.com>
;; Version: 1.0
;; Created:  November 8, 2023
;; Keywords: unix
;; Homepage: https://github.com/SebastianMeisel/journalctl-mode
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; License:

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This is a major-mode for Emacs to view systemd's journalctl output in Emacs.
;; The output is split into chunks for performance reasons.
;; Fontification is provided and may be customized.
;; At the moment it is still in very early development.  Please give feedback on any problems that occur.

;; Put journalctl-mode.el in your load-path and add   ( require 'journalctl-mode)  to your .emacs file.

;;; Code:

(require 'array)
(require 'transient)

;; customization

(defgroup journalctl nil
  "View journalctl output in a Emacs buffer."
  :group 'external)

(defcustom journalctl-chunk-size
  250
  "Number of lines of journalctl output that are
  loaded in the buffer. You can navigate."
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

(defcustom journalctl-follow-freq
  0.1
  "Frequency in seconds for follow simulation."
  :group 'journalctl
  :type 'float)

(defcustom journalctl-follow-lines
  "30"
  "Number of lines for follow simulation."
  :group 'journalctl
  :type 'string)


;;; faces
(defface journalctl-error-face
  '((t :inherit error))
  "Face to mark errors in journalctl's output."
  :group 'journalctl)

(defface journalctl-warning-face
  '((t :inherit warning))
  "Face to mark warnings in journalctl's output."
  :group 'journalctl)

(defface journalctl-starting-face
  '((t :inherit success))
  "Face to mark starting units in journalctl's output."
  :group 'journalctl)

(defface journalctl-finished-face
  '((t :inherit success :bold t))
  "Face to mark finished units in journalctl's output."
  :group 'journalctl)

(defface journalctl-timestamp-face
  '((t :inherit font-lock-type-face))
  "Face for timestamps in journalctl's output."
  :group 'journalctl)

(defface journalctl-host-face
  '((t :inherit font-lock-keyword-face))
  "Face for hosts in journalctl's output."
  :group 'journalctl)

(defface journalctl-process-face
  '((t :inherit font-lock-function-name-face))
  "Face for hosts in journalctl's output."
  :group 'journalctl)

;; variables
(defvar journalctl-current-chunk
  0
  "Counter for chunks of journalctl output loaded into the *journalctl*-buffer.")

(defvar journalctl-current-lines
  0
  "Number of lines  of journalctl output with current opts.")


(defvar journalctl-current-opts
  ""
  "Keeps the opts of  the last call of journalctl.")

(defvar journalctl-current-filter
  ""
  "Keeps filters as grep that shall be applied to journalctl's output.")

(defvar journalctl-unit-list
  (split-string
   (shell-command-to-string "systemctl list-units --all --quiet | sed -e 's/●/ /g' | awk '{print $1}'")
   "[\n]" t " ")
  "List of systemd-units available to journalctl.")

(defvar journalctl-user-unit-list
  (split-string
   (shell-command-to-string "systemctl list-units --all --quiet --user | sed -e 's/●/ /g' | awk '{print $1}'")
   "[\n]" t " ")
  "List of systemd-units available to journalctl.")

(defvar journalctl-boot-list
  (split-string
   (shell-command-to-string "journalctl --list-boots | awk '{print $1}'")
   "[\n]" t " ")
  "List of boot-logs available to journalctl.")


(defvar journalctl-output-list
  '("short-full"
        "short-iso"
        "short-iso-precise"
        "short-precise"
        "short-monotonic"
        "short-unix"
        "verbose"
        "export"
        "json"
        "json-pretty"
        "json-sse"
        "json-seq"
        "cat"
        "with-unit")
  "Options for the --output parameter.
It controls the formatting of the journal entries that are shown.")

(defvar journalctl-priority-list
  '("emerg" "0"
    "alert" "1"
    "crit" "2"
    "err" "3"
    "warning" "4"
    "notice" "5"
    "info" "6"
    "debug" "7"
    "emerg..alert" "alert..emerg" "0..1" "1..0"
    "emerg..crit" "crit..emerg" "0..2" "2..0"
    "emerg..err" "err..emerg" "0..3" "3..0"
    "emerg..warning" "warning..emerg" "0..4" "4..0"
    "emerg..notice" "notice..emerg" "0..5" "5..0"
    "emerg..info" "info..emerg" "0..6" "6..0"
    "emerg..debug" "debug..emerg" "0..7" "7..0"
    "alert..crit" "crit..alert" "1..2" "2..1"
    "alert..err" "err..alert" "1..3" "3..1"
    "alert..warning" "warning..alert" "1..4" "4..1"
    "alert..notice" "notice..alert" "1..5" "5..1"
    "alert..info" "info..alert" "1..6" "6..1"
    "alert..debug" "debug..alert" "1..7" "7..1"
    "crit..err" "err..crit" "2..3" "3..2"
    "crit..warning" "warning..crit" "2..4" "4..2"
    "crit..notice" "notice..crit" "2..5" "5..2"
    "crit..info" "info..crit" "2..6" "6..2"
    "crit..debug" "debug..crit" "2..7" "7..2"
    "err..warning" "warning..err" "3..4" "4..3"
    "err..notice" "notice..err" "3..5" "5..3"
    "err..info" "info..err" "3..6" "6..3"
    "err..debug" "debug..err" "3..7" "7..3"
    "warning..notice" "notice..warning" "4..5" "5..4"
    "warning..info" "info..warning" "4..6" "6..4"
    "warning..debug" "debug..warning" "4..7" "7..4"
    "notice..info" "info..notice" "5..6" "6..5"
    "notice..debug" "debug..notice" "5..7" "7..5"
    "info..debug" "debug..info" "6..7" "7..6")
  "List of log-levels / priorities used by journalctl.")

(defvar journalctl-output-fields-list
  (split-string
   (shell-command-to-string "journalctl --fields")
   "[\n]" t " ")
  "List of output-fields available to journalctl.")

(defvar journalctl-facility-list
  (split-string
   (shell-command-to-string "journalctl --facility=help")
   "[\n]" t " ")
  "List of facilities available to journalctl.")

(defvar journalctl-follow-timer
  nil
  "Timer for follow simulation.")


;; functions
(defun journalctl--disk-usage ()
  "Disk-usage of journalctl."
  (let ((cmd-out (shell-command-to-string "journalctl --disk-usage")))
    (if (string-match "[0-9.]+\\(T\\|G\\|M\\)" cmd-out)
        (match-string 0 cmd-out)
      "0G")))

(transient-define-infix journalctl-transient:--lines ()
		       :description "Limit number of events."
		       :class 'transient-option
		       :shortarg "n"
		       :argument "--lines="
		       )

(transient-define-infix journalctl-transient:--outputs ()
		       :description "Controls the formatting."
		       :class 'transient-option
		       :shortarg "o o"
		       :argument "--output="
		       :choices journalctl-output-list
		       )

(transient-define-infix journalctl-transient:--field ()
  :description "Print all values for a specific field."
  :class 'transient-option
  :shortarg "o f"
  :argument "--field="
  :choices journalctl-output-fields-list
  )

(transient-define-infix journalctl-transient:--boot ()
		       :description "Show boot log."
		       :class 'transient-option
		       :shortarg "c b"
		       :argument "--boot="
		       :choices journalctl-boot-list
		       )

(transient-define-infix journalctl-transient:--unit ()
		       :description "Show log for specific systemd unit."
		       :class 'transient-option
		       :shortarg "c u"
		       :argument "--unit="
		       :choices journalctl-unit-list
		       )

(transient-define-infix journalctl-transient:--user-unit ()
		       :description "Show log for specific systemd user unit."
		       :class 'transient-option
		       :shortarg "c U"
		       :argument "--user-unit="
		       :choices journalctl-user-unit-list
		       )

(transient-define-infix journalctl-transient:--priority ()
		       :description "Filter output by message priorities (ranges)."
		       :class 'transient-option
		       :shortarg "c p"
		       :argument "--priority="
		       :choices journalctl-priority-list
		       )

(transient-define-infix journalctl-transient:--identifier ()
		       :description "Show messages for specified syslog identifier."
		       :class 'transient-option
		       :shortarg "c t"
		       :argument "--identifier="
		       )

(transient-define-infix journalctl-transient:--facility ()
		       :description "Filter output by syslog facility."
		       :class 'transient-option
		       :shortarg "c f"
		       :argument "--facility="
		       :choices journalctl-facility-list
		       )

(transient-define-infix journalctl-transient:--since ()
		       :description  "Start showing entries on or newer than the specified date."
		       :class 'transient-option
		       :shortarg "S"
		       :argument "--since="
		       :prompt "Since (2012-10-30 18:17:16|yesterday|today|now|+|-):"
		       )

(transient-define-infix journalctl-transient:--until ()
		       :description  "Start showing entries on or older than the specified date."
		       :class 'transient-option
		       :shortarg "U"
		       :argument "--until="
		       :prompt "Until (2012-10-30 18:17:16|yesterday|today|now|+|-):"
		       )

(transient-define-infix journalctl-transient:--machine ()
		       :description  "Show messages from a running, local container."
		       :class 'transient-option
		       :shortarg "s m"
		       :argument "--machine="
		       )

(transient-define-infix journalctl-transient:--directory ()
		       :description  "Directory on which journalctl shall operate."
		       :class 'transient-option
		       :shortarg "s d"
		       :argument "--directory="
		       )

(transient-define-infix journalctl-transient:--file ()
		       :description  "File glob on which journalctl shall operate."
		       :class 'transient-option
		       :shortarg "s f"
		       :argument "--file="
		       )

(transient-define-infix journalctl-transient:--root ()
		       :description  "Directory root on which journalctl shall operate."
		       :class 'transient-option
		       :shortarg "s r"
		       :argument "--root="
		       )

(transient-define-infix journalctl-transient:--image ()
		       :description  "Image file on which journalctl shall operate."
		       :class 'transient-option
		       :shortarg "s i"
		       :argument "--image="
		       )

(transient-define-infix journalctl-transient:--namespace ()
		       :description  "namespace on which journalctl shall operate."
		       :class 'transient-option
		       :shortarg "s i"
		       :argument "--namespace="
		       )

(transient-define-prefix journalctl-transient ()
  "Transient for journalctl."
  [["Output"
   ("o a" " Show all fields in full."
   ("o l" "Ellipsize fields when they do not fit." "--no-full")
    "--all")
   ("o m" "Show entries interleaved from all available journals." "--merge")
   (journalctl-transient:--field)
   ("o q" "Suppresses all informational messages." "--quiet")
   ("o u" "Express time in Coordinated Universal Time (UTC)." "--utc")
   (journalctl-transient:--outputs)
   ]
  ["Sources"
   (journalctl-transient:--machine)
   (journalctl-transient:--directory)
   (journalctl-transient:--file)
   (journalctl-transient:--root)
   (journalctl-transient:--image)
   (journalctl-transient:--namespace)
   ]]
  [["Constraint"
   ("C k" "Show only kernel messages. This implies -b." "--dmesg")
   ("C s" "Show only system and kernel messages." "--system")
   ("C u" "Show only system and kernel messages." "--user")
   (journalctl-transient:--boot)
   (journalctl-transient:--identifier)
   (journalctl-transient:--unit)
   (journalctl-transient:--user-unit)
   (journalctl-transient:--facility)
   ]
  ["Filters"
   (journalctl-transient:--since)
   (journalctl-transient:--until)
   ("r" "Reverse output (newest entries first)." "--reverse")
   ("x" "Augment log lines with explanations." "--catalog")
   (journalctl-transient:--lines)
   ]]
  ["Aufruf"
   (journalctl-standard-suffix)
   (journalctl-close-menu-suffix)
   (journalctl-follow-suffix)
   ("q" "Quit journalctl." journalctl-quit)
   ])

(transient-define-suffix journalctl-standard-suffix ()
  :transient t
  :key "SPC"
  :description "Run journalctl with transient arguments on current chunk. KEEP menu."
  (interactive)
  (let ((args (transient-args (oref transient-current-prefix command))))
  (journalctl--run args journalctl-current-chunk)))

(transient-define-suffix journalctl-follow-suffix ()
  :transient nil
  :key "f"
  :description "Run journalctl with transient arguments on current chunk in follow mode."
  (interactive)
  (let ((args (transient-args (oref transient-current-prefix command)))
	(follow-args (concat "--lines=" journalctl-follow-lines " -e")))
    (cl-pushnew follow-args args)
    (setq journalctl-follow-timer
	  (run-with-timer journalctl-follow-freq journalctl-follow-freq
			  'journalctl--run args journalctl-current-chunk))))

(transient-define-suffix journalctl-close-menu-suffix ()
  :transient nil
  :key "RET"
  :description "Run journalctl with transient arguments on current chunk. CLOSE menu."
  (interactive)
  (let ((args (transient-args (oref transient-current-prefix command))))
  (journalctl--run args journalctl-current-chunk)))

;; (defun journalctl-opts-to-alist (opt-list)
;;   "Convert the string of command line parameters into a alist (PARAMETER . OPTION)."
;;   (mapcar 'car
;; 	  (mapcar 'cl--plist-to-alist
;; 		  (mapcar
;; 		   (lambda
;; 		     (element)
;; 		     (string-split element "=" nil))
;; 		   opt-list))))

(defun journalctl ()
  "Run journalctl and open transient menu."
  (interactive)
  (journalctl--run '("--lines 250"))
  (sleep-for 0 1) ;; prevent race condition
  (journalctl--run '(""))
  (journalctl-transient))

(defun journalctl--run (transient-opts &optional chunk)
  "Run journalctl with given TRANSIENT-OPTS and present CHUNK of output in a special buffer."
  (interactive (list (transient-args 'journalctl-transient)))
  (setq journalctl-current-opts transient-opts)
  (let* ((opts (mapconcat 'identity transient-opts " "))
	 (lines (string-to-number (shell-command-to-string (concat "journalctl " opts "| wc -l"))))
	 (this-chunk (or chunk 0)) ;; if chunk is not explicitly given, we assume the first (0) chunk
         (first-line (+ 1 (* this-chunk journalctl-chunk-size)))
         (last-line (if (<= (+ first-line journalctl-chunk-size) journalctl-current-lines)
                        (+ first-line journalctl-chunk-size)
                      journalctl-current-lines)))
    (with-current-buffer (get-buffer-create "*journalctl*")
      (switch-to-buffer "*journalctl*")
      (setq buffer-read-only nil)
      (fundamental-mode)
      (erase-buffer)
      (save-window-excursion
	(shell-command
	 (concat "journalctl " opts " | sed -ne '"
		 (int-to-string first-line) ","
		 (int-to-string last-line) "p'")
         "*journalctl*" "*journalctl-error*"))
      (setq buffer-read-only t)
      (setq journalctl-current-lines lines)
      (journalctl-mode))))

;;;;;; Moving and Chunks

(defun journalctl-next-chunk ()
  "Load the next chunk of journalctl output to the buffer."
  (interactive)
  (let* ((chunk (if  (> (* (+ 2 journalctl-current-chunk)
			   journalctl-chunk-size)
			journalctl-current-lines)
		    journalctl-current-chunk
		  (+ journalctl-current-chunk 1) )))
	(setq journalctl-current-chunk chunk)
	(journalctl--run journalctl-current-opts chunk)))

(defun journalctl-previous-chunk ()
  "Load the previous chunk of journalctl output to the buffer."
  (interactive)
  (let ((chunk (if (>= journalctl-current-chunk 1)
		   (- journalctl-current-chunk 1) 0)))
	(setq journalctl-current-chunk chunk)
	(journalctl--run journalctl-current-opts chunk)))

(defun journalctl-scroll-up ()
  "Scroll up journalctl output.
Move to next chunk when bottom of frame is reached."
  (interactive)
  (let ((target-line (+ (array-current-line) 25)))
    (if (>= target-line journalctl-current-lines)
	(message "%s" "End of journalctl output")
      (if (>= target-line journalctl-chunk-size)
	(journalctl-next-chunk)
      (forward-line 25)))))

(defun journalctl-scroll-down ()
  "Scroll down journalctl output.
Move to next chunk when bottom of frame is reached."
  (interactive)
  (let ((target-line (- (array-current-line) 25)))
    (if (<= target-line 0)
	(if (<=  journalctl-current-chunk 0)
	    (message "%s" "Beginn of journalctl output")
	  (journalctl-previous-chunk)))
    (forward-line  -25)))

;;;;;;;; Special functions
(defun journalctl-quit ()
  "Quit journalctl session."
  (interactive)
  (if (timerp journalctl-follow-timer)
      (cancel-timer journalctl-follow-timer))
  (setq journalctl-current-chunk 0)
  (kill-buffer  "*journalctl*"))


;;;;;;;;;;;;;;;;; Fontlock


(defvar journalctl-font-lock-keywords
      (let* (
            ;; generate regex string for each category of keywords
	     (error-keywords-regexp
	      (regexp-opt journalctl-error-keywords 'words))
	     (warn-keywords-regexp
	      (regexp-opt journalctl-warn-keywords 'words))
	     (starting-keywords-regexp
	      (regexp-opt journalctl-starting-keywords 'words))
	     (finished-keywords-regexp
	      (regexp-opt journalctl-finished-keywords 'words)))
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
    (define-key map (kbd "+") 'journalctl)
    (define-key map (kbd "-") 'journalctl)
    ;;
    (define-key map (kbd "n") 'journalctl-next-chunk)
    (define-key map (kbd "p") 'journalctl-previous-chunk)
    ;;
    (define-key map (kbd "C-v") 'journalctl-scroll-up)
    (define-key map (kbd "M-v") 'journalctl-scroll-down)
    (define-key map (kbd "q")  'journalctl-quit)
    map)
  "Keymap for journalctl mode.")

(defvar mode-line-process nil "Process status in the mode line.")
(defvar font-lock-defaults nil "Defaults for Font Lock mode specified by the major mode.")

;;;###autoload
(define-derived-mode journalctl-mode fundamental-mode "journalctl"
  "Major mode for viewing journalctl output."
  (setq mode-line-process
	(concat " (disk usage: "
		(journalctl--disk-usage)
		")"))
  ;; code for syntax highlighting
  (setq font-lock-defaults '((journalctl-font-lock-keywords))))


;; add the mode to the `features` list
(provide 'journalctl-mode)

;;; journalctl-mode.el ends here

;; Local Variables:
;; jinx-languages: "en"
;; jinx-local-words: "journalctl"
;; End:
