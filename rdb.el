;;; rdb.el --- Ruby debugger interface to inf-ruby

;; Copyright (C) 2021 Richard Sharman
;; Author: Richard Sharman <rsharman@pobox.com>
;; Keywords: ruby emacs debug inf-ruby

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; rdb extends inf-ruby by adding debugging features, based on
;; enhancements to the standard debug.rb file.  It provides automatic
;; displaying of the current location when at the debugger,  supports
;; breakpoint conditions and commands,  and repositioning breakpoints
;; after file modification.

;; To install:
;; * copy this file and inf-ruby.el to somewhere on emacs's load path.
;; * optionally: byte-compile the 2 files
;; * install ruby file debug-extra.rb (required) and
;;   optionally debug-format-frame.rb and enhanced-breakpoints.rb.
;;   Either place these in a directory on ruby's $LOAD_PATH or
;;   set variable `rdb-ruby-dir' (which you can customize) to
;;   the directory where these ruby file reside.
;;
;; Add to your ~/.emacs file:
;; (autoload 'rdb "rdb" "Run inf-ruby-mode with enhanced debug support." t)

;; Use command `rdb' to create a new inf-ruby process or,
;; if run in a comint buffer that is running a ruby program that has
;; stopped at the debugger, then set that buffer's mode to inf-ruby.


(require 'inf-ruby)
(eval-when-compile (require 'subr-x))

(defgroup rdb nil
  "Debug environment extensions for inf-ruby" :group 'inf-ruby)

(defcustom rdb-debug-extra-file
  "debug-extra.rb"
  "File required to add ruby functions to debug.rb"
  :type '(string)
  )

(defcustom rdb-ruby-dir
  nil
  "Directory containing extra ruby code.  
If non-nil, this is added to $LOAD_PATH requiring rdb-debug-extra-file."
  :type '(choice (const :tag "Off" nil)
		 (directory)
  ))

(defcustom rdb-add-to-history t
  "If non-nil, certain commands that are sent and echo'd are added to the comint history."
  :type 'boolean)

(defcustom rdb-show-file-and-line-info nil
  "If non-nil, show file and line info output by rdb.
Normally set to nil since this info is used to display the file & line in the other window."
  :type 'boolean)

;; When `rdb' calls `inf-ruby' it may enter the debugger, and if so can attempt to send
;; the continue commands to get back to the inf prompt.  A small delay is required between
;; commands here,  and these timings can be tweaked with the rdb-return-to-inf-delay-x
;; variables.
;; Alternatively, you can disable this by setting rdb-try-and-return-from-debugger to nil.

(defcustom rdb-try-and-return-from-debugger t
  "If non-nil, after the initial require debug send `c' commands until return to the repl.
This code is problematic so set to nil to avoid it!"
  :type 'boolean)

(defcustom rdb-return-to-inf-delay-1 0.2
  "Initial delay in rdb-return-to-inf before examining prompt."
  :type 'number)

(defcustom rdb-return-to-inf-delay-2 0.2
  "Delay in rdb-return-to-inf between examining prompts."
  :type 'number)

(defcustom rdb-hook nil
  "Hook run each time command `rdb' is called after the buffer is setup."
  :type 'hook)

;; End of user-customizeable stuff.

(defconst ruby-debug-marker-regexp "\\(\032\032\\)\\(.+\\):\\([0-9]+\\):\\s-*\n"
  "Regexp to match file and line info put out by debug.rb")

(defvar-local rdb-ruby-pid nil "Pid of ruby process in an rdb buffer.
When rdb is invoked in an existing shell buffer, this pid is monitored: when the process
terminates the buffer is reverted to its previous mode.")

(defvaralias 'rdb-active-buffer 'inf-ruby-buffer  "The active rdb buffer, the last used one.
If there are multiple rdb buffers, this records that last one used,
and is used to determine which process to send commands to.")

(defvar-local rdb-bkpt-alist nil "An alist of breakpoints.
Car is breakpoint number,  cdr is a marker.")

(defvar rdb-display-current-file t "Show location in other window when stopped in debugger.
Normally t, set to nil to avoid displaying debugger-setup stuff.")

(defvar rdb-inf-ruby-menu-updated nil
  "Set non-nil when rdb-update-inf-menu has been called.")

(defun rdb-goto-line (line)
  "Like goto-line but does not change mark."
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line)))) 
 
(defun rdb-show-file-and-line-with-arrow (file line)
  "Show FILE in other window with arrow at line LINE"
  (if (file-exists-p file)
      (let ((win (selected-window)))
	(save-excursion
	  (find-file-other-window file)
	  (rdb-goto-line line)
	  (setq overlay-arrow-position (point-marker))
	  (select-window win)))
    (message "Cannot find file %s" file)))

(defun rdb-debug-filter (string)
  ;; Return the text to be inserted after processing it.
  (if (string-match ruby-debug-marker-regexp string)
      (let ((filename (match-string 2 string))
	    (lineno (string-to-number (match-string 3 string))))
	(if (string-match "<internal:\\(.*\\)>$" filename)
	    (setq filename (match-string 1 filename)))
	(if rdb-show-file-and-line-info
	    (setq string (replace-match "" nil nil string 1))
	  (setq string (replace-match "" nil nil string)))
	(if rdb-display-current-file
	    (rdb-show-file-and-line-with-arrow filename lineno))
	))
  string)

(defun rdb-buffer ()
  "Return the active rdb buffer."
  ;; If rdb-active-buffer is nil, we could try and find a suitable one???
  (or rdb-active-buffer
      (error "no active rdb process found")))

(defun rdb-proc ()
  (get-buffer-process (rdb-buffer)))

(defun rdb-prompt (&optional buffer)
  "Return the current prompt for buffe BUFFER if there is one, else nil.
If no buffer given use the active rdb buffer."
  (or buffer (setq buffer (rdb-buffer)))
  (let ((inhibit-field-text-motion t) prompt)
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-max))
	(setq prompt (buffer-substring-no-properties (line-beginning-position)(point-max)))
	(if (string-match inf-ruby-prompt-pattern prompt)
	    prompt
	  nil)
      ))))

(defun rdb-at-debugger-prompt (&optional buffer)
  "Return non-nil if at debugger prompt for buffer BUFFER.
If no buffer given use the active rdb buffer."
  (let ((prompt (rdb-prompt buffer)))
    (and prompt (string-match "(rdb.*)" prompt))))

(defun rdb-send-string (cmd &optional history)
  "Send string CMD to ruby.  Add a trailing newline if not present.
If optional argument HISTORY is non-nil record it in the comint history."
  (comint-send-string  (rdb-proc)
		       (if (string-suffix-p "\n" cmd)
			   cmd
			 (concat cmd "\n")))
  (if history
      (with-current-buffer (rdb-buffer)
	(comint-add-to-input-history cmd))))

(defun rdb-send-buffer ()
  "Send this buffer to the active rdb process."
  (interactive)
  (let ((string (buffer-substring-no-properties (point-min)(point-max))))
    (rdb-send-string string)))
  
;; Another way to enter debugger from irb is
;; (interrupt-process (rdb-proc) )
;; but that can be problematic.  I forget why now.

(defun rdb-enter-debugger ()
  "Enter the debugger (debug.rb) unless we are already at its prompt."
  (interactive)
  (with-current-buffer (rdb-buffer)
    (unless (rdb-at-debugger-prompt)
      (rdb-send-string "DEBUGGER__.interrupt" rdb-add-to-history))))

(defun rdb-cmd-output (command &optional regexp group echo noerror)
  "Send COMMAND to ruby process, return list of matching GROUP of REGEXPs
If optional REGEXP is ommited, returns a list of lines.
For just non empty lines, use ^.+$
If optional GROUP is specified, 0 (entire regexp), is assumed.
If optional argument ECHO is non nil, display first result.
If no matching output is received show error output in other window unless NOERROR.
"
  (let ((reply (comint-redirect-results-list-from-process
		(rdb-proc) command (or regexp "^.*$") (or group 0))))
    (if reply
	(if echo
	    (message "%s" (car reply)))
      (unless noerror
	;; (error "Error sending command %s" command)
	(message "Error sending command %s" command)
	(switch-to-buffer-other-window " *Comint Redirect Work Buffer*")
	))
    reply))

(defun rdb-cmd-return-value (command &optional max-lines)
  "Send COMMAND to ruby process, return the command's response.
This can be nil, a number or a string.
This ignores the command's output, just the value irb or the debugger prints.

If output is more than MAX-LINES this is probably an error, so complain.
"
  (let* ((regexp "=> \\(.*\\)")
	 (num 1) reply)
    (if (rdb-at-debugger-prompt)
	(setq regexp ".+"
	      num 0))
    (setq reply (comint-redirect-results-list-from-process
		(rdb-proc)
		command
		regexp num
		))
    (unless reply
      (error "Error sending command %s" command))
    (when (and max-lines (> (length reply) max-lines))
      (switch-to-buffer-other-window " *Comint Redirect Work Buffer*")
      (error "Unexpected reply from command %s" command))
    (setq reply (car (last reply)))
    (cond
     ((equal reply "nil")
      nil)
     ((string-match "^[0-9]+$" reply)
      (string-to-number reply))
     ((string-match "^\"\\(.*\\)\"$" reply)
      ;; Remove outer double quotes,  replace \ with nothing except
      ;; don't replace \\ so that things like \n and \t work
      ;; (replace-regexp-in-string "\\\\" "" (match-string 1 reply)))
      (replace-regexp-in-string
       "\\(\\\\\\)[^\\]" "" (match-string 1 reply) t t 1))
     (t reply))
    ))

(defun rdb-file-and-line (&optional nocheck)
  "Return a list of the buffer file and line number.
Unless option argument NOCHECK is non-nil, complain if this is not ruby file."
  (let ((file (buffer-file-name))
	(line (line-number-at-pos)))
    (unless file
      (error "no file for current buffer"))
    (unless nocheck
      ;; Would (unless (string-match "\.rb" file) be better?
      (unless (equal major-mode 'ruby-mode)
	(error "not in an .rb file")))
   (list file line)))
  
(defun rdb-sq(str)
  "Return a single quoted string or "nil"
Replace any embedded single quotes with a quoted one."
  (if str
      (concat "'" (replace-regexp-in-string "'" "\\\\'" str) "'")
    "nil"))

(defun rdb-breakpoint (create &optional file line)
  "Return breakpoint number if there is a breakpoint at location.
If argument CREATE is non-nil then create one if there is none there.
Use line number LINE of file FILE if specified, otherwise use point.
Return the breakpoint number or nil."
  (or file
      (let ((fal (rdb-file-and-line)))
	(setq file (pop fal))
	(setq line (pop fal))))
  (let ((cmd (format "rdb_bk_num('%s', %d, %s)"
		     file line (if create "true" "false")))
	reply num)
    (setq num (rdb-cmd-return-value cmd 2))
    (if num
	(rdb-record-brkpt num file line))
    num))

(defun rdb-add-breakpoint (&optional file line)
  "Add breadkpoint at point when called interactively, or at given FILE or LINE if specified."
  (interactive)
  (message "Breakpoint %d" (rdb-breakpoint t)))

(defun rdb-remove-breakpoint (&optional file line)
  "Remove breakpoint from current line, or from given FILE or LINE if specified."
  (interactive)
  (or file
      (let ((fal (rdb-file-and-line)))
	(setq file (pop fal)
	      line (pop fal))))
  (let ((num (rdb-breakpoint nil))
	command)
    (if (null num)
	(message "No breakpoint here")
      (setq command (format "rdb_bk_remove(%d)" num))
      (if (rdb-cmd-output command nil nil t)
	  (with-current-buffer (rdb-buffer)
	    (assq-delete-all num rdb-bkpt-alist))))
    ))

(defun rdb-add-condition (&optional condition)
  "Add a condition to breakpoint at current line.
When called interactively the condition is prompted for.
Enter it without any surrouding quotes.
Enter just `none' (again, without quotes) to remove any condition."
  (interactive)
  (let* ((num (rdb-breakpoint t))
	 old-condition)
    (unless num
      (error "Unable to find or place breakpoint at given line"))
    (setq old-condition (rdb-cmd-return-value (format "rdb_bk_condition(%d)" num)))
    (or condition
	(setq condition (read-string "Condition? (or none to remove) " old-condition)))
    (if (or (equal condition "")(equal condition "none"))
	(setq condition nil))
    (let ((cmd (format "rdb_bk_set_condition(%d, %s)" num
		       (rdb-sq condition))))
      (if (rdb-cmd-output cmd "Breakpoint.*" 0 t)
	  (rdb-record-brkpt num nil nil condition))
      )))

(defun rdb-add-command (&optional command)
  "Add a command to breakpoint at current line.
When called interactively the command is prompted for.
Enter it without any surrouding quotes.
Enter just `none' (again, without quotes) to remove any command."
  (interactive)
  (let* ((num (rdb-breakpoint t))
	 old-command)
    (unless num
      (error "Unable to find or place breakpoint at given line"))
    (setq old-command (or (rdb-cmd-return-value (format "rdb_bk_command(%d)" num)) ""))
    (or command
	(setq command (read-string "Command? (or none to remove) " old-command)))
    (if (or (equal command "")(equal command "none"))
	(setq command nil))
    (let ((cmd (format "rdb_bk_set_command(%d, %s)" num
		       (rdb-sq command))))
      (if (rdb-cmd-output cmd "Breakpoint.*" 0 t)
	  (rdb-record-brkpt num nil nil nil command))
    )))
    
(defun rdb-list-breakpoints ()
  "List the breakpoints, along with any conditions or command."
  (interactive)
  (rdb-send-string "rdb_bk_list" rdb-add-to-history))

(defun rdb-delete-all-breakpoints ()
  (interactive)
  (rdb-send-string "rdb_zap_breakpoints" rdb-add-to-history))

(defun rdb-show-this-location ()
  "Show this file and line number in other window.
Point is assumed to be on a line containing file and line infor, 
for example on the output of listing of breakpoints."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\([^[:space:]]+\\):\\([0-9]+\\)"
			   (line-end-position) t)
	(let ((file (match-string-no-properties 1))
	      (line (string-to-number (match-string-no-properties 2))))
	  (if (string-match "<internal:\\(.*\\)>$" file)
	      (setq file (match-string 1 file)))
	  (rdb-show-file-and-line-with-arrow file line)))
  ))

(defun rdb-return-to-inf ()
  "While we are sitting at the debugger prompt, send `c' commands
until we are back at the inf-ruby prompt."
  (interactive)
  (let ((rdb-show-file-and-line-info t)
	(rdb-display-current-file nil)
	prompt)
    (with-current-buffer (rdb-buffer)
      (goto-char (point-max))
      ;; Delay a little in case re-using an existing buffer and
      ;; the old prompt may be visible.
      (sit-for rdb-return-to-inf-delay-1)
      (while (progn
	       ;; Wait until we're at a prompt
	       (while (not (setq prompt (rdb-prompt)))
		 (sit-for rdb-return-to-inf-delay-2))
	       (if (string-match "(rdb.*)" prompt)
		   (progn
		     (rdb-send-string "c")
		     (sit-for 0.1)
		     t)
		 nil)))
      )
      (rdb-send-string "") ;; to show the inf prompt
      ))

(defun rdb-show-rdb-buffer ()
  (interactive)
  (let ((buff (rdb-buffer)))
    (if buff
	(switch-to-buffer-other-window buff)
      (message "no rdb buffer active"))))

(defun rdb-listor (list1 list2)
  "Return a list composed of or-ing elements of each list."
  (let ((result nil) either)
    (while (or list1 list2)
      (setq either (or (car list1)(car list2)))
      (add-to-list 'result either t)
      (setq list1 (cdr list1)
	    list2 (cdr list2)))
    result
    ))

(defun rdb-iterate-over-breakpoints (func)
  "Retrieve breakpoints from rdb, iterate over them calling FUNC.
The function is called with 5 arguments:  breakpoint number, 
file, line number, condition and command.   Either of the last two may be nil."
  (let ((lines (rdb-cmd-output "rdb_bk_list"))
	(regexp "\\s-*\\([0-9]+\\)\\s-*\\(.*\\):\\([0-9]+\\)\\(\\s-*if \\(.*\\)\\)?")
	line)
  (while (setq line (pop lines))
    (if (string-match regexp line)
      (let ((num (string-to-number (match-string 1 line)))
	    (file (match-string 2 line))
	    (linenum (string-to-number (match-string 3 line)))
	    (condition (match-string 5 line))
	    (command nil))
	  (if (and lines (string-match "^\\s-+Runs:\\s-*\\(.*\\)" (car lines)))
	    (setq command  (match-string 1 (pop lines))))
	  (funcall func num file linenum condition command))
      ))
  ))

(defun rdb-record-brkpt (n &rest args)
  "Record breakpoint number N in rdb-bkpt-alist.
ARGS consists of 2 to 4 arguments: file, line, condition and command."
  (with-current-buffer (rdb-buffer)
    (let ((old (assoc n rdb-bkpt-alist))
	  file line marker new value)
      (setq file (pop args)
	    line (pop args))
      (if file
	  (setq marker (with-current-buffer (find-file-noselect file)
			 (rdb-goto-line line)
			 (point-marker)))
	(setq marker nil))
      (setq new (list n marker (pop args)(pop args)))
      (setq value (rdb-listor old new))
      (if old
	  (setcdr old (cdr value))
	(add-to-list 'rdb-bkpt-alist value t))
      )))

(defun rdb-record-breakpoints (&optional no-reset)
  "Save breakpoints in an alist suitable for rdb-reposition-breakpoints.
Unless optional argument NO-RESET is non-nil, clear the alist before updating it.

Breakpoints added through emacs are automatically recorded;  however, breakpoints
put in by the debugger are not."
  (interactive "P")
  (unless no-reset
    (with-current-buffer (rdb-buffer)
      (setq rdb-bkpt-alist nil)))
  (rdb-iterate-over-breakpoints 'rdb-record-brkpt)
  )

(defun rdb-record-missing-breakpoints ()
  (interactive)
  (let ((n 0))
    (with-current-buffer (rdb-buffer)
      (rdb-iterate-over-breakpoints
       '(lambda (num file line cond command)
	  (if (assoc num rdb-bkpt-alist)
	      (message "ok, found %d" num)
	    (message "Adding missing breakpoint %d" num)
	    (setq n (1+ n))
	    (rdb-record-brkpt num file line cond command)
	    ))))
    (message "%d breakpoint%s added" n (if (= n 1) "" "s"))
    ))

(defun rdb-sort-alist ()
  "Sort rdb-bkpt-alist by order of breakpoint number."
  (with-current-buffer (rdb-buffer)
    (setq rdb-bkpt-alist (sort rdb-bkpt-alist (lambda (x y) (< (car x) (car y)))))))

(defun rdb-update-inf-menu ()
  "Add entries for rdb to the inf-ruby-minor-mode menu.

Each separator line needs a unique label.
If separator is first or last item it is ignored, so the first item here 
does not appear on the stand alone menu, but will separate the
new entries if added to an existing map."
  (interactive)
  (unless (boundp 'inf-ruby-minor-mode-map)
    (error "inf-ruby-minor-mode-map does not exist.  Run inf-ruby first."))
  (let ((map (lookup-key inf-ruby-minor-mode-map [menu-bar Inf-Ruby ])))
    (define-key-after map [separator-0] '(menu-item "--double-line")    )
    (define-key-after map [add-brkpt] '("Add breakpoint here" . rdb-add-breakpoint))
    (define-key-after map [add-condition] '( "Add condition here" . rdb-add-condition ))
    (define-key-after map [add-command] '( "Add command here" . rdb-add-command ))
    (define-key-after map [remove-brkpt] '("Remove breakpoint here" . rdb-remove-breakpoint))
    (define-key-after map [show-brkpt] '("List breakpoints" . rdb-list-breakpoints))
    (define-key-after map [separator-1] '(menu-item "--single-line"))
    (define-key-after map [record-missing-brkpts] '( "Record missing breakpoints" . rdb-record-missing-breakpoints) )
    (define-key-after map [reposition-brkpts] '( "Reposition breakpoints" . rdb-reposition-breakpoints) )
    (define-key-after map [delete-all-brkpts] '( "Delete all breakpoints" . rdb-delete-all-breakpoints) )
    (define-key-after map [separator-2] '(menu-item "--single-line"))
    (define-key-after map [enter-debugger] '( "Enter debugger" . rdb-enter-debugger ))
    )
  ;; Add keybindings
  (define-key inf-ruby-minor-mode-map (kbd "C-x C-a C-a") 'rdb-list-breakpoints)
  (define-key inf-ruby-minor-mode-map (kbd "C-x C-a C-b") 'rdb-add-breakpoint)
  (define-key inf-ruby-minor-mode-map (kbd "C-x C-a C-d") 'rdb-remove-breakpoint)
  (define-key inf-ruby-minor-mode-map (kbd "C-x C-a C-o") 'rdb-show-rdb-buffer)
  )

(defun rdb-find-kids(ppid)
  "Find processes whose parent pid is ppid."
  (let ((sp  (list-system-processes))
	pid result)
    (while sp
      (setq pid (pop sp))
      (if (equal ppid (cdr (assoc 'ppid (process-attributes pid))))
	  (add-to-list 'result pid)))
    result
    ))

(defun rdb-shell-ruby-pid ()
  "Return the pid of the ruby process running in this shell buffer."
  ;; Assuming current buffer is running a shell,  if it is running ruby
  ;; then return ruby's pid;  else nil.
  (let ((p (get-buffer-process (current-buffer)))
	ppid kids pid)
    (if p
	(setq ppid (process-id p)))
    (when ppid
      (setq kids (rdb-find-kids ppid))
      (while (and kids (null pid))
	(if (equal "ruby" (cdr (assoc 'comm (process-attributes (car kids)))))
	    (setq pid (car kids))
	  (pop kids)))
      )
    pid))

(defun rdb-pid-alive (pid)
  "Return non-nil if PID is alive."
  (let ((pa (and pid (process-attributes pid))))
    (and pa (member (cdr (assoc 'state pa)) '( "R" "S" "T")))
    ))

;; Adding this to post-command-hook ensures that rdb-active-buffer contains
;; the most recently used rdb buffer.
(defun rdb-make-inactive (buff)
  ;; make this no longer active
  (if (buffer-live-p buff)
      (with-current-buffer buff
	(message "buffer %s becomes inactive" (buffer-name buff))
	(if (equal (substring mode-name -1) "*")
	    (progn
	      (with-current-buffer buff
		(setq mode-name (substring mode-name 0 -1))
		(force-mode-line-update)))))
    ))
    
(defun rdb-make-active (buff)
  ;; make this the active buffer
  ;; nothing to do if this is the active buffer
  (unless (eq rdb-active-buffer buff)
    (if rdb-active-buffer
	(rdb-make-inactive rdb-active-buffer))
    (setq rdb-active-buffer buff)
    (message "buffer %s becomes active" (buffer-name buff))
    (with-current-buffer buff
      (setq mode-name (concat mode-name "*"))
      (force-mode-line-update))))
  
(defun rdb-post-cmd-function ()
  (let ((ok (comint-check-proc (current-buffer))))
    (if rdb-ruby-pid
      (unless (rdb-pid-alive rdb-ruby-pid)
	(message (format "Ruby process %d has exited" rdb-ruby-pid))
	(rdb-restore-mode)
	(setq rdb-ruby-pid nil)
	(setq ok nil)
	))
    (if ok
	;; The process is alive.  Make it the active buffer
	(unless (eq rdb-active-buffer (current-buffer))
	  (rdb-make-active (current-buffer)))
      ;; Buffer is no longer ok
      (if (eq rdb-active-buffer (current-buffer))
	  (progn
	    (rdb-make-inactive (current-buffer))
	    (setq rdb-active-buffer nil)
	    )
	;; Don't do this again
	(remove-hook 'post-command-hook 'rdb-post-cmd-function t)
	))))

(defun rdb-save-brkpts (&optional reset)
  "Return breakpoints as a string of lines of commands.
Line numbers are recomputed from the markers stored in the alist, so they
are updated if the file was edited."
  (let ((output ""))
    (with-current-buffer (rdb-buffer)
      ;; (rdb-sort-alist) ;; not necessary
      (let (newnum n m cmd buff)
	(when reset 
	  ;; (rdb-cmd-output "rdb_zap_breakpoints" "All breakpoints removed" 0)
	  (setq output "rdb_zap_breakpoints\n")
	  (setq newnum 1))
	(dolist (item rdb-bkpt-alist)
	  (let* ((num (pop item))
		 (m (pop item))
		 (buff (marker-buffer m))
		 (file (buffer-file-name buff))
		 (line (with-current-buffer buff
			 (line-number-at-pos (marker-position m))))
		 cmd)
	    (if newnum
		(setq num newnum
		      newnum (1+ newnum)))
	    (if (null item)
		(setq cmd
		      (format "rdb_bk_reposition(%d, '%s', %d)\n" num file line))
	      (setq cmd
		    (format "rdb_bk_reposition(%d, '%s', %d, %s, %s)\n"
			    num file line (rdb-sq (pop item)) (rdb-sq (pop item)))))
	    (message "%s" cmd)
	    ;; (rdb-cmd-output cmd)
	    (setq output (concat output cmd))
	    ))
	;;	(if reset
	;;	    (rdb-record-breakpoints))
	))
    output
    ))

(defun rdb-save-brkpts-to-buffer (&optional reset)
  "Save breakpoints in a buffer *Brkpts*.
This may be saved to a file to be used in a later session."
  (interactive "P")
  (with-current-buffer (get-buffer-create "*Brkpts*")
    (erase-buffer)
    (insert (rdb-save-brkpts reset))))

(defun rdb-reposition-breakpoints (&optional reset)
  "Move or put back breakpoints.  Optional arg RESET menans restart from number 1."
  (interactive "P")
  (let ((cmd (rdb-save-brkpts reset)))
    (rdb-send-string cmd)))

    
;;;###autoload
(defun rdb (force)
  (interactive "P")
  "Switch to inf-ruby-mode after running inf-ruby if necessary.
If buffer is running a process, check that it is sitting at the debugger prompt
unless optional argument FORCE is non-nil (prefix if called interactively)."
  (let ((create-new-buffer (not (comint-check-proc (current-buffer))))
	(was-at-inf-prompt t))
    (if create-new-buffer
	(inf-ruby)
      (if (eq major-mode 'inf-ruby-mode)
	  (setq was-at-inf-prompt (not (rdb-at-debugger-prompt (current-buffer))))
	(let ((pid (rdb-shell-ruby-pid)))
	  (unless force
	    (unless pid
	      (error "No ruby process found for this buffer."))
	    (unless (rdb-at-debugger-prompt (current-buffer))
	      (error "This process is not stopped at the rdb prompt")))
	  (setq was-at-inf-prompt nil)
	  (inf-ruby-switch-from-compilation)
	  ;; rdb-ruby-pid being non-nil indicates we should switch back the mode afterwards,
	  ;; see rdb-restore-mode.  Note that inf-ruby-switch-from-compilation kills all local variables
	  ;; (because it changes the major mode).
	  (setq rdb-ruby-pid pid)
	  )))
    (setq rdb-active-buffer (current-buffer))
    (add-hook 'post-command-hook 'rdb-post-cmd-function nil t)
    (if rdb-ruby-dir
	(let ((cmd (format "$LOAD_PATH.unshift('%s') unless $LOAD_PATH.include?('%s')"
			   rdb-ruby-dir rdb-ruby-dir)))
	  (rdb-send-string cmd)))
    (rdb-send-string (format "require '%s'" rdb-debug-extra-file))
    (if (and rdb-try-and-return-from-debugger
	     was-at-inf-prompt)
	(rdb-return-to-inf))
    (add-hook 'comint-preoutput-filter-functions 'rdb-debug-filter 0 'local)
    ;; We only need to do this once:
    (unless rdb-inf-ruby-menu-updated
      (rdb-update-inf-menu)
      (setq rdb-inf-ruby-menu-updated t))
    (run-hooks 'rdb-hook)
    ))

(defun rdb-restore-mode ()
  "Restore previous mode if rdb-mode entered from rdb-mode-this-buffer."
  ;; Normally called from post-command-hook but could be called directly.
  (interactive)
  (inf-ruby-maybe-switch-to-compilation)
  ;; In case this is called directly, remove the post-command function.
  (remove-hook 'post-command-hook 'rdb-post-cmd-function t))

(provide 'rdb)
