;; Run cpr under Emacs
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Original of the gdb package : W. Schelter, University of Texas
;;     wfs@rascal.ics.utexas.edu
;; Rewritten by rms.

;; Some ideas are due to  Masanobu. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These lines describe the changes to gdb.el that were made in order to create
;; a CPR-compatible package.
;;
;; Author of the CPR package : C. Beust, Bull/Inria, Sophia Antipolis, Nice
;;     beust@sa.inria.fr

;; M-s steps by one line, and redisplays the source file and line.
;; M-SPC steps one source line over functions
;; M-c   continues normal running
;;
;; All the other commands were kept as is except the ones that don't exist in
;; CPR (frame up/down for example).
;;
;; Instead of the shell mode, I chose to use comint.el as an underlying mode
;; for CPR operations. comint.el is far ahead shell.el and using this latter
;; is now pure heresy...
;;
;; Main advantages brought by the comint mode :
;;
;; M-p  Recall previous line of input (like ^P under a regular shell)
;; M-n  Recall next line of input (like ^N under a regular shell)
;;
;; Press C-hm for in the *cpr* buffer for more information on comint.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Description of CPR interface:

;; A facility is provided for the simultaneous display of the source code
;; in one window, while using cpr to step through a function in the
;; other.  A small arrow in the source window, indicates the current
;; line.

;; Starting up:

;; In order to use this facility, invoke the command CPR to obtain a
;; shell window with the appropriate command bindings.  You will be asked
;; for the name of a file to run.  Cpr will be invoked on this file, in a
;; window named *cpr-foo* if the file is foo.


;; You may easily create additional commands and bindings to interact
;; with the display.  For example to put the cpr command next on \M-n
;; (def-cpr next "\M-n")

;; This causes the emacs command cpr-next to be defined, and runs
;; cpr-display-frame after the command.

;; cpr-display-frame is the basic display function.  It tries to display
;; in the other window, the file and line corresponding to the current
;; position in the cpr window.  For example after a cpr-step, it would
;; display the line corresponding to the position for the last step.  Or
;; if you have done a backtrace in the cpr buffer, and move the cursor
;; into one of the frames, it would display the position corresponding to
;; that frame.

;; cpr-display-frame is invoked automatically when a filename-and-line-number
;; appears in the output.


(require 'comint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions were added for CPR portability
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This regexp matches a function name followed by its arguments. It sets the
;; meta-argument to the name of the function.
(defvar cpr-function-name-regexp "\\([^(]+\\)([^)]*)")

(defun cpr-function-name (name line)
  "This should belong to C-mode. It returns the name of the function which
   contains 'line' in the buffer 'name'. Might be bogus since the only way
   I found to locate this function is by assuming it is followed by '^{'"
  (save-window-excursion
    (save-excursion
      (save-restriction
	(widen)
	(if (re-search-backward "^{" 0 t)
	    (if (re-search-backward cpr-function-name-regexp)
		(progn
		  (backward-word 1)
		  (re-search-forward cpr-function-name-regexp)
		  (buffer-substring (match-end 1) (match-beginning 1))
		  ) ;; progn
	      "** couldn't locate function with regexp **")
	  "** couldn't locate function with ^{ **")
	) ;; save-restriction
      ) ;; save-excursion
  ) ;; save-window-excursion
)


;; This regexp matches the format of the coordinates as displayed by CPR. For
;; example :      'prog:\prog.c\main 29'
;; The meta-argument 1 is set to the source name (prog.c) and number 2 is the
;; line number
(defvar cpr-location-regexp "^[^0-9:]+:.\\([^\\]+\\)[^ ]+ \\([0-9]+\\)")

(defun cpr-transform-string (string)
  (cpr-perform-transformation string)
)


(defun cpr-perform-transformation (string)
  "Convert a cpr-string (prog:\prog.c\main 29) into a gdb-string
   (\032\032prog.c:29:CHARPOS). CHARPOS is set to zero since it doesn't
   seem to be used by the package (might induce a bug)."
  (let ((result string))
    (if (string-match cpr-location-regexp string)
	(progn
	  (setq result
		(concat
		 "\032\032"
		 (substring string (match-beginning 1) (match-end 1))
		 ":"
		 (substring string (match-beginning 2) (match-end 2))
		 ":"
		 "0"
		)
	   ); setq
	  ) ;; progn
    ) ;; if
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gdb-old :
;; (defvar cpr-prompt-pattern "^(.*cpr[+]?) *"
(defvar cpr-prompt-pattern "^>"
  "A regexp to recognize the prompt for cpr or cpr+.") 

(defvar cpr-mode-map nil
  "Keymap for cpr-mode.")

(if cpr-mode-map
   nil
  (setq cpr-mode-map (copy-keymap comint-mode-map))
  (define-key cpr-mode-map "\C-l" 'cpr-refresh))

(define-key ctl-x-map " " 'cpr-break)
(define-key ctl-x-map "&" 'send-cpr-command)

;;Of course you may use `def-cpr' with any other cpr command, including
;;user defined ones.   

(defmacro def-cpr (name key &optional doc)
  (let* ((fun (intern (format "cpr-%s" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'cpr-call cstr))
	  (list 'define-key 'cpr-mode-map key  (list 'quote fun)))))

(def-cpr "proceed"   "\M-n" "Step one source line with display")
(def-cpr "cont"   "\M-c" "Continue with display")

(def-cpr "finish" "\C-c\C-f" "Finish executing current function")

(defun cpr-mode ()
  "Major mode for interacting with an inferior Cpr process.
The following commands are available:

\\{cpr-mode-map}

\\[cpr-display-frame] displays in the other window
the last line referred to in the cpr buffer.

\\[cpr-step],\\[cpr-next], and \\[cpr-nexti] in the cpr window,
call cpr to step,next or nexti and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[cpr-break].

Commands:
Many commands are inherited from shell mode. 
Additionally we have:

\\[cpr-display-frame] display frames file in other window
\\[cpr-step] advance one line in program
\\[cpr-next] advance one line in program (skip over calls).
\\[send-cpr-command] used for special printing of an arg at the current point.
C-x SPACE sets break point at current line."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'cpr-mode)
  (setq mode-name "Inferior Cpr")
  (setq mode-line-process '(": %s"))
  (use-local-map cpr-mode-map)
  (make-local-variable 'comint-last-input-start)
  (setq comint-last-input-start (make-marker))
  (make-local-variable 'comint-last-input-end)
  (setq comint-last-input-end! (make-marker))
  (make-local-variable 'cpr-last-frame)
  (setq cpr-last-frame nil)
  (make-local-variable 'cpr-last-frame-displayed-p)
  (setq cpr-last-frame-displayed-p t)
  (make-local-variable 'cpr-delete-prompt-marker)
  (setq cpr-delete-prompt-marker nil)
  (make-local-variable 'cpr-filter-accumulator)
  (setq cpr-filter-accumulator nil)
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp cpr-prompt-pattern)
  (run-hooks 'shell-mode-hook 'cpr-mode-hook)
)

(defvar current-cpr-buffer nil)

(defvar cpr-command-name "cpr"
  "Pathname for executing cpr.")

(defun cpr (path)
  "Run cpr on program FILE in buffer *cpr-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for CPR.  If you wish to change this, use
the CPR commands `cd DIR' and `directory'."
;;  (interactive "FRun cpr on file: ")
  (interactive (comint-get-source "Run cpr on file: " () '(c-mode) t))
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*cpr-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
;;gdb-old :
;;    (make-shell (concat "cpr-" file) cpr-command-name nil "-fullname"
;;		"-cd" default-directory file)

;; cpr-new :
    (comint-mode)
    (make-comint (concat "cpr-" file) cpr-command-name nil "-line" file)

    (cpr-mode)
    (set-process-filter (get-buffer-process (current-buffer)) 'cpr-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'cpr-sentinel)
    (cpr-set-buffer)))

(defun cpr-set-buffer ()
  (cond ((eq major-mode 'cpr-mode)
	(setq current-cpr-buffer (current-buffer)))))

;; This function is responsible for inserting output from CPR
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that CPR prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun cpr-filter (proc string)
;; cpr-new :
  (setq string (cpr-transform-string string))
;;
  (let ((inhibit-quit t))
    (if cpr-filter-accumulator
	(cpr-filter-accumulate-marker proc
				      (concat cpr-filter-accumulator string))
	(cpr-filter-scan-input proc string))))

(defun cpr-filter-accumulate-marker (proc string)
  (setq cpr-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (let* ((first-colon (string-match ":" string 2))
			 (second-colon
			  (string-match ":" string (1+ first-colon))))
		    (setq cpr-last-frame
			  (cons (substring string 2 first-colon)
				(string-to-int
				 (substring string (1+ first-colon)
					    second-colon)))))
		  (setq cpr-last-frame-displayed-p nil)
		  (cpr-filter-scan-input proc
					 (substring string (1+ end))))
	      (setq cpr-filter-accumulator string)))
	(cpr-filter-insert proc "\032")
	(cpr-filter-scan-input proc (substring string 1)))
    (setq cpr-filter-accumulator string)))

(defun cpr-filter-scan-input (proc string)
  (if (equal string "")
      (setq cpr-filter-accumulator nil)
      (let ((start (string-match "\032" string)))
	(if start
	    (progn (cpr-filter-insert proc (substring string 0 start))
		   (cpr-filter-accumulate-marker proc
						 (substring string start)))
	    (cpr-filter-insert proc string)))))

(defun cpr-filter-insert (proc string)

  (let ((moving (= (point) (process-mark proc)))
	(output-after-point (< (point) (process-mark proc)))
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer proc))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (setq start (point))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (cpr-maybe-delete-prompt)
	  ;; Check for a filename-and-line number.
	  (cpr-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-line-number appears.
	   t))
      (set-buffer old-buffer))
    (if moving (goto-char (process-mark proc)))))

(defun cpr-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the cpr buffer.
	     (set-buffer obuf))))))


(defun cpr-refresh ()
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive)
  (redraw-display)
  (cpr-display-frame))

(defun cpr-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from CPR.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (cpr-set-buffer)
  (and cpr-last-frame (not nodisplay)
       (or (not cpr-last-frame-displayed-p) (not noauto))
       (progn (cpr-display-line (car cpr-last-frame) (cdr cpr-last-frame))
	      (setq cpr-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun cpr-display-line (true-file line)
  (let* ((buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(defun cpr-call (command)
  "Invoke cpr COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  (setq cpr-delete-prompt-marker (point-marker))
  (cpr-set-buffer)
  (send-string (get-buffer-process current-cpr-buffer)
	       (concat command "\n")))

(defun cpr-maybe-delete-prompt ()
  (if (and cpr-delete-prompt-marker
	   (> (point-max) (marker-position cpr-delete-prompt-marker)))
      (let (start)
	(goto-char cpr-delete-prompt-marker)
	(setq start (point))
	(beginning-of-line)
	(delete-region (point) start)
	(setq cpr-delete-prompt-marker nil))))

(defun cpr-break ()
  "Set CPR breakpoint at this source line."
  (interactive)
  (let* ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(1+ (count-lines 1 (point)))))
	(function (cpr-function-name file-name line)))
;; gdb-old:
;;    (send-string (get-buffer-process current-cpr-buffer)
;;		 (concat "break " file-name ":" line "\n"))))
;; cpr-new :
    (send-string (get-buffer-process current-cpr-buffer)
		 (concat "break \\" file-name "\\" function " " line "\n"))
    (send-string (get-buffer-process current-cpr-buffer)
		 (concat "blist\n"))
    )
)


(defun cpr-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (dot)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(dot)))
     (cond (found (forward-char 2)(setq result
			(buffer-substring found
				 (progn (re-search-forward "[^0-9a-f]")
					(forward-char -1)
					(dot)))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (dot)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (dot)))))))


(defvar cpr-commands nil
  "List of strings or functions used by send-cpr-command.
It is for customization by you.")

(defun send-cpr-command (arg)

  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the cpr buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list cpr-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of cpr-commands.  "


  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg cpr-commands)))
    (setq addr (cpr-read-address))
    (if (eq (current-buffer) current-cpr-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-cpr-buffer)
    (goto-char (dot-max))
    (insert-string comm)))
