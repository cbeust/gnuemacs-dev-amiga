; Autodocs.el, by Cedric BEUST (beust@sa.inria.fr)
;    V1.3, Jan 7th 1992
;    Consult an autodoc function from emacs (18.58, Amiga port 1.26DG)
;    Main entry point : autodoc-show-autodoc (assigned to F2)
;
; The function is searched in the file *xref-file* somewhere in the Amigaguide
; path (setenv Amigaguide/path "path1 path2 path3..."). If you don't have AD2HT
; you can generate a compatible file that must have two words by line : the
; function name and its library, like :
;    "AllocAbs()"			"exec"		
;    "AllocAslRequest()"		"asl"		
; First, an Amigaguide file is searched through the Amigaguide path and displayed
; in Emacs' public screen (see the variable *emacs-public-screen*). If no
; Amigaguide file could be found, the autodocs are searched in the directory
; *autodocs-directory* (NB : the new V39 format is assumed, i.e. every file at
; the same level, no more "LibrariesA-K/", "LibrariesL-Z/", etc....) and the
; corresponding file is loaded and shown at the right place (see
; *autodoc-function-re*)
;
; All functions implemented here are prefixed by autodoc-
;
; OBSCURE FEATURES :
;
; KNOWN BUGS :
;
; IMPROVEMENTS SINCE PREVIOUS VERSION :
;   o can identify a word correctly even if it contains an _ or if the cursor
;     is on the first or last character
;   o use read shell-commands instead of Arexx hack (I needed a more recent
;     fifo-handler)


; Variables used here

; The cross-reference file, that must be somewhere in the Amigaguide path
(defvar *xref-file* "Autodocs.XRef")

; Name of Emacs' public screen
(defvar *emacs-public-screen* "EMACS")

; Where to find the #?.doc
(defvar *autodocs-directory* "Autodocs:")

; Regular expression to find the definition of a function
(defvar *autodoc-function-re* "^.+library/")

; Assign the main function to F2

(global-set-key "\C-x\C-^K" (make-sparse-keymap))
(global-set-key "\C-x\C-^1~" 'autodoc-show-autodoc)

(defun autodoc-word-under-cursor ()
  "Return the word under the cursor."
  (let ((m1 0) (validchars "[a-zA-Z0-9_]"))
    (save-excursion
      (while (and (looking-at validchars)
		  (not (= (point-min) (point))))
	(backward-char 1))
      (setq m1 (point))
      (while (and (looking-at validchars)
		  (not (= (point-max) (point))))
	(forward-char 1))
      (buffer-substring m1 (point)))))

(defun autodoc-exists-amigaguide-file (file)
  "If the given file exists in the Amigaguide path, return its full path
   else return nil"
  (let ((paths (getenv "Amigaguide/path"))
	(c 0)
	(blank-pos 0)
	(path ""))
    (catch 'EXIT
      (save-window-excursion
	(while (and (> (length paths) 0)
		    (setq blank-pos (string-match " " paths)))
	  (setq path (substring paths 0 blank-pos))
	  (setq paths (substring paths (1+ blank-pos)))
	  (if (not (string-match "[:/]" (substring path (1- (length path)))))
	      (setq path (concat path "/")))
	  (if (file-readable-p (concat path file))
	      (throw 'EXIT (concat path file)))
	) ; outter while
      ) ; save
    ) ; catch
  ) ; let
) ; defun


(defun autodoc-shell-command (command)
  "Launch the specified command"
  (save-window-excursion
    (shell-command command)))

(defun autodoc-show-amigaguide-file (file function)
;;  (print-debug (concat "Amigaguide Screen=EMACS Doc=" function "() " file))
;;  (print-debug (concat " showing " file " " function))
  (autodoc-shell-command (concat "run >NIL: Amigaguide Screen="
				 *emacs-public-screen*
			     " Doc=" function "() " file)))

(defun autodoc-show-autodoc ()
  "Show autodoc for the function under the cursor. First check if an Amigaguide
   file is available, else display it in another emacs buffer."
  (interactive)
  (let* ((xref (autodoc-exists-amigaguide-file *xref-file*))
	 (m1 0)
	 (function (autodoc-word-under-cursor))
	 (guide-file "")
	 (command (concat "search " xref " " function))
	)

    (if (not xref) (progn
		     (message (concat "Couldn't open the file " *xref-file*)))
      (autodoc-shell-command command)
      (message (concat "Looking for " function))
      (save-window-excursion
	(switch-to-buffer "*Shell Command Output*")
	(beginning-of-buffer)
	(search-forward (concat "\"" function))
	(beginning-of-line)
	(search-forward "\"" (point-max) t 3)
	(setq m1 (point))
	(search-forward "\"")
	(setq guide-file (buffer-substring m1 (1- (point)))))

      (if (autodoc-exists-amigaguide-file guide-file)
	  (autodoc-show-amigaguide-file guide-file function)
	(find-file (concat *autodocs-directory* guide-file ".doc"))
	(switch-to-buffer (concat guide-file ".doc"))
	(re-search-forward (concat *autodoc-function-re* function))
	(next-line 1))
    )
  )
)

