;;;; patches  -*- lexical-binding: t -*-

;; This file contains commands that could be added to standard Emacs distribution.

;; TODO: better on right side because it imitates vertical scroll bar
;; BUG: beginning not shown in buffers with header-line active (e.g. Info)
;; (setq-default indicate-buffer-boundaries t)
;; (setq-default indicate-unused-lines t)

;; Customize

;; customize-save is too slow!!!


;;; info

;; TODO: change syntax (C-h s in *Info*)
;; < '               w p        which means: word,
;; > '               w p        which means: punctuation


;;; isearch

;; qv http://thread.gmane.org/gmane.emacs.devel/105836
;; 2008-11-19 Subject: isearch and M-y/C-y
(define-key isearch-mode-map "\C-k" 'isearch-yank-line)
(define-key isearch-mode-map "\M-s\C-y" 'isearch-yank-kill)
;; VERY BAD (conflicts with `forward-sentence'):
;; (define-key isearch-mode-map "\M-s\M-e" 'isearch-edit-string)

;; I don't want to use these Vi-like commands
;; and don't want to propose them as patches!
;; They are proof-of-concept if someone asks how to do this
;; for http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-forward-search-symbol-at-point (&optional count)
  (interactive "p")
  (isearch-forward-symbol-at-point (or count 1)))
(define-key search-map "*" 'isearch-forward-search-symbol-at-point)
(defun isearch-backward-search-symbol-at-point (&optional count)
  (interactive "p")
  (isearch-forward-symbol-at-point (- (or count 1))))
(define-key search-map "#" 'isearch-backward-search-symbol-at-point)

;; list-buffers is not a command which acts on the other window
(put 'list-buffers 'isearch-scroll nil)


;;; query-replace

(define-key query-replace-map "\C-s" 'skip)
(define-key query-replace-map "\C-r" 'backup)
(define-key query-replace-map "\M-e" 'edit-replacement)
(let ((meta-map (make-sparse-keymap)))
  (define-key query-replace-map (char-to-string meta-prefix-char) meta-map)
  (define-key query-replace-map [escape] meta-map))
(define-key query-replace-map "\M-\C-r" 'edit)


;;; indent-tabs-mode

;; Try next:
;; From emacs-devel:"Follow convention for reading with the minibuffer."
(defun indent-tabs-mode-maybe ()
  (if (and (null indent-tabs-mode)
           ;; Trust the major mode.
           (not (local-variable-p 'indent-tabs-mode))
           (save-excursion
             (goto-char (point-min))
             ;; If there are at least 10 lines with a leading TAB, use TABs.
             (re-search-forward "^\t" (+ (point) 100000) t 10)))
      (set (make-local-variable 'indent-tabs-mode) t)))
;; (add-hook 'find-file-hook 'indent-tabs-mode-maybe)


;;; trailing-whitespace

(defvar my-show-trailing-whitespace-dirs nil
  "Show trailing whitespace in all files located under specified subdirectories.")

(defun my-show-trailing-whitespace ()
  "Show trailing whitespace only in my own files under my home directory,
so that I can correct them."
  (if (and my-show-trailing-whitespace-dirs
           buffer-file-name
           (not buffer-read-only)
           (string-match (concat "^\\(" (mapconcat
                                         'identity
                                         my-show-trailing-whitespace-dirs
                                         "\\|")
                                 "\\)")
                         buffer-file-name))
      (setq show-trailing-whitespace t)))

;; Decide whether to show trailing whitespace after buffer is loaded from file
(add-hook 'find-file-hook 'my-show-trailing-whitespace)

;; Never show trailing whitespace in picture mode and view mode
(add-hook 'picture-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'view-mode-hook    (lambda () (setq show-trailing-whitespace nil)))

;; qv (info "(emacs)Rectangles") !!!
;; The right edge of the rectangle does not make
;; any difference to this command.


;;; simple.el

;; FROM `kill-current-buffer' in menu-bar.el
(defun kill-current-buffer-and-dired-jump ()
  "Kill the current buffer and jump to dired buffer corresponding
to killed buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (and (fboundp 'dired-jump) (dired-jump))
    (kill-buffer buffer)))

;; FROM `quit-window' in window.el
(defun quit-window-kill-buffer ()
  "Quit WINDOW and kill its buffer."
  (interactive)
  (quit-window 1))

;; FROM bug#8615
(define-key special-mode-map "q" 'quit-window-kill-buffer)

;; FROM bug#13167
;; (define-key ctl-x-map "q" 'quit-window-kill-buffer)
;; This is replaced by C-q in ./emacs.el

(defun my-delete-blank-lines ()
  "Delete blank lines on region."
  (interactive "*")
  (if (use-region-p)
      (let ((beg (min (point) (mark)))
            (end (max (point) (mark))))
        (save-excursion
          (goto-char beg)
          (while (re-search-forward "\n[ \t]*$" end t)
            (delete-blank-lines))))
    (delete-blank-lines)))
(define-key ctl-x-map "\C-o" 'my-delete-blank-lines)

(defun shell-command-on-region-or-buffer (start end command &optional arg)
  "Execute string COMMAND in inferior shell with region or whole buffer as input."
  (interactive (let ((string
                      (read-shell-command "Shell command on region or buffer: ")))
                 (list (if (use-region-p) (region-beginning) (point-min))
                       (if (use-region-p) (region-end) (point-max))
                       string current-prefix-arg)))
  (shell-command-on-region start end command arg arg))

(defun write-file-or-region ()
  "Write region or file.
With a prefix arg, append the region to the file."
  (interactive)
  (if (use-region-p)
      (write-region (region-beginning) (region-end)
                    (read-file-name
                     (format "%s region to file: "
                             (if current-prefix-arg "Append" "Write")))
                    current-prefix-arg)
    (call-interactively 'write-file)))
(define-key ctl-x-map "\C-w" 'write-file-or-region)


;;; bindings

(add-to-list 'completion-ignored-extensions "RCS/")
(add-to-list 'completion-ignored-extensions ".v")


;;; subr

(defun delete-from-list (list-var element)
  "Delete from the value of LIST-VAR the element ELEMENT if it is there.
The test for presence of ELEMENT is done with `equal'.

If you want to use `delete-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `delete-to-list'
into a hook function that will be run only after loading the package.
`with-eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (set list-var
           (delete element (symbol-value list-var)))
    (symbol-value list-var)))

(delete-from-list 'dired-omit-extensions ".log") ;; this unhides Apache log files
(delete-from-list 'dired-omit-extensions ".bin")
(delete-from-list 'dired-omit-extensions ".dvi")
(delete-from-list 'dired-omit-extensions ".pdf")
;; next also deletes from dired-font-lock-keywords
(delete-from-list 'completion-ignored-extensions ".log")
(delete-from-list 'completion-ignored-extensions ".bin")
(delete-from-list 'completion-ignored-extensions ".dvi")
(delete-from-list 'completion-ignored-extensions ".pdf")

(defun add-list-to-list (list-var element-list &optional append)
  "Add to the value of LIST-VAR the every element of list ELEMENT-LIST
using function `add-to-list'."
  (while element-list
    (add-to-list list-var (car element-list) append)
    (setq element-list (cdr element-list))))

(defcustom read-passwd-hide-delay 0.3
  "Time delay before hiding entered password chars."
  :type 'number
  :group 'display
  :version "27.1")

(defun read-passwd-hide-delay (prompt &optional confirm default)
  "Read a password, prompting with PROMPT, and return it.
If optional CONFIRM is non-nil, read the password twice to make sure.
Optional DEFAULT is a default password to use instead of empty input.

This function echoes `*' for each character that the user types.
You could let-bind `read-hide-char' to another hiding character, though.

Once the caller uses the password, it can erase the password
by doing (clear-string STRING)."
  (if confirm
      (let (success)
        (while (not success)
          (let ((first (read-passwd-hide-delay prompt nil default))
                (second (read-passwd-hide-delay "Confirm password: " nil default)))
            (if (equal first second)
                (progn
                  (and (arrayp second) (not (eq first second)) (clear-string second))
                  (setq success first))
              (and (arrayp first) (clear-string first))
              (and (arrayp second) (clear-string second))
              (message "Password not repeated accurately; please start over")
              (sit-for 1))))
        success)
    (let ((hide-chars-fun
           (lambda (beg end _len)
             (let ((minibuf (current-buffer)))
               (run-with-timer
                read-passwd-hide-delay
                nil
                (lambda ()
                  (clear-this-command-keys)
                  (when (buffer-live-p minibuf)
                    (with-current-buffer minibuf
                      (setq beg (min end (max (minibuffer-prompt-end) beg)))
                      (setq end (min end (point-max)))
                      (dotimes (i (- end beg))
                        (put-text-property (+ i beg) (+ 1 i beg)
                                           'display (string (or read-hide-char ?*))
                                           minibuf)))))))))
          minibuf)
      (minibuffer-with-setup-hook
          (lambda ()
            (setq minibuf (current-buffer))
            ;; Turn off electricity.
            (setq-local post-self-insert-hook nil)
            (setq-local buffer-undo-list t)
            (setq-local select-active-regions nil)
            (use-local-map read-passwd-map)
            (setq-local inhibit-modification-hooks nil) ;bug#15501.
            (setq-local show-paren-mode nil)            ;bug#16091.
            (add-hook 'after-change-functions hide-chars-fun nil 'local)
            ;; (add-hook 'after-change-functions (debounce 1 hide-chars-fun) nil 'local)
            )
        (unwind-protect
            (let ((enable-recursive-minibuffers t)
                  (read-hide-char (or read-hide-char ?*)))
              (read-string prompt nil t default)) ; t = "no history"
          (when (buffer-live-p minibuf)
            (with-current-buffer minibuf
              ;; Not sure why but it seems that there might be cases where the
              ;; minibuffer is not always properly reset later on, so undo
              ;; whatever we've done here (bug#11392).
              (remove-hook 'after-change-functions hide-chars-fun 'local)
              ;; (remove-hook 'after-change-functions (debounce 1 hide-chars-fun) 'local)
              (kill-local-variable 'post-self-insert-hook)
              ;; And of course, don't keep the sensitive data around.
              (erase-buffer))))))))


;;; history

;; Add help items to history

(define-advice describe-function (:before (function))
  "Add function name to the history."
  (when (and function (symbolp function))
    (add-to-history 'minibuffer-history (symbol-name function))))

(define-advice describe-variable (:before (variable &optional _buffer _frame))
  "Add variable name to the history."
  (when (and variable (symbolp variable))
    (add-to-history 'minibuffer-history (symbol-name variable))))

(define-advice describe-symbol (:before (symbol &optional _buffer _frame))
  "Add symbol name to the history."
  (when (and symbol (symbolp symbol))
    (add-to-history 'minibuffer-history (symbol-name symbol))))

;; ALSO TRY FROM gmane.emacs.help 2013-02-11 Subject: Using Emacs' help system:
(with-eval-after-load 'icomplete
  ;; FROM bug#13602
  ;; (setq icomplete-minibuffer-map (make-sparse-keymap))
  ;; (define-key icomplete-minibuffer-map [?\M-\t]           'minibuffer-force-complete)
  ;; (define-key icomplete-minibuffer-map [?\C-j]            'minibuffer-force-complete-and-exit)
  (define-key icomplete-minibuffer-map [(control left)]   'icomplete-backward-completions)
  (define-key icomplete-minibuffer-map [(control right)]  'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map [(control return)] 'icomplete-force-complete-and-exit)
  ;; (define-key icomplete-minibuffer-map [(meta right)]  'icomplete-forward-completions)
  ;; (define-key icomplete-minibuffer-map [(meta left)]   'icomplete-backward-completions)
  ;; (setq icomplete-with-completion-tables t)
  ;; (setq icomplete-with-completion-tables nil)
  )


;;; window.el

;; FROM http://thread.gmane.org/gmane.emacs.devel/159692/focus=159721
(defvar window-resize-keymap
  (let ((map (make-sparse-keymap)))
    ;; Standard keys:
    (define-key map "0" 'delete-window)
    (define-key map "1" 'delete-other-windows)
    (define-key map "2" 'split-window-below)
    (define-key map "3" 'split-window-right)
    (define-key map "o" 'other-window)
    (define-key map "^" 'enlarge-window)
    (define-key map "}" 'enlarge-window-horizontally)
    (define-key map "{" 'shrink-window-horizontally)
    (define-key map "-" 'shrink-window-if-larger-than-buffer)
    (define-key map "+" 'balance-windows)
    ;; Additional keys:
    (define-key map "v"     'shrink-window)
    (define-key map [down]  'shrink-window)
    (define-key map [up]    'enlarge-window)
    (define-key map [left]  'shrink-window-horizontally)
    (define-key map [right] 'enlarge-window-horizontally)
    map)
  "Keymap to resize windows.")
(when (fboundp 'advice-add)
  (advice-add 'enlarge-window-horizontally
              :after (lambda (&rest _args)
                       (set-transient-map window-resize-keymap)))
  (advice-add 'shrink-window-horizontally
              :after (lambda (&rest _args)
                       (set-transient-map window-resize-keymap)))
  (advice-add 'enlarge-window
              :after (lambda (&rest _args)
                       (set-transient-map window-resize-keymap)))
  (advice-add 'shrink-window
              :after (lambda (&rest _args)
                       (set-transient-map window-resize-keymap)))
  (defun window-resize-command ()
    (interactive)
    (message "Use window-resizing keys...")
    (set-transient-map window-resize-keymap))
  (define-key ctl-x-map "wr" 'window-resize-command))


;;; misc.el

(defun copy-line (&optional arg)
  "Copy current line `arg' times."
  (interactive "p")
  (dotimes (_ arg)
    (beginning-of-line)
    (forward-line 1)
    (insert "\n") ;; (open-line 1) because doesn't work on ChangeLog headers
    (forward-line -1)
    (copy-from-above-command)))
(define-key my-map "c" 'copy-line)
;; qv also http://www.emacswiki.org/cgi-bin/wiki/LineCopyChar


;;; cperl-mode

;; add outline-regexp and outline-level to function cperl-mode
(add-hook
 'cperl-mode-hook
 (lambda ()
   (set (make-local-variable 'outline-regexp) "=[^c]")
   (set (make-local-variable 'outline-level)
        (lambda ()
          (save-excursion
            (cond ((looking-at "=head1") 1)
                  ((looking-at "=head2") 2)
                  (t 3)))))))

;; add to generic-x, if not supported by cperl-mode.el
(define-generic-mode 'pod-generic-mode
  nil
  nil
  '(("^=\\w+" 0 'font-lock-function-name-face))
  ;; '(("^=\\w+\\(.*\\)" 1 'font-lock-function-name-face))
  (list "\\.pod\\'")
  (list (lambda ()
          (set (make-local-variable 'search-invisible) nil)
          (set (make-local-variable 'outline-regexp) "=[^c]")
          (set (make-local-variable 'outline-level)
               (lambda ()
                 (save-excursion
                   (cond ((looking-at "=head1") 1)
                         ((looking-at "=head2") 2)
                         ((looking-at "=[^i]") 3)
                         (t 4)))))))
  "Perl POD documentation files.")


;;; sh-script

;; why sh-mode's mode-class=special?
;; if file is read-only, it doesn't come into view mode (qv (after-find-file))
(put 'sh-mode 'mode-class nil)


;;; dired

;; new function

;; next function is made from dired-find-file
;; maybe better to add argument `literally' to dired-find-file?
(defun dired-find-file-literally ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (let ((file-name (file-name-sans-versions (dired-get-filename) t)))
    (if (file-exists-p file-name)
        (find-file-literally file-name)
      (if (file-symlink-p file-name)
          (error "File is a symlink to a nonexistent target")
        (error "File no longer exists; type `g' to update Dired buffer")))))

;; 2 new functions:
(defun dired-next-line-cycle (arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line arg)
  (if (dired-between-files)
      (progn
        (goto-char (point-min))
        ;; code borrowed from dired-mark-files-in-region, TODO: make general function
        (while (and (not (eobp)) (dired-between-files))
          (forward-line 1))))
  (dired-move-to-filename))
(defun dired-previous-line-cycle (arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line (- arg))
  (beginning-of-line)
  (if (dired-between-files)
      (progn
        (goto-char (point-max))
        (while (and (not (bobp)) (dired-between-files))
          (forward-line -1))))
  (dired-move-to-filename))

(defun dired-next-dirline-cycle (arg &optional opoint)
  "Goto ARG'th next directory file line."
  (interactive "p")
  (or opoint (setq opoint (point)))
  ;; go over boundaries back to the beginning
  (if (if (> arg 0)
          (or (re-search-forward dired-re-dir nil t arg)
              (progn
                (goto-char (point-min))
                (re-search-forward dired-re-dir nil t arg)))
        (beginning-of-line)
        (or (re-search-backward dired-re-dir nil t (- arg))
            (progn
              (goto-char (point-max))
              (re-search-backward dired-re-dir nil t (- arg)))))
      (dired-move-to-filename)          ; user may type `i' or `f'
    (goto-char opoint)
    ;; (progn (beep) (message "No more subdirectories")) ; message instead of error ?
    (error "No more subdirectories")))
(defun dired-prev-dirline-cycle (arg)
  "Goto ARG'th previous directory file line."
  (interactive "p")
  (dired-next-dirline-cycle (- arg)))

;; 2 new functions:
(defun dired-get-file-info ()
  "Get file info files for which PREDICATE returns non-nil."
  ;; code for this function is borrowed from dired-x.el::dired-mark-sexp
  (let (inode s mode nlink uid gid size time name sym)
    (save-excursion
      (if (dired-move-to-filename)
          (let (pos
                (mode-len 10)
                (dired-re-inode-size "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?"))
            (beginning-of-line)
            (forward-char 2)
            (if (looking-at dired-re-inode-size)
                (progn
                  (goto-char (match-end 0))
                  (setq inode (string-to-number (buffer-substring (match-beginning 1)
                                                               (match-end 1)))
                        s (string-to-number (buffer-substring (match-beginning 2)
                                                           (match-end 2)))))
              (setq inode nil
                    s nil))
            (setq mode (buffer-substring (point) (+ mode-len (point))))
            (forward-char mode-len)
            (setq nlink (read (current-buffer)))
            (setq uid (buffer-substring (+ (point) 1) (progn (forward-word 1) (point))))
            (skip-chars-forward "[ ]")
            (setq gid (buffer-substring (+ (point) 1) (progn (forward-word 1) (point))))
            (skip-chars-forward "[ ]")
            (skip-chars-forward "[0-9,._]")
            ;; works only with ls patch
            ;; patched in dired.el:dired-move-to-filename-regexp
            ;; (re-search-forward "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)")
            ;; try standard expression
            ;; (re-search-forward directory-listing-before-filename-regexp)
            ;; (goto-char (match-beginning 2))
            ;; (forward-char -1)
            (setq size (string-to-number
                        (replace-regexp-in-string
                         ;; handle thousand separators in sizes
                         "," ""
                         (buffer-substring (save-excursion
                                             ;; (backward-word 1)
                                             (skip-chars-backward "[0-9,.]")
                                             (setq pos (point)))
                                           (point)))))
            (goto-char pos)
            (backward-word 1)
            (setq gid (buffer-substring (save-excursion (forward-word 1) (point))
                                        (point))
                  time (buffer-substring (match-beginning 1)
                                         (1- (dired-move-to-filename)))
                  name (buffer-substring (point)
                                         (or (dired-move-to-end-of-filename t)
                                             (point)))
                  sym  (progn
                         (if (looking-at " -> ")
                             (buffer-substring (progn (forward-char 4) (point))
                                               (progn (end-of-line) (point)))
                           "")))
            (list
             (cons 'inode inode)
             (cons 's s)
             (cons 'mode mode)
             (cons 'nlink nlink)
             (cons 'uid uid)
             (cons 'gid gid)
             (cons 'size size)
             (cons 'time time)
             (cons 'name name)
             (cons 'sym sym)))
        nil))))

(defun dired-count-sizes (&optional mark)
  "Count sizes of files marked by MARK mark."
  (interactive
   (let* ((cursor-in-echo-area t)
          (mark (progn (message "Count files marked by mark: ")
                       (read-char))))
     (list mark)))
  (if (or (eq mark ?\r))
      (ding)
    (let ((string (format "\n%c" mark))
          (buffer-read-only)
          (total-size 0)
          total-size-str
          (total-count 0))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward string nil t)
          (if (if (= mark ?\ )
                  (save-match-data
                    (dired-get-filename 'no-dir t))
                t)
              (if (equal (buffer-substring-no-properties
                          (match-beginning 0) (match-end 0))
                         string)
                  (setq total-size
                        (+ total-size
                           (*;;(/
                            (cdr (assoc 'size (dired-get-file-info)))
                            1.0);;1024)
                           )
                        total-count (+ total-count 1))))))
      (setq total-size-str (replace-regexp-in-string
                            "^," ""
                            (apply 'string
                                   (reverse
                                    (string-to-list
                                     (replace-regexp-in-string
                                      "\\([0-9]\\{3\\}\\)" "\\1,"
                                      (apply 'string
                                             (reverse
                                              (string-to-list
                                               (replace-regexp-in-string
                                                ".0$" ""
                                                (number-to-string
                                                 total-size)))))))))))
      (message "Marked %s files with %s bytes" total-count total-size-str))))

;; recover-session should not omit files,
;; because backup files are listed in `dired-omit-files',
;; and so are not visible is the recover buffer
;; TODO: gives error, if backup dir is empty
(defun recover-session ()
  "Recover auto save files from a previous Emacs session.
This command first displays a Dired buffer showing you the
previous sessions that you could recover from.
To choose one, move point to the proper line and then type C-c C-c.
Then you'll be asked about a number of files to recover."
  (interactive)
  (if (null auto-save-list-file-prefix)
      (error "You set `auto-save-list-file-prefix' to disable making session files"))
  (let ((dir (file-name-directory auto-save-list-file-prefix))
        (nd (file-name-nondirectory auto-save-list-file-prefix)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (directory-files dir nil
                             (if (string= "" nd)
                                 directory-files-no-dot-files-regexp
                               (concat "\\`" (regexp-quote nd)))
                             t)
      (error "No previous sessions to recover")))
  (let ((ls-lisp-support-shell-wildcards t))
    (dired (concat auto-save-list-file-prefix "*")
           (concat dired-listing-switches " -t"))
    (if (bound-and-true-p dired-omit-mode)
        (dired-omit-toggle)))
  (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
  (define-key (current-local-map) "\C-c\C-c" 'recover-session-finish)
  (save-excursion
    (goto-char (point-min))
    (or (looking-at " Move to the session you want to recover,")
        (let ((inhibit-read-only t))
          ;; Each line starts with a space
          ;; so that Font Lock mode won't highlight the first character.
          (insert " To recover a session, move to it and type C-c C-c.\n"
                  (substitute-command-keys
                   " To delete a session file, type \
\\[dired-flag-file-deletion] on its line to flag
 the file for deletion, then \\[dired-do-flagged-delete] to \
delete flagged files.\n\n"))))))

(defun my-dired-jump (&optional other-window)
  "Jump to dired buffer corresponding to source file of symlink."
  (interactive "P")
  (if (and (eq major-mode 'dired-mode)
           (save-excursion (beginning-of-line) (looking-at dired-re-sym)))
      (let ((file (file-chase-links (dired-filename-at-point))))
        (if other-window
            (dired-other-window (file-name-directory file))
          (dired (file-name-directory file)))
        (or (dired-goto-file file)
            ;; refresh and try again
            (progn
              (dired-insert-subdir (file-name-directory file))
              (dired-goto-file file))))
    (dired-jump other-window))
  ;; [2009-12-03] Obsolete due to `dired-auto-revert-buffer'=t
  ;; (revert-buffer)
  )

(defun my-dired-jump-other-window ()
  "Like \\[my-dired-jump] (my-dired-jump) but in other window."
  (interactive)
  (my-dired-jump t))

(defun my-set-insert-directory-program ()
  (when (file-remote-p default-directory)
    (setq dired-actual-switches
          (replace-regexp-in-string "--block-size='1\\|--group-directories-first\\|--time-style=long-iso" ""
                                    dired-actual-switches))))
(add-hook 'dired-before-readin-hook 'my-set-insert-directory-program)

;; qv http://thread.gmane.org/gmane.emacs.devel/118789
;; (setq dired-listing-switches "-Alv --group-directories-first --time-style=long-iso --block-size='1")
;; (setenv "LS_BLOCK_SIZE" "'1")


;;; php-mode

;; FIX stupid php-mode but this doesn't work
;; because php-font-lock-keywords-3 uses syntactic font-lock
;; (add-hook 'php-mode-user-hook
;;           (lambda ()
;;             (modify-syntax-entry ?_ "_" php-mode-syntax-table)))

(add-hook 'php-mode-hook
          (lambda ()
            (add-to-list 'magic-fallback-mode-alist '("<\\?php" . php-mode))
            ;; (modify-syntax-entry ?#  "<\n" php-mode-syntax-table)
            ;; (modify-syntax-entry ?\n ">#"  php-mode-syntax-table)
            (modify-syntax-entry ?#  "<"     php-mode-syntax-table)
            (modify-syntax-entry ?\n ">"     php-mode-syntax-table)
            (modify-syntax-entry ?/  ". 124" php-mode-syntax-table)
            (modify-syntax-entry ?*  ". 23b" php-mode-syntax-table)
            ;; (set (make-local-variable 'comment-start) "# ")
            ;; (set (make-local-variable 'comment-end) "")
            ;; (set (make-local-variable 'comment-start-skip) "#+ *")
            ))

;; FROM http://drupal.org/node/59868
(c-set-offset 'arglist-intro 2)
(add-to-list 'auto-mode-alist '(".*drupal.*\\.info" . conf-windows-mode))


;;; comint

;; FROM http://thread.gmane.org/gmane.emacs.devel/153647/focus=153682
(add-hook 'comint-mode-hook
          (lambda ()
            (require 'compile)
            (setq mode-line-process
                  '(:propertize ":%s" face compilation-mode-line-fail))))

;; ALSO with exit status:
(advice-add 'shell-command-sentinel :after
	    (lambda (process _signal)
	      (with-current-buffer (process-buffer process)
		(setq mode-line-process
		      `(:propertize ,(format ":%s [%s]" "%s" (process-exit-status process))
				    face compilation-mode-line-fail))))
	    '((name . propertize-mode-line)))


;;; eshell

(with-eval-after-load 'em-hist
  ;; next patch should be corrected in eshell-hist-initialize
  (or eshell-history-size
      (setq eshell-history-size
            (string-to-number (or (getenv "HISTSIZE") "32768")))))


;;; debbugs

(defun add-debbugs-headers ()
  "Add debbugs boilerplate in `message-mode'."
  (interactive)
  (message-carefully-insert-headers (list (cons 'Bcc "control@debbugs.gnu.org")))
  ;; (message-sort-headers)
  (message-goto-body)
  (let* ((subject (mail-fetch-field "Subject"))
         (bug (or (and (string-match "[Bb]ug ?#?\\([0-9]+\\)" subject)
                       (match-string 1 subject))
                  "###")))
    (insert (string-join `(,(format "Version: %s" emacs-version)
                           "Severity: serious important normal minor wishlist"
                           "Tags: patch wontfix moreinfo unreproducible fixed notabug security confirmed"
                           "X-Debbugs-Cc:"
                           ,(format "unarchive %s" bug)
                           ,(format "reopen %s" bug)
                           ,(format "severity %s serious important normal minor wishlist" bug)
                           ,(format "tags %s + patch wontfix moreinfo unreproducible fixed notabug security confirmed" bug)
                           ,(format "retitle %s ..." bug)
                           ,(format "found %s %s" bug emacs-version)
                           ,(format "fixed %s %s" bug emacs-version)
                           ,(format "close %s %s" bug emacs-version)
                           "quit|stop|thanks")
                         "\n") "\n\n")))


;;; generic-x

;; renamed from `apache-log-generic-mode' (because not suitable for error_log)
;; and improved
(define-generic-mode 'apache-access-log-generic-mode
  nil
  nil
  ;; remote-host remote-logname remote-user time request status bytes-sent referer user-agent
  '(("^\\([-a-zA-z0-9.]+\\) - [-A-Za-z]+ \\(\\[.*?\\]\\)"
     (1 font-lock-constant-face)
     (2 font-lock-variable-name-face))
    )
  (list "access[_.]log\\'")
  nil
  "Mode for Apache access log files")

;; added new
(define-generic-mode 'apache-error-log-generic-mode
  nil
  nil
  ;; time error-severity IP-address
  '(("^\\(\\[.*\\]\\) \\(\\[.*\\]\\) \\(\\[.*\\]\\)"
     (1 font-lock-variable-name-face)
     (3 font-lock-constant-face))
    ("^\\(\\[.*\\]\\) \\[.*\\]"
     (1 font-lock-variable-name-face))
    )
  (list "error[_.]log\\'")
  (list
   (function
    (lambda ()
      (setq outline-regexp "\\["))))
  "Mode for Apache error log files")


;; simple.el ?

(defun identifier-at-point ()
  "Return a default tag based on the text at point."
  (or (and (boundp 'find-tag-default-function)
           find-tag-default-function
           (funcall find-tag-default-function))
      (and (get major-mode 'find-tag-default-function)
           (funcall (get major-mode 'find-tag-default-function)))
      (save-excursion
        (while (looking-at "\\sw\\|\\s_")
          (forward-char 1))
        (when (or (re-search-backward "\\sw\\|\\s_"
                                      (save-excursion (beginning-of-line) (point))
                                      t)
                  (re-search-forward "\\(\\sw\\|\\s_\\)+"
                                     (save-excursion (end-of-line) (point))
                                     t))
          (goto-char (match-end 0))
          (buffer-substring-no-properties
           (point)
           (progn (forward-sexp -1)
                  (while (looking-at "\\s'")
                    (forward-char 1))
                  (point)))))))


;;; fringe

(cond
 ((boundp 'fringe-indicator-alist)
  (define-fringe-bitmap 'light-down-arrow [32 32 32 32 32 32 168 112 32] nil nil 'bottom)
  (define-fringe-bitmap 'light-up-arrow [32 112 168 32 32 32 32 32 32] nil nil 'top)
  (define-fringe-bitmap 'light-top-left-angle [254 254 128 128 128] nil nil 'top)
  (define-fringe-bitmap 'light-bottom-left-angle [128 128 128 254 254] nil  nil 'bottom)
  (define-fringe-bitmap 'light-left-bracket [254 254 128 128 128 0 0 0 0 128 128 128 254 254] nil nil 'center)
  (define-fringe-bitmap 'light-right-curly-arrow [96 16 8 8 72 80 96 120] nil nil 'bottom)
  (define-fringe-bitmap 'light-left-curly-arrow [8 16 16 16 18 10 6 30] nil nil 'top)
  (define-fringe-bitmap 'light-right-arrow [16 8 252 8 16] nil 11 'center)
  (define-fringe-bitmap 'light-left-arrow [32 64 254 64 32] nil nil 'center)
  (setq-default fringe-indicator-alist
                '((truncation . (light-left-arrow light-right-arrow))
                  (continuation . (light-left-curly-arrow light-right-curly-arrow))
                  (overlay-arrow . right-triangle)
                  (up . light-up-arrow)
                  (down . light-down-arrow)
                  (top . (light-top-left-angle top-right-angle))
                  (bottom . (light-bottom-left-angle bottom-right-angle
                             top-right-angle light-top-left-angle))
                  (top-bottom . (light-left-bracket right-bracket
                                 top-right-angle light-top-left-angle))
                  (empty-line . empty-line)
                  (unknown . question-mark))))
 ((fboundp 'define-fringe-bitmap)
  ;; OLD fringe customization code:
  ;; bitmaps are from emacs-devel Subject: fringe buffer-boundary bitmaps
  (require 'fringe)
  (define-fringe-bitmap 'down-arrow [32 32 32 32 32 32 168 112 32] nil nil 'bottom)
  (define-fringe-bitmap 'up-arrow [32 112 168 32 32 32 32 32 32] nil nil 'top)
  (define-fringe-bitmap 'top-left-angle [254 254 128 128 128] nil nil 'top)
  (define-fringe-bitmap 'bottom-left-angle [128 128 128 254 254] nil  nil 'bottom)
  (define-fringe-bitmap 'left-bracket [254 254 128 128 128 0 0 0 0 128 128 128 254 254] nil nil 'center)

  (define-fringe-bitmap 'continued-line [96 16 8 8 72 80 96 120] nil nil 'bottom)
  (define-fringe-bitmap 'continuation-line [8 16 16 16 18 10 6 30] nil nil 'top)

  ;; (define-fringe-bitmap 'overlay-arrow [16 24 252 254 252 24 16] nil nil 'center)

  ;; (define-fringe-bitmap 'right-truncation [4 2 169 2 4] nil 11 'center)
  ;; (define-fringe-bitmap 'left-truncation [32 64 149 64 32] nil nil 'center)
  (define-fringe-bitmap 'right-truncation [16 8 252 8 16] nil 11 'center)
  (define-fringe-bitmap 'left-truncation [32 64 254 64 32] nil nil 'center)

  ;; (setq-default indicate-buffer-boundaries 'left)
  ))


;;; image

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))


;;; mule

;; The problem is ambiguous mnemonics of buffer coding in mode-line-format.
;; It shows the same character for very different codings such as
;; utf-8 and utf-16le-with-signature
;; Currently used characters:
;; ?=
;; ?-
;; ?@
;; ?$
;; ?*
;; ?&
;; ?0
;; ?1
;; ?2
;; ?3
;; ?4
;; ?5
;; ?6
;; ?7
;; ?8
;; ?9
;; ?A
;; ?b
;; ?B
;; ?c
;; ?C
;; ?D
;; ?E
;; ?g
;; ?G
;; ?h
;; ?i
;; ?j
;; ?J
;; ?k
;; ?K
;; ?L
;; ?M
;; ?q
;; ?Q
;; ?R
;; ?S
;; ?t
;; ?T
;; ?u
;; ?U
;; ?v
;; ?V
;; ?W
;; ?x
;; ?z
;; ?Z

;; This fix uses mnemonics only for familiar codings that are frequently used.
;; Otherwise, it displays the full name of the encodings.
(setq-default mode-line-mule-info
              `(""
                (current-input-method
                 (:propertize ("" current-input-method-title)
                              help-echo (concat
                                         ,(purecopy "Current input method: ")
                                         current-input-method
                                         ,(purecopy "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method"))
                              local-map ,mode-line-input-method-map
                              mouse-face mode-line-highlight))
                (:eval
                 (propertize
                  (cond
                   ((not (memq buffer-file-coding-system
                               '(cyrillic-koi8
                                 cyrillic-koi8-dos
                                 cyrillic-koi8-unix
                                 no-conversion
                                 prefer-utf-8-unix
                                 undecided-unix
                                 utf-8
                                 utf-8-dos
                                 utf-8-emacs
                                 utf-8-emacs-dos
                                 utf-8-emacs-unix
                                 utf-8-unix)))
                    (replace-regexp-in-string
                     "-\\(?:dos\\|unix\\)$" ""
                     (format "%S" buffer-file-coding-system)))
                   (t "%z"))
                  'help-echo 'mode-line-mule-info-help-echo
                  'mouse-face 'mode-line-highlight
                  'local-map mode-line-coding-system-map))
                (:eval (mode-line-eol-desc))))


