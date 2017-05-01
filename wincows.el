;;; wincows.el --- display and switch Emacs window configurations

;; Copyright (C) 2002-2008  Juri Linkov <juri@jurta.org>

;; Author: Juri Linkov <juri@jurta.org>
;; Keywords: windows
;; Version: 1.1

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this package; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The command `wincows' displays a list of window configurations with
;; names of their buffers separated by comma.  You can switch to a window
;; configuration selected from this list.

;; To create a new window configuration, run `wincows' and start creating
;; new windows without selecting an existing window configuration (you can
;; leave the current list with `q' for clarity.)

;;; Suggested keybindings

;; (define-key global-map "\e\t" 'wincows)
;; (define-key wincows-mode-map "\e\t" 'wincows-select)
;; (define-key wincows-mode-map "\t" 'wincows-next-line)
;; (define-key wincows-mode-map [backtab] 'wincows-prev-line)

;;; Code:

;;; Customizable User Options

(defgroup wincows nil
  "Display and switch Emacs window configurations."
  :group 'windows)

;;; Internal Variables

(defvar wincows-list nil
  "A list of window configurations.")

(defvar wincows-column 3)

;;; Key Bindings

(defvar wincows-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'quit-window)
    (define-key map "\C-m" 'wincows-select)
    (define-key map "d" 'wincows-delete)
    (define-key map "k" 'wincows-delete)
    (define-key map "\C-d" 'wincows-delete-backwards)
    (define-key map "\C-k" 'wincows-delete)
    (define-key map "x" 'wincows-execute)
    (define-key map " " 'wincows-next-line)
    (define-key map "n" 'wincows-next-line)
    (define-key map "p" 'wincows-prev-line)
    (define-key map "\177" 'wincows-backup-unmark)
    (define-key map "?" 'describe-mode)
    (define-key map "u" 'wincows-unmark)
    (define-key map [mouse-2] 'wincows-mouse-select)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for `wincows-mode' buffers.")

;;; Mode

(define-derived-mode wincows-mode nil "Window Configurations"
  "Major mode for selecting a window configuration.
Each line describes one window configuration in Emacs.
Letters do not insert themselves; instead, they are commands.
\\<wincows-mode-map>
\\[wincows-mouse-select] -- select window configuration you click on.
\\[wincows-select] -- select current line's window configuration.
\\[wincows-delete] -- mark that window configuration to be deleted, and move down.
\\[wincows-delete-backwards] -- mark that window configuration to be deleted, and move up.
\\[wincows-execute] -- delete or save marked window configurations.
\\[wincows-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[wincows-backup-unmark] -- back up a line and remove marks."
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun wincows-current (error-if-non-existent-p)
  "Return window configuration described by this line of the menu."
  (let* ((where (save-excursion
		  (beginning-of-line)
		  (+ 2 (point) wincows-column)))
	 (wincow (and (not (eobp)) (get-text-property where 'wincow))))
    (or (and wincow (window-configuration-p (nth 0 wincow)) wincow)
        (if error-if-non-existent-p
            (error "No window configuration on this line")
          nil))))

;;; Commands

(defun wincows ()
  "Display a list of window configurations with names of their buffers.
The list is displayed in a buffer named `*Wincows*'.

In this menu of window configurations so you can delete or select them.
Type ? after invocation to get help on commands available.
Type q to remove the window configuration menu from the display.

The first column shows `D' for for a window configuration you have
marked for deletion."
  (interactive)
  (switch-to-buffer (wincows-noselect))
  (message "Commands: d, x; RET; q to quit; ? for help."))

(defun wincows-next-line (&optional arg)
  (interactive)
  (next-line arg))

(defun wincows-prev-line (&optional arg)
  (interactive)
  (previous-line arg))

(defun wincows-unmark (&optional backup)
  "Cancel all requested operations on window configuration on this line and move down.
Optional prefix arg means move up."
  (interactive "P")
  (beginning-of-line)
  (move-to-column wincows-column)
  (let* ((buffer-read-only nil))
    (delete-char 1)
    (insert " "))
  (forward-line (if backup -1 1))
  (move-to-column wincows-column))

(defun wincows-backup-unmark ()
  "Move up and cancel all requested operations on window configuration on line above."
  (interactive)
  (forward-line -1)
  (wincows-unmark)
  (forward-line -1)
  (move-to-column wincows-column))

(defun wincows-delete (&optional arg)
  "Mark window configuration on this line to be deleted by \\<wincows-mode-map>\\[wincows-execute] command.
Prefix arg is how many window configurations to delete.
Negative arg means delete backwards."
  (interactive "p")
  (let ((buffer-read-only nil))
    (if (or (null arg) (= arg 0))
        (setq arg 1))
    (while (> arg 0)
      (delete-char 1)
      (insert ?D)
      (forward-line 1)
      (setq arg (1- arg)))
    (while (< arg 0)
      (delete-char 1)
      (insert ?D)
      (forward-line -1)
      (setq arg (1+ arg)))
    (move-to-column wincows-column)))

(defun wincows-delete-backwards (&optional arg)
  "Mark window configuration on this line to be deleted by \\<wincows-mode-map>\\[wincows-execute] command
and then move up one line.  Prefix arg means move that many lines."
  (interactive "p")
  (wincows-delete (- (or arg 1))))

(defun wincows-execute ()
  "Delete window configurations marked with \\<wincows-mode-map>\\[wincows-delete] commands."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil))
      (while (re-search-forward
              (format "^%sD" (make-string wincows-column ?\040))
              nil t)
	(forward-char -1)
	(let ((wincow (wincows-current nil)))
	  (when wincow
            (setq wincows-list (delete wincow wincows-list))
            (beginning-of-line)
            (delete-region (point) (progn (forward-line 1) (point))))))))
  (beginning-of-line)
  (move-to-column wincows-column))

(defun wincows-select ()
  "Select this line's window configuration.
This command deletes and replaces all the previously existing windows
in the selected frame."
  (interactive)
  (let* ((wincow (wincows-current t))
         (bl (delq nil (mapcar (lambda (b) (and (buffer-live-p b) b))
                               (nth 3 wincow))))
         (bbl (delq nil (mapcar (lambda (b) (and (buffer-live-p b) b))
                                (nth 4 wincow)))))
    ;; Delete the selected window configuration
    (setq wincows-list (delete wincow wincows-list))
    (kill-buffer (current-buffer))
    (modify-frame-parameters (selected-frame) (list (cons 'buffer-list bl)))
    (modify-frame-parameters (selected-frame) (list (cons 'buried-buffer-list bbl)))
    (set-window-configuration (nth 0 wincow))
    ;; set-window-configuration does not restore the value
    ;; of point in the current buffer, so restore that separately.
    (when (and (markerp (nth 1 wincow))
               (marker-buffer (nth 1 wincow))
               ;; After dired-revert, marker relocates to 1.
               ;; window-configuration restores point to global point
               ;; in this dired buffer, not to its window point,
               ;; but this is slightly better than 1.
               (not (eq 1 (marker-position (nth 1 wincow)))))
      (goto-char (nth 1 wincow)))))

(defun wincows-mouse-select (event)
  "Select the window configuration whose line you click on."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (wincows-select))

;;; *Wincows* Buffer Creation

(defun wincows-noselect ()
  "Create and return a buffer with a list of window configurations.
The list is displayed in a buffer named `*Wincows*'.

For more information, see the function `wincows'."
  (wincows-add-current)
  ;; WORKAROUND FOR A BUG IN NEXT-LINE
  (pop-to-buffer (get-buffer-create "*Wincows*")) (delete-other-windows)
  (with-current-buffer (get-buffer-create "*Wincows*")
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; Vertical alignment to the center of the frame
    (insert-char ?\n (/ (- (frame-height) (length wincows-list) 1) 2))
    ;; Horizontal alignment to the center of the frame
    (setq wincows-column (- (/ (frame-width) 2) 15))
    (dolist (wincow wincows-list)
      (insert (propertize
               (format "%s %s\n"
                       (make-string wincows-column ?\040)
                       (propertize
                        (nth 2 wincow)
                        'mouse-face 'highlight
                        'help-echo "mouse-2: select this window configuration"))
               'wincow wincow)))
    (wincows-mode)
    (goto-char (point-min))
    (goto-char (or (next-single-property-change (point) 'wincow) (point-min)))
    (when (> (length wincows-list) 1)
      (wincows-next-line))
    (move-to-column wincows-column)
    (set-buffer-modified-p nil)
    (delete-other-windows)
    (current-buffer)))

(defun wincows-add-current ()
  "Add current Emacs window configuration to the list."
  (interactive)
  (let ((current (list
                  (current-window-configuration)
                  ;; set-window-configuration does not restore the value
                  ;; of point in the current buffer, so record that separately.
                  (point-marker)
                  (mapconcat
                   (lambda (w) (buffer-name (window-buffer w)))
                   (window-list)
                   ", ")
                  (delq nil (mapcar
                             (lambda (b) (and (buffer-live-p b) b))
                             (frame-parameter (selected-frame)
                                              'buffer-list)))
                  (delq nil (mapcar
                             (lambda (b) (and (buffer-live-p b) b))
                             (frame-parameter (selected-frame)
                                              'buried-buffer-list))))))
    (setq wincows-list (cons current wincows-list))))

(provide 'wincows)

;;; wincows.el ends here
