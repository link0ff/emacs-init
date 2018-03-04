;;; .emacs --- Emacs init file

;; Copyright (C) 1989-2018  Juri Linkov <juri@linkov.net>

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: dotemacs, init
;; URL: <http://www.linkov.net/emacs/dotemacs>
;; Version: 2018-03-05 for GNU Emacs 27.0.50 (x86_64-pc-linux-gnu)

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;                                             "Show me your ~/.emacs
;;                                    and I will tell you who you are."
;;                                      -- old proverb modified by me


;;; settings

;; Enable all disabled commands (eval-expression, narrow-to-..., etc.)
(setq disabled-command-function nil)

;; Get rid of all space-wasting garbage and minimize clutter
(and (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(and (fboundp 'tooltip-mode) (fboundp 'x-show-tip) (tooltip-mode -1))

;; Blinking cursors are distracting - turn blink OFF
(and (fboundp 'blink-cursor-mode) (blink-cursor-mode (- (*) (*) (*))))

;; Use "y or n" for answers instead of complete words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; If not on AC power line, then display battery status on the mode line
(defvar my-battery-timer nil)
(when (and (require 'battery nil t) (functionp battery-status-function))
  (when (and (boundp 'my-battery-timer) (timerp  my-battery-timer))
    (cancel-timer my-battery-timer))
  (setq my-battery-timer
        ;; Check periodically if went off-line and
        ;; discharging battery needs to be displayed
        (run-at-time
         t 600
         (lambda ()
           (unless (equal (cdr (assoc ?L (funcall battery-status-function))) "on-line")
             (display-battery-mode 1))))))

;; Create display table to modify some display elements
(or standard-display-table (setq standard-display-table (make-display-table)))

;; Display page delimiter ^L as a horizontal line
(aset standard-display-table ?\^L (vconcat (make-vector 64 ?-) "^L"))

;; Display triangle for outline of invisible lines.
;; For information, see (info "(elisp) Display Table Format")
;; (from old code in faces.el in Emacs repo modified for Emacs 23)
(if (facep 'escape-glyph)
    (let* ((face (lsh (face-id 'escape-glyph) 22)) ;; 22 was 19 in Emacs 22
           (backslash (+ face ?\\))
           ;; TRIANGULAR BULLET keeps the default font height
           (dot (+ face #x2023)))
      ;; (aset standard-display-table 2208 (vector backslash ?\s)) ; no-break space
      ;; (aset standard-display-table 2221 (vector backslash ?-))  ; soft hyphen
      ;; (set-char-table-extra-slot standard-display-table 2 backslash) ; \364
      ;; (set-char-table-extra-slot standard-display-table 3 (+ face ?^)) ; ^@
      ;; (set-char-table-extra-slot standard-display-table 4 (vector dot dot dot))
      (set-char-table-extra-slot standard-display-table 4 (vector dot))))

;; Non-customizable variables
(setq gc-cons-percentage 0.3)
(setq print-gensym t)
(setq print-circle t)

;; Tabify only initial whitespace
(setq tabify-regexp "^\t* [ \t]+")

;; For a new non-file buffer set its major mode based on the buffer name.
;; http://thread.gmane.org/gmane.emacs.devel/115520/focus=115794
;; But this has problems, e.g. in autoinsert.el that uses
;; `(eq major-mode (default-value 'major-mode))'.
(setq-default major-mode (lambda ()
                           (if buffer-file-name
                               (fundamental-mode)
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))))


;;; frame

;; To use maximum screen space, my Emacs frame covers the entire screen
;; and has no menus, no toolbars, no scrollbars, no title and no borders.
;; Such customization on 1024x768 display mode and 6x10 font produces
;; Emacs text screen resolution 168 columns x 75 lines.
;; `split-window-horizontally' gives two windows with 83 columns x 75 lines.
;; And `follow-mode' displays one buffer with 83 columns x 150 lines.

(cond
 ((eq window-system 'x)
  ;;(create-fontset-from-ascii-font "-rfx-fixed-medium-r-normal--10-*-*-*-c-60-koi8-*")
  (create-fontset-from-ascii-font "-misc-fixed-medium-r-*--10-*-*-*-*-*-*-*")
  (setq default-frame-alist
        (append
         '(
           ;;TRY: maximize instead of (width . 168)
           ;;TRY: maximize instead of (height . 77)
           ;; This is useful with the next code in the ~/.sawfish/rc,
           ;; because I can't find a way to unframe and maximize Emacs window from Emacs:
           ;; (require 'sawfish.wm.state.maximize)
           ;; (define (my-customize-emacs-window w)
           ;;   (when (string-match "emacs" (nth 2 (get-x-property w 'WM_CLASS)))
           ;;     (window-put w 'type 'unframed)
           ;;     (maximize-window w)))
           ;; (add-hook 'before-add-window-hook my-customize-emacs-window t)
           (font . "-misc-fixed-medium-r-normal--10-*-*-*-c-60-iso8859-1")
           ;;(font . "-*-*-medium-r-normal--10-*-*-*-c-60-fontset-koi8_r_10")
           ;;? (font . "-rfx-fixed-medium-r-normal--10-*-*-*-c-60-koi8-*")
           ;;? (font . "-rfx-fixed-medium-r-normal--10-*-*-*-c-60-*-*")
           ;; (font . "-misc-fixed-medium-r-normal--10-100-75-75-c-60-iso10646-1")
           ;; (font . "-*-*-medium-r-*--10-*-*-*-*-*-fontset-iso8859_1_10")
           (cursor-type . bar)
           ;; To win a lot of screen pixels:
           (vertical-scroll-bars . nil)
           (horizontal-scroll-bars . nil)
           (scroll-bar-width . 0)
           (internal-border-width . 0)
           (menu-bar-lines . 0)
           (tool-bar-lines . 0)
           (line-spacing . 0))
         default-frame-alist))))


;;; tabs

(global-set-key [(control tab)]               'tab-next)
(global-set-key [(control shift iso-lefttab)] 'tab-previous)


;;; mouse

;; Move the mouse to the screen corner on any keypress.
;; This is commented out because now `mouse-avoidance-mode'
;; is customized to `banish' in `custom-set-variables':
;; (when (and (display-mouse-p) (require 'avoid nil t))
;;   ;; Move the mouse to the lower-right corner instead of default upper-right
;;   ;; (defun mouse-avoidance-banish-destination ()
;;   ;;   (cons (+ 3 (frame-width)) (- (frame-height) 1)))
;;   ;; The above is now customized for 24.2+ in `custom-set-variables' as:
;;   ;; (mouse-avoidance-banish-position
;;   ;;  '((frame-or-window . frame) (side . right) (side-pos . -3)
;;   ;;    (top-or-bottom . bottom) (top-or-bottom-pos . 1)))
;;   (mouse-avoidance-mode 'banish))

;; Show the text pointer in void text areas (no need any more)
;; (setq void-text-area-pointer nil)
;; (setq make-pointer-invisible t)


;;; colors

(defun my-colors-light (&optional frame)
  "Set colors suitable for working in light environments,
i.e. in daylight or under bright electric lamps."
  (interactive)
  (setq frame-background-mode 'light)
  (if frame
      (select-frame frame)
    (setq frame (selected-frame)))
  ;; The color with minimal eye fatigue in light environments
  ;; is "AntiqueWhite3" (RGB: 205 192 176),
  ;; (set-background-color "AntiqueWhite3")
  (set-background-color "white")
  (set-foreground-color "black")
  (when (facep 'region)
    (set-face-background 'region "gainsboro" frame))
  (when (facep 'fringe)
    (set-face-background 'fringe (face-background 'default) frame)
    (set-face-foreground 'fringe (face-foreground 'default) frame))
  ;; When started Emacs under root, warn by red color in the modeline
  (when (and (facep 'mode-line)
             ;; Alternative condition is (= (user-uid) 0)
             (file-exists-p "/root")
             (file-writable-p "/root"))
    (set-face-background 'mode-line "firebrick")))

(define-key global-map [f6 ?c ?s] 'my-colors-light)

(defun my-colors-dark (&optional frame)
  "Set colors suitable for working in the darkness without electricity."
  (interactive)
  (setq frame-background-mode 'dark)
  (if frame
      (select-frame frame)
    (setq frame (selected-frame)))
  (set-background-color "black")
  (set-foreground-color "DarkGrey")
  (when (facep 'region)
    (set-face-background 'region "DimGray" frame))
  (when (facep 'fringe)
    (set-face-background 'fringe (face-background 'default) frame)
    (set-face-foreground 'fringe (face-foreground 'default) frame)))

(define-key global-map [f6 ?c ?d] 'my-colors-dark)

(defvar my-sunset-timer nil)
(defvar my-sunrise-timer nil)

;; Automatically switch to dark background after sunset
;; and to light background after sunrise.
;; (Note that `calendar-latitude' and `calendar-longitude'
;;  should be set before calling the `solar-sunrise-sunset')
(defun my-colors-set (&optional frame)
  (interactive)
  (require 'solar)
  (if (and calendar-latitude calendar-longitude calendar-time-zone)
      (let* ((l (solar-sunrise-sunset (calendar-current-date)))
             (sunrise-string (apply 'solar-time-string (car l)))
             (sunset-string (apply 'solar-time-string (car (cdr l))))
             (current-time-string (format-time-string "%H:%M")))
        (if (or (string-lessp current-time-string sunrise-string)
                (string-lessp sunset-string current-time-string))
            (my-colors-dark frame)
          (my-colors-light frame))
        (if (and (boundp 'my-sunset-timer)  (timerp my-sunset-timer))
            (cancel-timer my-sunset-timer))
        (if (and (boundp 'my-sunrise-timer) (timerp my-sunrise-timer))
            (cancel-timer my-sunrise-timer))
        (setq my-sunset-timer  (run-at-time sunset-string  (* 60 60 24)
                                            'my-colors-dark))
        (setq my-sunrise-timer (run-at-time sunrise-string (* 60 60 24)
                                            'my-colors-light)))))

;; (my-colors-set)
;; (add-to-list 'after-make-frame-functions 'my-colors-set)

(add-to-list 'after-make-frame-functions
             'toggle-frame-maximized)


;;; faces

(defun my-faces-fix (&optional frame)
  "Fix defined faces."
  (interactive)
  ;; Check if this function is called by `custom-define-hook' from
  ;; `custom-declare-face' where the variable `face' is bound locally.
  (when (boundp 'face)
    (dolist (face (face-list))

      ;; Make italic gray instead of black
      ;; (when (face-italic-p face frame)
      ;;   (if (equal (face-foreground face frame) "black")
      ;;       (set-face-foreground face "gray50" frame)))

      ;; My font makes bold text illegible,
      ;; so replace bold property with underline property
      ;; (when (face-bold-p face frame)
      ;;   (set-face-bold face nil frame)
      ;;   ;; (set-face-inverse-video face t frame)
      ;;   (set-face-underline face t frame))

      ;; Fonts with different height decrease the amount of lines
      ;; visible on screen, so remove the height property
      (when (numberp (face-attribute face :height frame))
        (set-face-attribute face frame :height 'unspecified))

      ;; Fonts with different width decrease the amount of characters
      ;; on the line, so remove the width property
      (when (numberp (face-attribute face :width frame))
        (set-face-attribute face frame :width 'unspecified))

      ;; Fonts with different weight decrease the height and width,
      ;; of the line, so remove the weight property
      ;; (when (numberp (face-attribute face :weight frame))
      ;;   (set-face-attribute face frame :weight 'unspecified))
      ;; (unless (string-match "^mode-line" (symbol-name face))
      ;;   ;; Reset all face attributes
      ;;   (modify-face face))
      )))

;; 1. Fix existing faces
;; (let ((face t)) (my-faces-fix))
;; (add-hook 'after-init-hook (lambda () (let (face) (my-faces-fix))) t)
;; 2. Call `my-faces-fix' every time some new face gets defined
(add-to-list 'custom-define-hook 'my-faces-fix)


;;; keybindings

(define-key global-map [(control left)]       'backward-sexp)
(define-key global-map [(control right)]      'forward-sexp)
(define-key global-map [(control kp-left)]    'backward-sexp)
(define-key global-map [(control kp-right)]   'forward-sexp)
(define-key global-map [(control meta left)]  'backward-word)
(define-key global-map [(control meta right)] 'forward-word)

(define-key global-map [(control meta up)]    'backward-paragraph)
(define-key global-map [(control meta down)]  'forward-paragraph)

;; fix new controversial keybindings in Emacs 23
(define-key global-map [home] 'beginning-of-visual-line)
(define-key global-map [end]  'end-of-visual-line)
(define-key global-map [up]   'previous-line)
(define-key global-map [down] 'next-line)
;; This is BAD because C-n/C-p are useful in the multi-line minibuffer:
;; (define-key global-map "\C-p" 'previous-logical-line) ; previous-real-line
;; (define-key global-map "\C-n" 'next-logical-line)     ; next-real-line

;; TODO: these are new overridden by `windmove',
;; find new keyprefix for navigational keys (in Info, view, etc):
(define-key global-map [(meta left)]
  (lambda ()
    (interactive)
    ;; TODO: also check in (wincows)
    (if (> (length (get-buffer-window-list (current-buffer) t t)) 1)
        (dired-jump)
      ;; Go to the top to not store emacs-places.
      (goto-char (point-min))
      (kill-current-buffer-and-dired-jump))))
(define-key global-map [(meta right)]         'my-find-thing-at-point)
;; Keybindings (meta up) (meta down) are free when windmove uses `super'.

;; (windmove-default-keybindings 'meta)
(windmove-default-keybindings 'super)
(windmove-default-keybindings 'hyper)
;; (windmove-default-keybindings '(meta shift))

(winner-mode)

;; TODO: bind (shift meta) to a new package that moves windows like
;; `rotate-window-buffers' bound to `C-z C-u'
(define-key global-map [(shift super left)]  'rotate-window-buffers)
(define-key global-map [(shift super right)] 'rotate-window-buffers)
(define-key global-map [(shift hyper left)]  'rotate-window-buffers)
(define-key global-map [(shift hyper right)] 'rotate-window-buffers)

;; Actually I don't use next two keybindings, use them for something useful
;; (define-key global-map [(control meta prior)] 'scroll-right)
;; (define-key global-map [(control meta next)]  'scroll-left)

(define-key global-map [(control down)] 'scroll-up-line)
(define-key global-map [(control up)] 'scroll-down-line)
(define-key global-map [(control kp-down)] 'scroll-up-line)
(define-key global-map [(control kp-up)] 'scroll-down-line)

;; (define-key global-map [(control return)]
;;   (lambda () (interactive) (let ((use-hard-newlines t)) (newline))))
;; (define-key global-map [(meta return)]
;;   (lambda () (interactive) (scroll-other-window 1))) ;; [(meta down)]
;; (define-key global-map [(meta backspace)]
;;   (lambda () (interactive) (scroll-other-window -1))) ;; [(meta up)]

(define-key global-map [(control backspace)] 'backward-kill-word)
;; (define-key global-map [(meta backspace)] 'undo)
;; (define-key global-map [(meta backspace)] 'backward-kill-word)
;; (define-key global-map [(control backspace)] 'join-lines)

(define-key global-map [(control ?=)] 'compare-windows)
;; alternative: (lambda () (interactive) (compare-windows t))

;; I often mistype `compare-windows' as `comapre-windows', allow both:
(defalias 'comapre-windows 'compare-windows)

(define-key global-map [(control kp-home)] 'beginning-of-buffer)
(define-key global-map [(control kp-end)]  'end-of-buffer)
(define-key global-map [(control shift kp-5)] 'goto-line)
(define-key global-map [(control kp-begin)] 'goto-line)

;; Use new dwim case commands
(define-key esc-map "u" 'upcase-dwim)
(define-key esc-map "l" 'downcase-dwim)
(define-key esc-map "c" 'capitalize-dwim)

;; These following two keybindings are standard default:
;; (define-key global-map [(meta /)] 'dabbrev-expand)
;; (define-key global-map [(control meta /)] 'dabbrev-completion)
(define-key global-map [(meta kp-divide)] 'hippie-expand)
;; The following key is not available:
;; (define-key global-map [(control meta kp-divide)] 'hippie-expand)

;; BAD key: (define-key global-map "\M-n" 'clone-buffer)
(define-key global-map [(control x) (c) (b)] 'clone-buffer)

(define-key ctl-x-map "\C-\M-u" 'raise-sexp)  ;; like `C-M-u'
(define-key ctl-x-map "\M-("    'delete-pair) ;; the reverse of `M-('

(define-key global-map [f1] 'info)
(define-key global-map [(control f1)] 'info-lookup-symbol)
(define-key global-map [f2] 'save-buffer)
;; (define-key global-map [f9] 'call-last-kbd-macro)
(define-key global-map [(control f9)] 'compile)
(define-key global-map [(meta f7)] 'grep) ; Commander-like
(define-key global-map [(meta shift f7)] 'grep-find)

(define-key goto-map "re" 'grep)
(define-key goto-map "rr" 'rgrep)
(define-key goto-map "rl" 'lgrep)
(define-key goto-map "\M-r\M-e" 'grep)
(define-key goto-map "\M-r\M-r" 'rgrep)
(define-key goto-map "\M-r\M-l" 'lgrep)

;; (define-key global-map [(control escape)]
;;   (lambda () (interactive) (buffer-menu 1))) ; not needed
;; (define-key global-map [(control escape)] 'ibuffer)
;; (define-key global-map [(shift f10)] 'buffer-menu) ; not needed

;; Like standard Emacs 22 commands (bound to C-x left/right)
(define-key global-map [f11] 'previous-buffer) ;; my-buffer-prev
(define-key global-map [f12] 'next-buffer)     ;; my-buffer-next

;; Like standard Emacs 22 commands (bound to M-g n/p)
(define-key global-map [(control f11)] 'previous-error)
(define-key global-map [(control f12)] 'next-error)
(define-key global-map [(control shift f11)] 'compilation-previous-file)
(define-key global-map [(control shift f12)] 'compilation-next-file)

(defvar my-next-error-prev-buffer nil)
(defun my-next-error ()
  ;; Get rid of file buffers visited during going through results.
  (when (and my-next-error-prev-buffer
             (not (eq my-next-error-prev-buffer (current-buffer)))
             ;; buffer not edited
             (memq buffer-undo-list '(nil t))
             ;; only on consequent keystrokes
             (memq this-command '(next-error previous-error))
             (eq (with-current-buffer next-error-last-buffer major-mode)
                 'grep-mode))
    ;; TODO: preserve existing file buffers, and positions in all file buffers
    ;; (goto-char (point-min)) ...
    (kill-buffer my-next-error-prev-buffer))
  (setq my-next-error-prev-buffer (current-buffer)))
(add-hook 'next-error-hook 'my-next-error)

;; See bug#20489: 25.0.50; next-error-find-buffer chooses non-current buffer without good reason
;; See bug#28864: 25.3.50; next-error-no-select does select
;; (setq next-error-find-buffer-function
;;       (lambda (&optional avoid-current extra-test-inclusive extra-test-exclusive)
;; 	(window-parameter nil 'next-error-buffer)))
;; (add-hook 'next-error-hook
;; 	  (lambda ()
;; 	    (set-window-parameter
;; 	     nil 'next-error-buffer next-error-last-buffer)))

;; TODO: currently key (control escape) is free, bind it to something useful,
;; unless it is used by the window manager

;; The following two corrections are for Scandinavian keyboard layouts.
;; Bind AltGr-space to the same command as is bound to Alt-space (M-SPC)
;; instead of inserting space-looking nobreak-space (nbsp, 0xa0, 0x8a0).
;; This is not necessary in Emacs 22 where nbsp has a special face.
;; (define-key global-map [?\xa0]  'just-one-space)
;; (define-key global-map [?\x8a0] 'just-one-space)

;; Swap currency sign with dollar sign, so dollar sign which is used more
;; often in programming languages could be typed more easily by pressing
;; shift-4 instead of AltGr-4.
;; (keyboard-translate ?\244 ?\$)
;; (keyboard-translate ?\$ ?\244)
;; Better to change this in .xmodmaprc for other applications too as:
;; keycode 13 = 4 dollar 4 dollar dollar cent

;; Map some diacritic characters (Ao, A", O") to arrow keys
;; which have the same layout as arrow pad keys on AltGr keyboards
;; This is experimental to make C-f/C-b/C-n/C-p like as easy as hjkl.
;; (define-key global-map [?\x8e5] 'previous-line) ; [up]
;; (define-key global-map [?\x8e4] 'next-line)
;; (define-key global-map [?\x8f6] 'backward-char)
;; (define-key global-map [?'] 'forward-char)


;;; quail

;; The default key `C-\' is difficult to type on AltGr keyboards.
;; (global-set-key [(control ?+)] 'toggle-input-method)
;; (global-set-key [(control ?')] 'toggle-input-method)
;; (global-set-key [(meta return)] 'toggle-input-method)
;; (define-key isearch-mode-map [(meta return)] 'isearch-toggle-input-method)
;; added for capslock to ~/.xsession: echo "keycode 66 = Print" | xmodmap -
;; (global-set-key [print] 'toggle-input-method)
;; (define-key isearch-mode-map [print] 'isearch-toggle-input-method)
;; (define-key mule-keymap "\\" 'set-input-method)

;; TODO for Emacs23: if toggle-input-method is called on the active region
;; then convert region to other coding, this is very useful when the region
;; was typed with a wrong input method, when the user forgot to toggle it


;;; mule

;; Delete codings like `utf-*-with-signature' (they hide BOMs)
;; to allow to always display the BOM (Byte-order mark signature)
;; to be able to remove it without the need to visit files literally
;; or with `C-x RET c utf-8 RET C-x C-f'.
;; SEE ALSO http://thread.gmane.org/gmane.emacs.devel/116668/focus=116738

(setq auto-coding-regexp-alist
      (delete (rassoc 'utf-16be-with-signature auto-coding-regexp-alist)
      (delete (rassoc 'utf-16le-with-signature auto-coding-regexp-alist)
      (delete (rassoc 'utf-8-with-signature auto-coding-regexp-alist)
              auto-coding-regexp-alist))))


;;; C-z my-map

;; Make the prefix key `C-z' for my personal keymap.
;; On qwerty-keyboards `C-z' is one of the most accessible keys
;; like `C-x' and `C-c', but the prefix key `C-c' is reserved
;; for mode-specific commands (both user-defined and standard Emacs extensions).
;; The standard binding of `C-z' (`suspend-emacs' or `iconify-or-deiconify-frame')
;; is reassigned here to double key sequence `C-z C-z'.
(defvar my-map
  (let ((map (make-sparse-keymap))
        (c-z (global-key-binding "\C-z")))
    (global-unset-key "\C-z")
    (define-key global-map "\C-z" map)
    (define-key map "\C-z" c-z)
    map))

;; my map can be used from isearch
;; (define-key isearch-mode-map "\C-z" my-map)
;; (define-key isearch-mode-map "\C-z" 'isearch-other-control-char)

(when window-system
  ;; Use single escape keypress instead of knocking it 3 times.
  ;; On a window system there is no need to use ESC as a prefix key.
  (define-key global-map [escape] 'keyboard-escape-quit)
  (define-key isearch-mode-map  [escape] 'isearch-cancel)

  ;; Set ESC-modifier to C-z escape
  ;; This is useful to invoke `M-TAB' or `M-|' on keyboards with AltGr key,
  ;; as `C-z ESC TAB' or `C-z ESC |'
  (define-key my-map [escape] esc-map)

  (define-key my-map [(control ?u)] 'rotate-window-buffers)
  (define-key my-map "t" 'toggle-truncate-lines)
  (define-key my-map "v" nil)
  (define-key my-map "vs" 'set-variable)
  (define-key my-map "vc" 'customize-variable)
  (define-key my-map "vtw2" (lambda () (interactive) (setq-local tab-width 2) (force-mode-line-update)))
  (define-key my-map "r" 'revert-buffer)
  (define-key my-map "\C-q" 'quoted-insert) ; because global C-q is rebound above
  ;; `C-z -' and `C-z C--' inserts a vertical line.
  (define-key my-map [(control ?-)] (lambda () (interactive) (insert "\f\n"))) ; because global C-q C-l is rebound above
  (define-key my-map "-" (lambda () (interactive) (insert "\f\n"))) ; because global C-q C-l is rebound above
  (define-key my-map "p" (lambda () (interactive) (my-shell-command "perl test.pl")))
  ;; TEST: try `C-z C-x C-x C-x C-x ...', try `C-x z C-z C-z C-z' (repeat.el)
  )

;; Modify esc-map when not on a tty
(when window-system
  ;; Insert paired characters
  (define-key esc-map "\""
    (lambda ()
      (interactive)
      (let ((insert-pair-alist
             (cons
              (if (and (memq buffer-file-coding-system '(utf-8-unix utf-8-emacs-unix))
                       (or (and comment-start (nth 4 (syntax-ppss)))
                           (and (derived-mode-p 'text-mode)
                                (not (derived-mode-p 'sgml-mode))
                                (not (derived-mode-p 'vc-git-log-edit-mode))
                                )
                           ;; (derived-mode-p 'fundamental-mode)
                           ))
                  '(?\" ?\“ ?\”)
                '(?\" ?\" ?\"))
              insert-pair-alist)))
        (call-interactively 'insert-pair))))
  ;; (define-key esc-map "`"  'insert-pair)
  ;; (define-key global-map "\M-`" 'insert-pair)
  (define-key esc-map "'"
    (lambda ()
      (interactive)
      (let ((insert-pair-alist
             (cons
              (if (and (memq buffer-file-coding-system '(utf-8-unix utf-8-emacs-unix))
                       (or (and comment-start (nth 4 (syntax-ppss)))
                           (and (derived-mode-p 'text-mode)
                                (not (derived-mode-p 'sgml-mode))
                                (not (derived-mode-p 'vc-git-log-edit-mode))
                                )
                           ;; (derived-mode-p 'fundamental-mode)
                           ))
                  '(?\' ?\‘ ?\’)
                '(?\' ?\' ?\'))
              insert-pair-alist)))
        (call-interactively 'insert-pair))))
  ;; Optionally, make ' insert backquote `'.
  ;; (add-to-list 'insert-pair-alist '(?\' ?\` ?\'))
  (define-key esc-map "["  'insert-pair)
  (define-key esc-map "{"  'insert-pair)
  (define-key esc-map ")"  'up-list))

(define-key my-map  "`"  'insert-pair)
(define-key my-map  "<"  'insert-pair)

;; (defun insert-pair-without-space ()
;;   (interactive)
;;   (let ((parens-require-spaces nil))
;;     (call-interactively 'insert-pair)))
;; (defun insert-pair-with-space ()
;;   (interactive)
;;   (let ((parens-require-spaces t))
;;     (call-interactively 'insert-pair)))
;; (define-key esc-map "[" 'insert-pair-without-space)
;; (define-key esc-map "(" 'insert-pair-with-space)


;;; cursor

;; USE (setq-default cursor-type ...) INSTEAD OF THE NEXT FUNCTION
;; (defun set-cursor-type (cursor-type)
;;   "Set the text cursor type of the selected frame to CURSOR-TYPE.
;; When called interactively, prompt for the name of the type to use.
;; To get the frame's current cursor type, use `frame-parameters'."
;;   ;; see `fringe-query-style'
;;   (interactive (list (intern (completing-read
;;                            "Cursor type: "
;;                            '("box" "hollow" "bar" "hbar" nil)))))
;;   (modify-frame-parameters (selected-frame)
;;                         (list (cons 'cursor-type cursor-type))))

;; Currently cursor color is frame-local, but should be buffer-local like
;; cursor-type (or maybe even window-local).
;; Also background color should be buffer-local
;; (maybe this is already fixed in the tiled-background branch?).

;; (defadvice toggle-input-method (after my-toggle-input-method activate)
;;   (if current-input-method
;;       (set-cursor-color "red") ; "AntiqueWhite4"
;;     (set-cursor-color "black")))

;; Use box cursor for overwrite-mode, and red cursor for quail active input.
(defun my-change-cursor ()
  "Change cursor color and type depending on insertion mode and input method."
  (set-cursor-color
   (cond (current-input-method "red3") ; "AntiqueWhite4"
         ((eq (frame-parameter (selected-frame) 'background-mode) 'dark)
                               "DarkGrey")
         (t                    "black")))
  (setq-default cursor-type
   (cond (overwrite-mode       'box)
         (t                    'bar))))
(add-hook 'post-command-hook 'my-change-cursor)


;;; functions

(defun my-find-thing-at-point ()
  "Find variable, function or file at point."
  (interactive)
  (cond ((not (eq (variable-at-point) 0))
         (call-interactively 'describe-variable))
        ((function-called-at-point)
         (call-interactively 'describe-function))
        (t (find-file-at-point))))

(defun my-next-link-or-scroll-page-forward (next-point)
  "Scroll one screen forward when no more next links are visible on the screen.
The argument `next-point' is the point's position of the next link."
  (if (and (> (window-end) next-point) (> next-point (point)))
      (goto-char next-point)
    (if (>= (window-end) (point-max))
        (goto-char (point-max))
      (progn (View-scroll-page-forward-set-page-size) (move-to-window-line 0)))))

(defun my-prev-link-or-scroll-page-backward (prev-point)
  "Scroll one screen backward when no more previous links are visible on the screen.
The argument `prev-point' is the point's position of the previous link."
  (if (and (< (window-start) prev-point) (< prev-point (point)))
      (goto-char prev-point)
    (if (<= (window-start) (point-min))
        (goto-char (point-min))
      (progn (View-scroll-page-backward-set-page-size)))))

(defvar my-scroll-auto-timer nil)
(defun my-scroll-auto (arg)
  "Scroll text of current window automatically with a given frequency.
With a numeric prefix ARG, use its value as frequency in seconds.
With C-u, C-0 or M-0, cancel the timer."
  (interactive
   (list (progn
           (if (and (boundp 'my-scroll-auto-timer)
                    (timerp  my-scroll-auto-timer))
               (cancel-timer my-scroll-auto-timer))
           (or current-prefix-arg
               (read-from-minibuffer
                "Enter scroll frequency measured in seconds (0 or RET for cancel): "
                nil nil t nil "0")))))
  (if (not (or (eq arg 0) (equal arg '(4))))
      (setq my-scroll-auto-timer (run-at-time t arg 'scroll-up 1))))

(define-key my-map "s" 'my-scroll-auto)


;;; window

(defun my-move-to-window-top (&optional arg)
  "Position point to the top line of the window."
  (interactive)
  (move-to-window-line 0))

(define-key global-map [(control prior)] 'my-move-to-window-top)
(define-key global-map [(control kp-prior)] 'my-move-to-window-top)

(defun my-move-to-window-bottom (&optional arg)
  "Position point to the bottom line of the window."
  (interactive)
  (move-to-window-line -1))

(define-key global-map [(control next)]  'my-move-to-window-bottom)
(define-key global-map [(control kp-next)]  'my-move-to-window-bottom)

(defun my-windows-balance ()
  (interactive)
  (other-window 1)
  (balance-windows)
  (shrink-window-if-larger-than-buffer)
  (other-window -1))

(define-key my-map "wb" 'my-windows-balance)

;; [2009-11-29] TRY TO USE `recenter-top-bottom' instead of this:
;; (defvar my-recenter-line 15)
;; (defun my-recenter (&optional arg)
;;   "Places point in window on eyes level."
;;   (interactive "P")
;;   (if (equal arg '(16))
;;       (setq my-recenter-line (count-screen-lines
;;                               (window-start)
;;                               (save-excursion (beginning-of-line) (point)))
;;             arg nil
;;             recenter-position (/ (float my-recenter-line)
;;                                  (count-screen-lines
;;                                   (window-start) (window-end)))))
;;   (if arg (recenter arg) (recenter my-recenter-line)))
;; (put 'my-recenter 'isearch-scroll t)
;;
;; (define-key my-map "\C-l" 'my-recenter)
;; (define-key global-map "\C-l" 'my-recenter)

;; [2009-11-29] TRY TO USE `move-to-window-line-top-bottom' instead of this:
;; (defun my-move-to-window-line (&optional arg)
;;   "Places point in window on eyes level."
;;   (interactive "P")
;;   (if (equal arg '(16))
;;       (setq my-recenter-line (count-screen-lines
;;                               (window-start)
;;                               (save-excursion (beginning-of-line) (point)))
;;             arg nil))
;;   (if arg (move-to-window-line arg) (move-to-window-line my-recenter-line)))
;;
;; (define-key my-map "\M-r" 'my-move-to-window-line)
;; (define-key global-map "\M-r" 'my-move-to-window-line)

(when (boundp 'recenter-positions)
  (setq recenter-position (car recenter-positions)))

;; OLD: (setq split-window-preferred-function 'split-window-preferred-horizontally)
;; (defadvice split-window-preferred-horizontally
;;            (around my-split-window-preferred-horizontally act)
;;   (let ((window ad-do-it))
;;     (if (string-match "\\*Help\\*\\(\\|<[0-9]+>\\)" (buffer-name (car (buffer-list))))
;;         (selected-window)
;;       window)))

(defun split-window-horizontally-3 ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (other-window -1))

(defun follow-mode-3 ()
  (interactive)
  (split-window-horizontally-3)
  (follow-mode 1)
  (goto-char (point-min)))

(define-key my-map "3" 'split-window-horizontally-3)
(define-key my-map "f3" 'follow-mode-3)

(defun split-window-horizontally-4 ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (other-window 2)
  (split-window-horizontally)
  (other-window 2))

(defun follow-mode-4 ()
  (interactive)
  (split-window-horizontally-4)
  (follow-mode 1)
  (goto-char (point-min)))

(define-key my-map "4" 'split-window-horizontally-4)
(define-key my-map "f4" 'follow-mode-4)


;;; follow

(eval-after-load "follow"
  '(progn
     (define-key follow-mode-map [prior] 'follow-scroll-down)
     (define-key follow-mode-map [next]  'follow-scroll-up)))


;;; isearch

;; Save and restore window start positions on returning to previous search
(setq isearch-push-state-function
      (lambda ()
	;; Recenter new search hits outside of window boundaries
        (when (and isearch-success (not (pos-visible-in-window-p)))
          (recenter (round (* 0.15 (window-height)))))
        `(lambda (cmd)
           (when isearch-success
             (set-window-start nil ,(window-start))))))

;; Wrap without failing, posted to
;; http://stackoverflow.com/questions/285660/automatically-wrapping-i-search#287067
;; (defadvice isearch-repeat (after isearch-no-fail activate)
;;   (unless isearch-success
;;     (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-repeat)
;;     (isearch-repeat (if isearch-forward 'forward))
;;     (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-repeat)))

;; Automatically recenter every found isearch match
;; (defadvice isearch-update (before my-isearch-update activate)
;;   (sit-for 0)
;;   (if (and
;;        ;; not the scrolling command
;;        (not (eq this-command 'isearch-other-control-char))
;;        ;; not the empty string
;;        (> (length isearch-string) 0)
;;        ;; not the first key (to lazy highlight all matches w/o recenter)
;;        (> (length isearch-cmds) 2)
;;        ;; the point in within the given window boundaries
;;        (let ((line (count-screen-lines (point) (window-start))))
;;          (or (> line (* (/ (window-height) 4) 3))
;;              (< line (* (/ (window-height) 9) 1)))))
;;       (let ((recenter-position 0.3))
;;         (recenter '(4)))))

;; Automatically reposition every found isearch match
;; (defadvice isearch-update (before my-isearch-reposite activate)
;;   (sit-for 0)
;;   (reposition-window))

;; TODO: try to use `isearch-update-post-hook', e.g.
;; (add-hook 'isearch-update-post-hook 'recenter)
;; (add-hook 'replace-update-post-hook 'recenter)
(defadvice isearch-update (before my-isearch-reposite activate)
  (sit-for 0)
  ;; While browsing patches, make the next hunk posited at the window's top:
  (when (and (derived-mode-p 'diff-mode) isearch-regexp (equal "^revno:" isearch-string))
    (recenter 1)))

(put 'toggle-truncate-lines 'isearch-scroll t)

;; TODO: propose these 2 commands but without keybindings (or maybe `M-s M-<').
;; SEE ALSO http://www.reddit.com/r/emacs/comments/1h8zfu/incremental_search_from_beginning_of_the_buffer/
(defun isearch-beginning-of-buffer ()
  "Go to the first occurrence of the current match.
Move isearch point to the beginning of the buffer and repeat the search."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(define-key isearch-mode-map "\M-<" 'isearch-beginning-of-buffer)

(defun isearch-end-of-buffer ()
  "Go to the last occurrence of the current match.
Move isearch point to the end of the buffer and repeat the search."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

(define-key isearch-mode-map "\M->" 'isearch-end-of-buffer)

(define-key isearch-mode-map             "\t" 'isearch-complete)
(define-key minibuffer-local-isearch-map "\t" 'isearch-complete-edit)

(define-key isearch-mode-map [(control return)] 'isearch-exit)

;; C-RET doesn't add the current search string to the search ring
;; and moves point to the beginning of the found search string.
(add-hook 'isearch-mode-end-hook
          (lambda ()
            ;; Exiting isearch with C-RET
            (when (eq last-input-event 'C-return)
              ;; Move point to the beginning of the found search string
              (if (and isearch-forward isearch-other-end)
                  (goto-char isearch-other-end))
              ;; Don't add the current search string to the search ring
              (if isearch-regexp
                  (setq regexp-search-ring (cdr regexp-search-ring))
                (setq search-ring (cdr search-ring))))))

;; S-RET leaves lazy-highlighted matches.
(defun my-isearch-exit-leave-lazy-highlight ()
  "Exit search and leave extra match highlighting."
  (interactive)
  (let ((lazy-highlight-cleanup nil))
    (when isearch-lazy-highlight
      (isearch-lazy-highlight-new-loop (point-min) (point-max)))
    (isearch-exit)))

(define-key isearch-mode-map [(shift return)]
                             'my-isearch-exit-leave-lazy-highlight)

;; Make Isearch mode-line string shorter, just " /" instead of " Isearch"
;; (add-hook 'isearch-mode-hook
;;           (lambda () (setq isearch-mode " /") (force-mode-line-update)))

;; Do not use customization to not corrupt .emacs with literal
;; control characters.
;; The next line is bad, because \n is bad for `C-M-s SPC $'
;; (setq search-whitespace-regexp "[ \t\r\n]+")
;; TRY to ignore punctuation, BAD because C-w (`isearch-yank-word-or-char')
;; doesn't yank punctuation characters, so use word search instead of this:
;; (setq search-whitespace-regexp "\\W+")
;; TRY to match newlines like in `compare-windows-whitespace':
(setq search-whitespace-regexp "\\(\\s-\\|\n\\)+")
;; Actually this line doesn't affect `search-whitespace-regexp' defined below.
(setq Info-search-whitespace-regexp "\\(\\s-\\|\n\\)+")

;; TRY:
;; Like `word-search-regexp'
(defun search-whitespace-regexp (string &optional _lax)
  "Return a regexp which ignores whitespace.
Uses the value of the variable `search-whitespace-regexp'."
  (if (or (not (stringp search-whitespace-regexp))
          (null (if isearch-regexp
                    isearch-regexp-lax-whitespace
                  isearch-lax-whitespace)))
      string
    ;; FIXME: this is not strictly correct implementation because it ignores
    ;; `subregexp-context-p' and replaces spaces inside char set group like
    ;; in `C-M-s M-s SPC [ ]', it converts it to ["\\(\\s-\\|\n\\)+"] !
    (replace-regexp-in-string
     search-whitespace-regexp
     search-whitespace-regexp ;; or replace by " " that is handled by search-spaces-regexp
     (regexp-quote string) nil t)))
;; (defun search-forward-lax-whitespace (string &optional bound noerror count)
;;   (re-search-forward (search-whitespace-regexp (regexp-quote string)) bound noerror count))
;; (defun search-backward-lax-whitespace (string &optional bound noerror count)
;;   (re-search-backward (search-whitespace-regexp (regexp-quote string)) bound noerror count))
;; (defun re-search-forward-lax-whitespace (regexp &optional bound noerror count)
;;   (re-search-forward (search-whitespace-regexp regexp) bound noerror count))
;; (defun re-search-backward-lax-whitespace (regexp &optional bound noerror count)
;;   (re-search-backward (search-whitespace-regexp regexp) bound noerror count))
(setq search-default-mode #'char-fold-to-regexp)

;; OBSOLETE:
;; Decomposition search for accented letters.
(define-key isearch-mode-map "\M-sd" 'isearch-toggle-decomposition)
(defun isearch-toggle-decomposition ()
  "Toggle Unicode decomposition searching on or off."
  (interactive)
  (setq isearch-word (unless (eq isearch-word 'isearch-decomposition-regexp)
		       'isearch-decomposition-regexp))
  (if isearch-word (setq isearch-regexp nil))
  (setq isearch-success t isearch-adjusted t)
  (isearch-update))
(defun isearch-decomposition-regexp (string &optional _lax)
  "Return a regexp that matches decomposed Unicode characters in STRING."
  (let ((accents "['`\"́̋]"))
    (mapconcat
     (lambda (c0)
       (concat (string c0) accents "?"))
     (replace-regexp-in-string accents "" string) "")))
(put 'isearch-decomposition-regexp 'isearch-message-prefix "deco ")


;;; occur

;; Make *Occur* buffer names unique and writable
;; (like in `compilation-mode-hook' below).
(add-hook 'occur-hook
          (lambda ()
            (occur-rename-buffer t)
            (setq buffer-read-only nil)))

;; Based on `occur-mode-goto-occurrence-other-window'
(defun occur-mode-goto-occurrence-kill-buffer ()
  "Go to the occurrence the current line describes, and kill the Occur buffer."
  (interactive)
  (let ((buf (current-buffer))
        (pos (occur-mode-find-occurrence)))
    (switch-to-buffer-other-window (marker-buffer pos))
    (goto-char pos)
    (kill-buffer buf)
    (run-hooks 'occur-mode-find-occurrence-hook)))

;; Bind to "o" in place of `occur-mode-goto-occurrence'.
(define-key occur-mode-map [(control return)] 'occur-mode-goto-occurrence-kill-buffer)


;;; replace

(defun substitute-regexp (substitution)
  "Use s/old/new/g regexp syntax for `query-replace'."
  (interactive
   (list
    (read-from-minibuffer "Substitute regexp: " '("s///g" . 3) nil nil
                          'query-replace-history nil t)))
  (if (string-match "^s/\\(.*\\)/\\(.*\\)/\\([gi]*\\)" substitution)
      (let* ((sregex (match-string 1 substitution))
             (ssubst (match-string 2 substitution))
             (sflags (match-string 3 substitution))
             (case-fold-search (string-match "i" sflags)))
        (perform-replace
         sregex ssubst (string-match "g" sflags)
         t nil nil nil
         (if (and transient-mark-mode mark-active) (region-beginning))
         (if (and transient-mark-mode mark-active) (region-end))))
    (error "Invalid syntax")))

;; FROM http://emacs.stackexchange.com/questions/27135/search-replace-like-feature-for-swapping-text/27170#27170
(defun query-swap-strings (from-string to-string &optional delimited start end backward region-noncontiguous-p)
  "Swap occurrences of FROM-STRING and TO-STRING."
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query swap"
		   (if current-prefix-arg
		       (if (eq current-prefix-arg '-) " backward" " word")
		     "")
		   (if (use-region-p) " in region" ""))
	   nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   (if (use-region-p) (region-beginning))
	   (if (use-region-p) (region-end))
	   (nth 3 common)
	   (if (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   (concat "\\(" (regexp-quote from-string) "\\)\\|" (regexp-quote to-string))
   `(replace-eval-replacement replace-quote (if (match-string 1) ,to-string ,from-string))
   t t delimited nil nil start end backward region-noncontiguous-p))


;;; minibuffer

;; See http://lists.gnu.org/archive/html/emacs-devel/2014-12/msg00299.html
(define-key minibuffer-local-map [S-return] 'newline)

;; Remove potentially dangerous commands from the history immediately
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (when (string-match
                   "^rm"
                   (or (car-safe (symbol-value minibuffer-history-variable)) ""))
              (set minibuffer-history-variable
                   (cdr (symbol-value minibuffer-history-variable))))))

;; This is not needed when isearch C-s/C-r in the minibuffer is available
;; (but `C-M-r ^command' doesn't match at the beginning of the input area)
(define-key minibuffer-local-map "\eN" 'next-complete-history-element)
(define-key minibuffer-local-map "\eP" 'previous-complete-history-element)

;; M-k in the minibuffer deletes the minibuffer history element.
(defun delete-history-element ()
  "Delete the current minibuffer history element from the history.
After deleting the element, the history position is changed either
to the the previous history element, or to the next history element
if the deleted element was the last in the history list."
  (interactive)
  (cond
   ((= minibuffer-history-position 1)
    (set minibuffer-history-variable
         (cdr (symbol-value minibuffer-history-variable))))
   ((> minibuffer-history-position 1)
    (setcdr (nthcdr (- minibuffer-history-position 2)
                    (symbol-value minibuffer-history-variable))
            (nthcdr minibuffer-history-position
                    (symbol-value minibuffer-history-variable)))))
  (condition-case nil (next-history-element     1) (error nil))
  (condition-case nil (previous-history-element 1) (error nil)))

(define-key minibuffer-local-map "\ek" 'delete-history-element)
(define-key minibuffer-local-isearch-map "\ek" 'delete-history-element)

;; THE NEXT 3 FUNCTIONS WORK WITH BIG DELAY (try to use like icomplete.el)
;; see also PC-temp-minibuffer-message, file-cache-temp-minibuffer-message,
;; calc-temp-minibuffer-message and bug report in emacs-pretest-bug
;; Subject: bad doc string for PC-temp-minibuffer-message
(defun minibuffer-history-position-message ()
  (if (memq this-command '(next-history-element previous-history-element))
      (minibuffer-message
       (propertize
        (format "%s[%s]"
                (make-string
                 1
                 ;;              (- (frame-width)
                 ;;                 (minibuffer-prompt-width)
                 ;;                 (length (minibuffer-contents-no-properties))
                 ;;                 5)
                 ?\ )
                minibuffer-history-position) 'face 'shadow))))
;; (defadvice next-history-element (after history-position-message activate)
;;   (minibuffer-history-position-message))
;; (defadvice previous-history-element (after history-position-message activate)
;;   (minibuffer-history-position-message))
;; (defadvice goto-history-element (after history-position-message activate)
;;   (minibuffer-history-position-message))

;; (defadvice goto-history-element (before minibuffer-set-default activate)
;;   (if (functionp minibuffer-default-function)
;;       (funcall minibuffer-default-function)))

;; Another implementation of the same idea:
(defvar minibuffer-history-position-overlay)
(make-variable-buffer-local 'minibuffer-history-position-overlay)
(defun minibuffer-history-position-setup ()
  "Set up a minibuffer for `minibuffer-history-position-mode'.
The prompt should already have been inserted."
  (setq minibuffer-history-position-overlay (make-overlay (point-min) (1+ (point-min))))
  (overlay-put minibuffer-history-position-overlay 'evaporate t))
;; (add-hook 'minibuffer-setup-hook 'minibuffer-history-position-setup)
(defun minibuffer-history-position-update ()
  "Update a minibuffer for `minibuffer-history-position-mode'."
  (overlay-put minibuffer-history-position-overlay 'before-string
               (propertize (format "(%d) " minibuffer-history-position)
                           'face 'minibuffer-prompt)))
;; (defadvice next-history-element (after my-next-history-element activate)
;;   (minibuffer-history-position-update))
;; (defadvice previous-history-element (after my-previous-history-element activate)
;;   (minibuffer-history-position-update))


;;; other features

;; http://thread.gmane.org/gmane.emacs.devel/116457/focus=116468 is like this:
(defun my-info-refresh (&optional arg)
  "Display some useful information in the echo area instead of the mode line.
With prefix arg, insert the current timestamp to the current buffer."
  (interactive "P")
  (cond
   ((equal arg '(4))  ; C-u f5
    (insert (format-time-string "%Y%m%d" (current-time))))
   ((equal arg '(16)) ; C-u C-u f5
    (insert (format-time-string "%Y-%m-%d" (current-time))))
   (t (message "%s"
               (concat
                (format-time-string "%Y-%m-%d %H:%M:%S %z" (current-time)) ;; ISO
                " "
                (if (boundp 'calendar-day-abbrev-array)
                    (aref calendar-day-abbrev-array (nth 6 (decode-time (current-time))))
                  (format-time-string "%a" (current-time)))
                " : "
                (or (buffer-file-name) default-directory))))))

(define-key my-map     [f5]  'my-info-refresh)
(define-key global-map [f5]  'my-info-refresh)

(defun my-work-log-add (&optional arg)
  (interactive "P")
  (find-file my-work-log-file)
  (goto-char (point-max))
  (cond ((re-search-backward "^[0-9-]+ [0-9:]+\\( +\\)[^0-9]" nil t)
         (goto-char (match-beginning 1))
         (replace-match "" t t nil 1)
         (insert (format-time-string " %H:%M " (current-time))))
        ((re-search-backward "^[0-9-]+ [0-9:]+" nil t)
         (forward-line 1)
         (insert (format-time-string "%Y-%m-%d %H:%M       \n" (current-time)))
         (backward-char))))

(define-key my-map "wl" 'my-work-log-add)

(defun my-buffer-xray ()
  "Display text properties and overlays of current buffer by adding markups."
  (interactive)
  (let* ((newbuf (get-buffer-create (format "*xray-buffer*/%s" (buffer-name))))
         (s (buffer-substring (point-min) (point-max))) ;; (buffer-string) -no-properties
         (overlays (sort (overlays-in (point-min) (point-max))
                         (lambda (a b) (< (overlay-start a)
                                          (overlay-start b)))))
         (oi 0)
         ;; ois is indexes of overlays sorted by start positions
         (ois (mapcar (lambda (o) (setq oi (1+ oi)) (cons o oi))
                      overlays))
         ;; poss is list of positions of boundaries of text properties
         ;; and start and end positions of overlays
         (poss (sort
                (append
                 (let ((p (point-min)) (pp))
                   (while p
                     (setq pp (cons (cons p (text-properties-at p)) pp))
                     (setq p (next-property-change p)))
                   pp)
                 (mapcar (lambda (o)
                           (list (overlay-start o) 'os (cdr (assq o ois))))
                         overlays)
                 (mapcar (lambda (o)
                           (list (overlay-end o) 'oe (cdr (assq o ois))))
                         overlays))
                ;; sort positions in the descending order
                (lambda (a b) (if (= (car a) (car b))
                                  ;; for equal positions first no prop
                                  (or (null (cadr b))
                                      (and (eq (cadr a) 'os) (eq (cadr b) 'os)
                                           (> (caddr a) (caddr b)))
                                      (and (eq (cadr a) 'oe) (eq (cadr b) 'oe)
                                           (< (caddr a) (caddr b))))
                                (> (car a) (car b))))))
         (p (point)))
    (switch-to-buffer newbuf)
    (insert s)
    (goto-char p)
    (save-excursion
      (mapcar (lambda (pos)
                (goto-char (car pos))
                ;; insert markup from buffer end to the beginning
                (cond
                 ((eq (cadr pos) 'os)
                  (insert (format "<o%s>" (caddr pos))))
                 ((eq (cadr pos) 'oe)
                  (insert (format "</o%s>" (caddr pos))))
                 ((null (cdr pos))
                  (insert "</p>"))
                 (t (let ((props (cdr pos)))
                      (insert "<p")
                      (while props
                        (insert (format " %s=\"" (car props)))
                        (insert
                         (cond
                          ((overlayp (cadr props))
                           (format "o%s" (cdr (assq (cadr props) ois))))
                          (t
                           (format "%s" (cadr props)))))
                        (insert "\"")
                        (setq props (cddr props)))
                      (insert ">")))))
              poss))
    (run-hooks 'my-buffer-xray)))

(add-hook 'my-buffer-xray 'html-mode)


;;; qv (evaluable bookmarks)

;; TODO: use bookmark.el?
;; TODO: add Info node and line number
(defun qv (&optional url anchor)
  "Add or activate live bookmarks.
When called interactively, put the address of the current location
inside a function call to `qv' into the clipboard that can be
pasted in another buffer that stores bookmarks.
Otherwise, after typing `C-x C-e' on the bookmark funcall
goes to the saved location."
  (interactive)
  (if (called-interactively-p 'any)
      (kill-new
       (message "%s"
                (concat "(qv "
                        (cond
                         (buffer-file-name
                          (format "\"%s\"\n    \"^%s$\"" ;; "\"%s\" %s"
                                  buffer-file-name
                                  ;;(line-number-at-pos)
                                  (replace-regexp-in-string
                                   "^[ \t]*" "[ \t]*"
                                   (replace-regexp-in-string
                                    "\"" "\\\\\""
                                    (replace-regexp-in-string
                                     "\\\\" "\\\\\\\\"
                                     (regexp-quote
                                      (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position)))))))))
                        ")")))
    (cond
     ((file-exists-p url)
      (find-file url)
      (cond
       ((integerp anchor)
        (goto-line anchor))
       ((stringp anchor)
        (goto-char (point-min))
        (if (re-search-forward anchor)
            (goto-char (match-beginning 0)))))))))


;;; packages

;; Load some useful packages
(require 'misc)
(require 'tempo)
(require 'wid-edit)
(require 'generic)
(require 'generic-x)
;; Use standard js-mode instead of javascript-generic-mode from generic-x.
(setq auto-mode-alist (delete (rassoc 'javascript-generic-mode auto-mode-alist)
                              auto-mode-alist))

;; (and (require 'ffap) (ffap-bindings))
;; Don't bind ffap keybindings anymore, because now `C-x C-f M-n'
;; gets the filename at point when ffap.el is loaded
(require 'ffap)


;;; ee

(when (require 'ee-autoloads nil t)
  (define-key global-map [f1] 'ee-info)
  (define-key global-map [(control tab)] 'ee-buffers)
  (define-key my-map "eb"  'ee-buffers)
  (define-key my-map "ehc" 'ee-history-command)
  (define-key my-map "ehe" 'ee-history-extended-command)
  (define-key my-map "ehs" 'ee-history-shell-command)
  (define-key my-map "ei"  'ee-imenu)
  (define-key my-map "em"  'ee-marks)
  (define-key my-map "eo"  'ee-outline)
  (define-key my-map "epr" 'ee-programs)
  (define-key my-map "eps" 'ee-ps)
  (define-key my-map "et"  'ee-tags)
  (define-key my-map "ewa" 'ee-windows-add)
  (define-key my-map "eww" 'ee-windows)
  (define-key global-map [(meta  ?\xa7)] 'ee-windows-and-add-current)
  ;; (define-key global-map [(meta ?\x8a7)] 'ee-windows-and-add-current)
  (define-key global-map [(meta     ?`)] 'ee-windows-and-add-current)
  (define-key global-map [(super    ?`)] 'ee-windows-and-add-current)
  (eval-after-load "ee-windows"
    '(progn
       (define-key ee-windows-keymap [(meta  ?\xa7)] 'ee-windows-select-and-delete-current)
       ;; (define-key ee-windows-keymap [(meta ?\x8a7)] 'ee-windows-select-and-delete-current)
       (define-key ee-windows-keymap [(meta     ?`)] 'ee-windows-select-and-delete-current)
       (define-key ee-windows-keymap [(super    ?`)] 'ee-windows-select-and-delete-current)
       (define-key ee-windows-keymap [( ?\xa7)] 'ee-view-record-next)
       ;; (define-key ee-windows-keymap [(?\x8a7)] 'ee-view-record-next)
       (define-key ee-windows-keymap [(    ?`)] 'ee-view-record-next)
       (define-key ee-windows-keymap [( ?\xbd)] 'ee-view-record-prev)
       ;; (define-key ee-windows-keymap [(?\x8bd)] 'ee-view-record-prev)
       (define-key ee-windows-keymap [(    ?~)] 'ee-view-record-prev))))

;; Use standalone wincows.el instead
(when (require 'wincows nil t)
  (define-key global-map [(meta  ?\xa7)] 'wincows)
  ;; (define-key global-map [(meta ?\x8a7)] 'wincows)
  (define-key global-map [(meta     ?`)] 'wincows)
  (define-key global-map [(super    ?`)] 'wincows)
  (eval-after-load "wincows"
    '(progn
       (define-key wincows-mode-map [(meta  ?\xa7)] 'wincows-select)
       ;; (define-key wincows-mode-map [(meta ?\x8a7)] 'wincows-select)
       (define-key wincows-mode-map [(meta     ?`)] 'wincows-select)
       (define-key wincows-mode-map [(super    ?`)] 'wincows-select)
       (define-key wincows-mode-map [( ?\xa7)] 'wincows-next-line)
       ;; (define-key wincows-mode-map [(?\x8a7)] 'wincows-next-line)
       (define-key wincows-mode-map [(    ?`)] 'wincows-next-line)
       (define-key wincows-mode-map [( ?\xbd)] 'wincows-prev-line)
       ;; (define-key wincows-mode-map [(?\x8bd)] 'wincows-prev-line)
       (define-key wincows-mode-map [(    ?~)] 'wincows-prev-line))))


;;; lisp

;; This is my most frequently used command bound to C-RET in Lisp modes.
(defun my-reindent-then-newline-and-indent-and-indent-sexp ()
  "Reindent current line, insert newline, then indent the new line.
Move backward out of one level of parentheses.
Indent each line of the list starting just after point."
  (interactive "*")
  (reindent-then-newline-and-indent)
  (save-excursion
    (condition-case nil (backward-up-list) (error nil))
    (indent-sexp)))

(defun my-join-line-and-indent-sexp ()
  "Join this line to previous and fix up whitespace at join.
Move backward out of one level of parentheses.
Indent each line of the list starting just after point."
  (interactive "*")
  (join-line)
  (save-excursion
    (condition-case nil (backward-up-list) (error nil))
    (let ((indent-sexp-function (key-binding "\e\C-q")))
      (if indent-sexp-function (call-interactively indent-sexp-function)))))

;; This is another frequently used command bound to C-backspace.
;; It's almost the reverse of C-RET defined above.
(defun my-join-line-and-indent-sexp-or-backward-kill-word ()
  "If point is on the whitespaces at the beginning of a line,
then join this line to previous and indent each line of the upper list.
Otherwise, kill characters backward until encountering the end of a word."
  (interactive)
  (if (save-excursion (and (skip-chars-backward " \t") (bolp)))
      (my-join-line-and-indent-sexp)
    (backward-kill-word 1)))

(global-set-key [C-backspace] 'my-join-line-and-indent-sexp-or-backward-kill-word)

;; This is bound to TAB in Lisp modes.
(defun my-lisp-indent-or-complete (&optional arg)
  "Complete Lisp symbol, or indent line or region.
If the character preceding point is symbol-constituent, then perform
completion on Lisp symbol preceding point using `lisp-complete-symbol'.
Otherwise, call `indent-for-tab-command' that indents line or region."
  (interactive "P")
  (if (and (not (and transient-mark-mode mark-active
                     (not (eq (region-beginning) (region-end)))))
           (memq (char-syntax (preceding-char)) (list ?w ?_))
           (not (bobp)))
      (completion-at-point)
    (indent-for-tab-command arg)))

(defun my-beginning-of-line-or-indentation (arg)
  "Jump to the beginning of the line or to the indentation (like `M-m')."
  (interactive "p")
  (if (bolp)
      (beginning-of-line-text arg) ; (back-to-indentation) ?
    (if (fboundp 'move-beginning-of-line)
        (move-beginning-of-line arg)
      (beginning-of-line arg))))
;; (put 'my-beginning-of-line-or-indentation 'isearch-move t)
(define-key global-map [(control ?a)] 'my-beginning-of-line-or-indentation)

(defun my-reindent-then-newline-and-indent ()
  "Create the next number item in the numbered list, or reindent."
  (interactive)
  (let ((num 1))
    (if (save-excursion
          (backward-paragraph)
          (forward-line)
          (not (and (looking-at "^\\s-*\\([0-9]\\)\\.")
                    (setq num (match-string 1)))))
        (reindent-then-newline-and-indent)
      (insert (format "\n\n%s. " (1+ (string-to-number num)))))))

(define-key global-map [(control       return)] 'reindent-then-newline-and-indent)
(define-key global-map [(control shift return)] 'my-reindent-then-newline-and-indent)

(define-key global-map [S-return] 'electric-newline-and-maybe-indent)

;; Lisp mode
(define-key lisp-mode-map [(control return)]
            'my-reindent-then-newline-and-indent-and-indent-sexp)
;; (define-key lisp-mode-map [(control backspace)]
;;             'my-join-line-and-indent-sexp-or-backward-kill-word)
(tempo-define-template "lisp-print-map" '("(map (lambda (x) ) " p ")"))
(define-key lisp-mode-map "\C-zim" 'tempo-template-lisp-print-map)

;; Emacs Lisp mode
(define-key emacs-lisp-mode-map [(control return)]
            'my-reindent-then-newline-and-indent-and-indent-sexp)
;; (define-key emacs-lisp-mode-map [(control backspace)]
;;             'my-join-line-and-indent-sexp-or-backward-kill-word)
(define-key emacs-lisp-mode-map [tab] 'my-lisp-indent-or-complete)
;; use C-M-i instead of
;; (define-key emacs-lisp-mode-map [(control meta tab)] 'lisp-complete-symbol)
;; use C-M-i instead of
;; (define-key emacs-lisp-mode-map "\C-ze\t" 'lisp-complete-symbol)
(define-key emacs-lisp-mode-map "\C-xF"  'find-function)
(define-key emacs-lisp-mode-map "\C-x4F" 'find-function-other-window)
(define-key emacs-lisp-mode-map "\C-x5F" 'find-function-other-frame)
(define-key emacs-lisp-mode-map "\C-xK"  'find-function-on-key)
(define-key emacs-lisp-mode-map "\C-xV"  'find-variable)
(define-key emacs-lisp-mode-map "\C-x4V" 'find-variable-other-window)
(define-key emacs-lisp-mode-map "\C-x5V" 'find-variable-other-frame)
(tempo-define-template "emacs-lisp-print-message" '("(message \"%s\" " p ")"))
(define-key emacs-lisp-mode-map "\C-zim" 'tempo-template-emacs-lisp-print-message)
(tempo-define-template "emacs-lisp-print-defun"
                       '("(defun " p " ()\n  (interactive)\n\n)\n"))
(define-key emacs-lisp-mode-map "\C-zid" 'tempo-template-emacs-lisp-print-defun)

;; Lisp Interaction mode
(define-key lisp-interaction-mode-map [(control return)]
            'my-reindent-then-newline-and-indent-and-indent-sexp)
;; (define-key lisp-interaction-mode-map [(control backspace)]
;;             'my-join-line-and-indent-sexp-or-backward-kill-word)
;; use C-M-i instead of
;; (define-key lisp-interaction-mode-map [(control meta tab)] 'lisp-complete-symbol)
(tempo-define-template "lisp-print-map" '("(map (lambda (x) ) " p ")"))
(define-key lisp-interaction-mode-map "\C-zim" 'tempo-template-emacs-lisp-print-message)

(font-lock-add-keywords
 nil ;; 'emacs-lisp-mode
 `(("\\<lambda\\>"
    (0 (progn (compose-region (match-beginning 0) (match-end 0)
                              ,(make-char 'greek-iso8859-7 107))
              nil)))))

(eval-after-load "scheme"
  '(progn
     (define-key scheme-mode-map [(control return)]
                 'my-reindent-then-newline-and-indent-and-indent-sexp)
     ;; (define-key scheme-mode-map [(control backspace)]
     ;;             'my-join-line-and-indent-sexp-or-backward-kill-word)
     ))


;;; clojure

(eval-after-load "clojure-mode"
  '(progn (add-hook 'clojure-mode-hook
                    (lambda ()
                      (set (make-local-variable 'inferior-lisp-program)
                           ;; For latest version:
                           "lein repl"
                           ;; "java -cp /home/work/java/clojure/jar/clojure-1.7.0-alpha1.jar clojure.main"
                           ;; "java -cp /home/work/java/clojure/jar/clojure-1.5.0-RC4.jar clojure.main"
                           ;; For 1.2 with init file:
                           ;; "java clojure.main -i ~/init.clj"
                           ;; "java clojure.main"
                           ;; For OLD version 1.0 (deprecated):
                           ;; "java -cp clojure.jar clojure.lang.Repl"
                           ;; For installed version in Ubuntu:
                           ;; "clojure"
                           )))

          ;; FROM https://github.com/weavejester/compojure/wiki/Emacs-indentation
          (define-clojure-indent
            (defroutes 'defun)
            (GET 2)
            (POST 2)
            (PUT 2)
            (DELETE 2)
            (HEAD 2)
            (ANY 2)
            (context 2))
          ))

;; qv http://stackoverflow.com/questions/3528705/clojure-inferior-lisp-window-in-emacs-being-displayed-over-code-when-doing-c-c-c
(setq same-window-buffer-names (delete "*inferior-lisp*" same-window-buffer-names))

(eval-after-load "cc-mode"
  '(progn (add-hook 'java-mode-hook
                    (lambda ()
                      (setq tab-width 4)))))

(eval-after-load "css-mode"
  '(progn (add-hook 'css-mode-hook
                    (lambda ()
                      (setq tab-width 2)))))

(eval-after-load "js"
  '(progn (add-hook 'js-mode-hook
                    (lambda ()
                      (setq js-indent-level 2)
                      (setq tab-width 2)))))

(eval-after-load "ruby-mode"
  '(progn (add-hook 'ruby-mode-hook
                    (lambda ()
                      (set (make-local-variable 'require-final-newline) nil)))))


;;; snd

(autoload 'sndtr-mode "sndtr" "Major mode for editing Snd transcripts." t)
;; transcripts sndtr files
(add-to-list 'auto-mode-alist '("\\.trs\\'" . sndtr-mode))
;; marks snd files
(add-to-list 'auto-mode-alist '("\\.marks\\'" . scheme-mode))

(defun run-snd ()
  (interactive)
  (run-scheme "snd -notebook" "snd"))

;; Added "<" for Scheme "#<unspecified>"
(setq inferior-lisp-prompt "^[^<> \n]*>+:? *")

;(define-key inferior-scheme-mode-map [(meta down)] 'comint-next-prompt)
;(define-key inferior-scheme-mode-map [(meta up)] 'comint-previous-prompt)
(add-hook
 'inferior-scheme-mode-hook
 (lambda ()
   ;; no special variable for prompt in cmuscheme.el
   (setq comint-prompt-regexp "^[^<>\n]*>+ *") ; added "<"
   (define-key global-map "\C-zii"
     (lambda ()
       (interactive)
       (let* ((proc (scheme-proc))
              (m (marker-position (process-mark proc)))
              (str
               (save-excursion
                 (comint-send-string
                  proc
                  "(list (selection-position) (selection-length))\n")
                 (accept-process-output proc)
                 (set-buffer "*scheme*")
                 (buffer-substring
                  m
                  (marker-position (process-mark proc))))))
         (insert str))))))


;;; dsssl

;; Make font-lock recognise more DSSSL keywords.
;; (setq scheme-font-lock-keywords
;;      (cons '("(\\(make\\|element\\|style\\|mode\\|root\\|with-mode\\)[ \t\n]\
;;                \\([0-9a-z.-]+\\|([^)]+)\\)"
;;              (1 font-lock-keyword-face)
;;              (2 font-lock-function-name-face))
;;            scheme-font-lock-keywords))

;; Use Scheme mode for DSSSL files.
;; (add-to-list 'auto-mode-alist '("\\.dss?s?l$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss$" . scheme-mode))


;;; perl

;; Use cperl mode instead of perl mode
;; PS: Don't use over-bloated cperl mode; use default perl mode instead
;; (defalias 'perl-mode 'cperl-mode)
;; (fset 'perl-mode 'cperl-mode)

(add-to-list
 'auto-insert-alist
 '(perl-mode
   nil
   "#!/usr/bin/perl -w" \n
   "# -*- Perl -*-" \n
   ;; "# \$Id\$" \n
   ;; "# \$RCSfile\$\$Revision\$\$Date\$" \n
   "# \$Revision\$" \n
   \n
   "while (<>) {" \n
   > "chomp;" \n
   > _ \n
   > "print \"$_\\n\";\n"
   "}\n"))

(tempo-define-template "perl-skeleton"
 '("#!/usr/bin/perl -w\n# -*- Perl -*-\n# \$Revision\$\n\nwhile (<>) {\n  chomp;\n  "
   p "\n}\n"))
(tempo-define-template "perl-s-skeleton" '("s/" p "//;"))
(tempo-define-template "perl-print-skeleton" '("print \"$_" p "\\n\";"))
(tempo-define-template "perl-while-skeleton" '("while (<>) {\n  chomp;\n  " p "\n}\n"))

(eval-after-load "perl-mode"
  '(progn
     ;; (define-auto-insert 'perl-mode (lambda () (tempo-template-perl-skeleton)))
     (define-key perl-mode-map "\C-ziw" 'tempo-template-perl-while-skeleton)
     (define-key perl-mode-map "\C-zip" 'tempo-template-perl-print-skeleton)
     (define-key perl-mode-map "\C-zis" 'tempo-template-perl-s-skeleton)))

;; Try to distinguish between Perl and Prolog file types
;; TODO: make/use external programs (a-la 'file')
;; but best solution is to use "-*- mode: -*-" in the first line
;; qv http://thread.gmane.org/gmane.emacs.devel/114377/focus=114713
(setq auto-mode-alist
      (append '(("\\.perl\\'" . perl-mode)
                ("\\.pm\\'" . perl-mode)
                ;; pl files in *perl* dir are Perl files
                ;; ("perl.*\\.pl\\'" . perl-mode)
                ("\\.pl\\'" . perl-mode))
              auto-mode-alist))
(defun my-pl-find-file-hook ()
  ;; To distinguish Prolog and Perl files with the same file extension
  ;; '.pl', it assumes that Perl programs begin with a comment '#',
  ;; but this doesn't work yet for Prolog shell scripts, so it's more
  ;; reliable to use file local variables with the needed mode specified.
  (if (and (looking-at "#")
           (or
            ;; This works when '.pl' is associated with Prolog mode
            (string-match "Prolog" mode-name)
            ;; BTW, Perl mode fits perfectly for different conf-files
            (equal mode-name "Fundamental")))
      (perl-mode)))
;; (add-hook 'find-file-hooks 'my-pl-find-file-hook)

;; Create Perl links in the *Man* buffer
(eval-after-load "man"
  '(progn
     (add-hook
      'Man-cooked-hook
      (lambda ()
        ;; TODO: add to perl-mode.el? and cperl-mode.el?
        ;; BAD: it breaks links followed with a dot!
        (if (string-match "^\\([0-9]+ *\\)?perl" Man-arguments)
            (Man-highlight-references0
             "DESCRIPTION"
             "\\(perl\\(?:[a-z0-9]+[a-z]\\|[a-z][a-z0-9]+\\)\\)[^a-z0-9]"
             1 0 'Man-xref-man-page))))))


;;; prolog

(setq prolog-system 'swi)
(setq prolog-indent-width 8)
(setq prolog-electric-dot-flag t)
(setq prolog-program-switches
  '((sicstus ("-i"))
    (swi ("-G8M"))
    (t nil)))
(setq prolog-info-predicate-index "(prolog)Predicates188")

;; Use better prolog-mode from http://www.emacswiki.org/emacs/PrologMode
;; renamed here to prolog2.el
;; (load "progmodes/prolog2.el")
;; (autoload 'run-prolog   "prolog2" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode  "prolog2" "Major mode for editing Prolog programs." t)
;; (autoload 'mercury-mode "prolog2" "Major mode for editing Mercury programs." t)

;; (setq outline-regexp "[0-9]+ \\?-") ; for *prolog*
(setq auto-mode-alist
      (append '(
               ;; ("\\.pl?\\'" . 'prolog-mode) ; SWI Prolog
               ;; pl files in *prolog* dir are Prolog files
               ("prolog.*\\.pl?\\'" . prolog-mode) ; SWI Prolog
               ("\\.[Pp][Rr][Oo]\\'" . prolog-mode)
               ("\\.ari\\'" . prolog-mode) ; Arity Prolog
               ) auto-mode-alist))

;; Resolve file extension conflict between Octave and Mercury Prolog
;; in favor of Mercury Prolog
;; (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . mercury-mode))

(add-hook
 'prolog-mode-hook
 (lambda ()
   (require 'prolog)
   (setq prolog-system 'swi)
   (define-key prolog-mode-map [(control f1)]
     (lambda () (interactive) (my-search-prolog-doc-at-point)))
   ;;(fset 'prolog-add-predicate-comment
   ;;       [C-f5 up up ?\M-3 ?% ?  ?\M-2 C-right ?\C-k ?\C-m ?\M-2 ?% ?  ? ])
   (fset 'prolog-add-predicate-comment
         [?\C-n ?\C-o C-f5 ?\C-a up ?\M-3 ?% ?  ?\M-2 C-right ?\C-k ?\C-m ?\M-2 ?% ?  ? ])
   (define-key prolog-mode-map "\C-zic" 'prolog-add-predicate-comment)
   ;; (define-key prolog-mode-map "\C-zic"
   ;;   (lambda () (interactive) (end-of-line) (insert-string " :- ")))
   ;; (define-key prolog-mode-map "\C-zi,"
   ;;   (lambda () (interactive) (end-of-line) (insert-string ", ")))
   ;; (define-key prolog-mode-map "\C-zi."
   ;;   (lambda () (interactive) (end-of-line) (insert-string ".") (newline)))
   ;; (defun prolog-outline-level () (- 4 (outline-level)))
   (set (make-local-variable 'outline-regexp) "%%%+")
   (set (make-local-variable 'outline-level) (lambda () (- 5 (outline-level))))
   ;; (setq outline-level 'prolog-outline-level)
   ;; global-font-lock-mode doesn't work with prolog.el, but works with prolog2.el
   ;; (font-lock-mode 1)
   ))

(add-hook
 'prolog-inferior-mode-hook
 (lambda ()
   ;; (setq comint-input-ring-file-name "~/.pl_history")
   ;; (comint-read-input-ring t)
   ;; THIS CAUSED TRANSIENT-MODE NOT-WORKING !!!
   ;; -> (add-hook 'pre-command-hook 'comint-write-input-ring)
   (define-key prolog-inferior-mode-map [(control f1)]
     (lambda () (interactive) (my-search-prolog-doc-at-point)))
   (define-key prolog-inferior-mode-map "\C-zo" 'comint-kill-output-since-last-prompt)
   (set (make-local-variable 'outline-regexp) "^[1-9][0-9]* \\?- ")
   (set (make-local-variable 'outline-level) (lambda () 1))))

(defun my-search-prolog-doc-at-point ()
  (let* ((wordchars "a-zA-Z_0-9")
         (str
          (concat "\^L\n\n"
                  (current-word)
                  ;; (buffer-substring-no-properties
                  ;; (save-excursion (skip-chars-backward wordchars) (point))
                  ;; (save-excursion (skip-chars-forward  wordchars) (point)))
                  "(")))
    (view-file "~/doc/prog/prolog/PROLOG")
    ;; (set (make-local-variable 'outline-regexp) "^\\(Chapter [0-9]\\|\\)")
    ;; (make-local-variable 'outline-level)
    (if (not (re-search-forward str nil t))
        (progn
          (goto-char (point-min))
          (re-search-forward str nil t)))
    (outline-show-entry) ;?
    (message str)))

;; for PROLOG manual:
;; outline-regexp: "Chapter\\|[0-9]\\.[0-9]+ .....\\|[0-9]+\\.[0-9]+\\.[0-9]+ ....."
;; outline-level: outline-level-for-prolog-manual
;; mode: outline-minor
;; (setq outline-regexp "Chapter\\|[0-9]+\\.[0-9]+ .....\\|[0-9]+\\.[0-9]+\\.[0-9]+ .....")
;; (setq outline-level (lambda ()
;;                       (save-excursion
;;                         (cond
;;                          ((looking-at "Chapter") 1)
;;                          ((looking-at "[0-9]+\\.[0-9]+ ") 2)
;;                          ((looking-at "[0-9]+\\.[0-9]+\\.[0-9]+ ") 3)))))
;; (defun outline-level-for-prolog-manual ()
;;   (save-excursion
;;     (cond
;;      ((looking-at "Chapter") 1)
;;      ((looking-at "[0-9]+\\.[0-9]+ ") 2)
;;      ((looking-at "[0-9]+\\.[0-9]+\\.[0-9]+ ") 3))))


;;; erlang

;; TODO: for Yaws templates use mumamo with erlang-mode and html-mode
(add-to-list 'auto-mode-alist '("\\.yaws\\'" . erlang-mode))

(setq erlang-inferior-shell-split-window nil)

(eval-after-load "erlang"
  '(progn (add-hook 'erlang-mode-hook
                    (lambda ()
                      (setq tab-width 2)
                      (alchemist-mode 1)))))

(eval-after-load "alchemist"
  '(progn
     ;; (global-company-mode)
     ;; (define-key elixir-mode-map "\M-\t" 'company-complete)
     ;; (define-key company-active-map [escape] 'company-abort)
     ))


;;; haskell

;; also qv comment in (qv "files.el" "^(defvar interpreter-mode-alist")
(add-to-list 'interpreter-mode-alist '("runhugs" . literate-haskell-mode))


;;; html

(add-to-list 'auto-mode-alist '("\\.mustache\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))

;; These are needed to set before loading sgml-mode.el:
;; (setq sgml-quick-keys t)
(setq html-quick-keys t)
(eval-after-load "sgml-mode"
  '(progn
     (modify-syntax-entry ?. "." sgml-mode-syntax-table)
     (modify-syntax-entry ?: "." sgml-mode-syntax-table)
     (setq html-tag-face-alist (append '(("a" . underline))
                                       html-tag-face-alist))))

(eval-after-load "sgml-mode"
  '(progn
(define-skeleton html-headline-1
  "HTML level 1 headline tags."
  nil
  "<h1><a name=\"" (setq str (read-string "Name: "))
  "\" id=\"" str "\">" _ "</a></h1>")

(define-skeleton html-headline-2
  "HTML level 2 headline tags."
  nil
  "<h2><a name=\"" (setq str (read-string "Name: "))
  "\" id=\"" str "\">" _ "</a></h2>")))

(add-hook 'html-mode-hook 'turn-off-auto-fill)
(add-hook 'html-mode-hook
          (lambda ()
            ;; (define-key html-mode-map [?\x8a0] (lambda))
            (define-key html-mode-map "\C-c&" 'sgml-name-char)
            ;; (define-key html-mode-map "\C-z" my-map)
            ))
(add-hook 'html-mode-hook
          (lambda ()
            ;; Because `sgml-mode' overrides the user customization
            ;; sgml-xml-mode=t with the value from `sgml-xml-guess'.
            (setq sgml-xml-mode t)
            ))

(defvar my-auto-insert-html-mode-language "en")
(add-to-list
 'auto-insert-alist
 '(html-mode
   nil
   (when (string-match "\\.\\([^.][^.]\\)\\.html$" buffer-file-name)
     (setq my-auto-insert-html-mode-language
           (match-string 1 buffer-file-name))
     "")
   ;; "<?xml version=\"1.0\" encoding=\""
   ;; (if (equal my-auto-insert-html-mode-language "ru") "KOI8-R" "ISO-8859-1")
   ;; "\"?>\n"
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
   "     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
   "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\""
   my-auto-insert-html-mode-language "\" lang=\""
   my-auto-insert-html-mode-language "\">\n"
   "<head>\n"
   "<meta http-equiv=\"Content-Type\" content=\"text/html; charset="
   (if (equal my-auto-insert-html-mode-language "ru") "koi8-r" "iso-8859-1")
   "\" />\n"
   "<meta http-equiv=\"Content-Language\" content=\""
   my-auto-insert-html-mode-language "\" />\n"
   "<meta name=\"description\" content=\"\" />\n"
   "<meta name=\"keywords\" content=\"\" />\n"
   "<title>" (capitalize (setq str (downcase (read-string "Title: ")))) "</title>\n"
   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/jurta.css\" />\n"
   "</head>\n"
   "<body>\n<h1><a name=\"" str "\" id=\"" str "\">" str "</a></h1>\n"
   _
   "\n"
   "</body>\n"
   "</html>\n"))


;;; htmlize

;; (global-set-key [print] 'htmlize-buffer)
(global-set-key [print] 'htmlfontify-buffer)


;;; debug

;; Add key bindings similar to IDEBUG or Turbo Debugger
(defun my-gud-gdb-find-file-OLD (f)
  ;; TODO: make gdb minor mode to make rebindings more easy !
  (save-excursion
    (let ((buf (find-file-noselect f)))
      (set-buffer buf)
      ;; TODO: why this is needed?
      (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
      ;; TODO: use (local-set-key) instead
      (define-key (current-local-map) [f4]
        (lambda () (interactive) (gud-call "tbreak %f:%l") (gud-call "cont")))
      (define-key (current-local-map) [f5]
        (lambda () (interactive)
           (if (and transient-mark-mode mark-active)
               (gud-call (concat "print "
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))))
             (gud-call "print %e"))))
      (define-key (current-local-map) [f7]
        (lambda () (interactive) (gud-call "step %p")))
      (define-key (current-local-map) [f8]
        (lambda () (interactive) (gud-call "next %p")))
;; TODO: try next instead of prev
;;       (local-set-key [f5] (lambda () (interactive) (gud-call "print %e")))
;;       (local-set-key [f7] (lambda () (interactive) (gud-call "step %p")))
;;       (local-set-key [f8] (lambda () (interactive) (gud-call "next %p")))
      ))
  (gud-gdb-find-file f))

;; (add-hook 'gdb-mode-hook
;;           (lambda ()
;;             (setq gud-find-file 'my-gud-gdb-find-file)))

(defun my-gud-perldb-find-file (f)
  (save-excursion
    (let ((buf (find-file-noselect f)))
      (set-buffer buf)
      ;; TODO: why this is needed?
      (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
      ;; TODO: use (local-set-key) instead
      (define-key (current-local-map) [f4]
        (lambda () (interactive) (gud-call "c %l")))
      (define-key (current-local-map) [f5]
        (lambda () (interactive) (gud-call ".")))
      (define-key (current-local-map) [f6]
        (lambda () (interactive) (gud-call "x %e")))
      (define-key (current-local-map) [f7]
        (lambda () (interactive) (gud-call "s")))
      (define-key (current-local-map) [f8]
        (lambda () (interactive) (gud-call "n")))))
  (gud-perldb-find-file f))

(add-hook 'perldb-mode-hook
          (lambda ()
            (setq gud-find-file 'my-gud-perldb-find-file)))


;;; outline

(define-key global-map [(shift f5)] 'outline-minor-mode)

(eval-after-load "outline"
  '(progn
     (define-key outline-mode-map [(control ?o)] outline-mode-prefix-map)
;;   (define-key outline-mode-map [(control meta left)]
;;               'my-outline-hide-entry-or-subtree)
;;   (define-key outline-mode-map [(control meta right)]
;;               'my-outline-show-entry-or-subtree)
;;   (define-key outline-mode-map [(meta down)] 'outline-next-visible-heading)
;;   (define-key outline-mode-map [(meta up)]   'outline-previous-visible-heading)
;;   (define-key outline-mode-map [(control meta down)] 'outline-forward-same-level)
;;   (define-key outline-mode-map [(control meta up)]   'outline-up-heading)
;;     (define-key outline-mode-map [(meta up)]    'my-outline-prev-or-up)
;;     (define-key outline-mode-map [(meta left)]
;;       (lambda () (interactive) (outline-hide-subtree))) ;; (outline-hide-entry)
;;     (define-key outline-mode-map [(meta right)]
;;       (lambda () (interactive) (outline-show-children) (outline-show-entry)))
;; Use `C-M-l' or `C-l' instead of `f5'
;;     (define-key outline-mode-map [f5]
;;       (lambda () (interactive) (recenter 0) (outline-show-entry) (beginning-of-line)))
     (define-key outline-mode-map [(control ?*)]
       (lambda () (interactive) (outline-show-branches)))
     (define-key outline-minor-mode-map [f5] outline-mode-prefix-map)
;;   (define-key outline-minor-mode-map [(control meta left)]
;;               'my-outline-hide-entry-or-subtree)
;;   (define-key outline-minor-mode-map [(control meta right)]
;;               'my-outline-show-entry-or-subtree)
;;   (define-key outline-minor-mode-map [(meta down)] 'outline-next-visible-heading)
;;   (define-key outline-minor-mode-map [(meta up)]   'outline-previous-visible-heading)
;;   (define-key outline-minor-mode-map [(control meta down)] 'outline-forward-same-level)
;;   (define-key outline-minor-mode-map [(control meta up)]   'outline-up-heading)
;;     (define-key outline-minor-mode-map [(meta up)]    'my-outline-prev-or-up)
;;     (define-key outline-minor-mode-map [(meta left)]
;;       (lambda () (interactive) (outline-hide-subtree))) ;; (outline-hide-entry)
;;     (define-key outline-minor-mode-map [(meta right)]
;;       (lambda () (interactive) (outline-show-children) (outline-show-entry)))
;; Use `C-M-l' or `C-l' instead of `f5'
;;     (define-key outline-minor-mode-map [f5]
;;       (lambda () (interactive) (recenter 0) (outline-show-entry) (beginning-of-line)))
     (define-key outline-minor-mode-map [(control ?*)]
       (lambda () (interactive) (outline-show-branches)))
     (require 'foldout)))

;; Start outline mode with hidden sublevels or hidden body
(add-hook
 'outline-mode-hook
 (lambda ()
   ;; (outline-hide-sublevels 1) ; alternative
   ;; (outline-hide-body)
   ))

;; Start outline minor mode with hidden sublevels or hidden body
(add-hook
 'outline-minor-mode-hook
 (lambda ()
   ;; (outline-hide-sublevels 1) ; alternative
   ;; (outline-hide-body)
   ))

;; this is old and bad
;; (defun my-outline-hide-entry-or-subtree ()
;;   (interactive)
;;   (if (save-excursion (next-line 1) (looking-at outline-regexp))
;;       ;; (save-excursion (outline-end-of-heading) (outline-visible))
;;       (outline-hide-subtree)
;;     (progn (outline-hide-entry) (beginning-of-line))))

(defun my-outline-hide-entry-or-subtree ()
  (interactive)
  (if (save-excursion (next-line 1) (or (looking-at outline-regexp) (eobp)))
      (if (>= (funcall outline-level)
              (save-excursion (next-line 1)
                              (or (and (eobp) 0) (funcall outline-level))))
          (if (= (funcall outline-level) 1)
              (goto-char (point-min))
            (outline-up-heading 1))
        (outline-hide-subtree))
    (progn (outline-hide-entry) (beginning-of-line))))

(defun my-outline-show-entry-or-subtree ()
  (interactive)
  (if (save-excursion
        (re-search-forward (concat "\n\\(" outline-regexp "\\)")
                           (save-excursion
                             (outline-next-visible-heading 1) ; (next-line 1)
                             (point))
                           t))
      (outline-show-children)
    (outline-show-entry)))

;;  (defun my-outline-prev-or-up ()
;;    (interactive)
;;    (if (and (looking-at outline-regexp)
;;            (= (funcall outline-level)
;;               (save-excursion (outline-previous-visible-heading 1)
;;                               (funcall outline-level))))
;;        (outline-up-heading 1)
;;      (outline-previous-visible-heading 1)))

;;  (defun my-outline-hide-or-up ()
;;    (interactive)
;;    (if (save-excursion (outline-end-of-heading) (outline-visible))
;;        (outline-hide-subtree)
;;      (outline-up-heading 1)))


;;; diff

(eval-after-load "diff-mode"
  '(progn
     (define-key diff-mode-map [(control meta down)] 'diff-hunk-next)
     (define-key diff-mode-map [(control meta up)]   'diff-hunk-prev)
     (define-key diff-mode-map [(control meta shift down)] 'diff-file-next)
     (define-key diff-mode-map [(control meta shift up)]   'diff-file-prev)
     (define-key diff-mode-map [(control return)] 'diff-goto-source-kill-buffer)

     (add-hook 'diff-mode-hook 'rename-uniquely)

     ;; These commented out lines no more needed because diff-font-lock-keywords
     ;; was changed to use `diff-indicator-...' faces for that
     ;; (setcar (assoc "^!.*\n" diff-font-lock-keywords) "^!")
     ;; (setcar (assoc "^[+>].*\n" diff-font-lock-keywords) "^[+>]")
     ;; (setcar (assoc "^[-<].*\n" diff-font-lock-keywords) "^[-<]")
     ;; (setcdr (assoc "^#.*" diff-font-lock-keywords) font-lock-comment-face)
     ;; (setq diff-font-lock-keywords
     ;;       (append diff-font-lock-keywords
     ;;               '(("^[!-+<>]"
     ;;                  (0 diff-marker-face prepend)))))
     ;; Use the colors of savannah.gnu.org WebCVS diffs.
     ;; Commented out because the repo for 24.2 should have them.
     ;; (set-face-background 'diff-file-header "#bbbbbb")
     ;; (set-face-background 'diff-header  "#dddddd")
     ;; (set-face-background 'diff-added   "#eeffee")
     ;; (set-face-background 'diff-changed "#ffffee")
     ;; (set-face-background 'diff-removed "#ffeeee")
     ;; (set-face-background 'diff-indicator-added   "#eeffee")
     ;; (set-face-background 'diff-indicator-changed "#ffffee")
     ;; (set-face-background 'diff-indicator-removed "#ffeeee")
     ;; (set-face-background 'diff-refine-change "#fffbbf")
     ;; Make revision separators more noticeable:
     (setq diff-font-lock-keywords
           (append diff-font-lock-keywords
                   '(("^\\(?:diff\\|revno:\\|Only in\\|Binary files\\)" (0 'match prepend)))))))

(defun diff-goto-source-kill-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (call-interactively 'diff-goto-source)
    (kill-buffer buf)))

(define-generic-mode 'diff-generic-mode
  (list ?#)
  nil
  '(("^\\(<-? \\)" 1 'font-lock-keyword-face)
    ("^\\(-?> \\)" 1 'font-lock-function-name-face)
    ("^\\(\\(<!\\|!>\\) .*\\)" 1 'font-lock-warning-face))
  (list "\\.subpatch\\'")
  nil
  "For diffuniq and subpatch.")

;; Prevent git-diff from calling pager
;; (setenv "PAGER" "/bin/cat")
;; (setenv "PAGER") (getenv "PAGER")


;;; text

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'fill-nobreak-predicate 'fill-french-nobreak-p)
(add-hook 'fill-nobreak-predicate 'fill-single-word-nobreak-p)

(add-hook 'text-mode-hook       'flyspell-mode)
(add-hook 'change-log-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook       'flyspell-prog-mode)

;; Flyspell only on typing, not on moving point
;; (add-hook 'flyspell-mode-hook
;;           (lambda ()
;;             (remove-hook 'post-command-hook (function flyspell-post-command-hook) t)
;;             (add-hook 'after-change-functions
;;                       (lambda (start stop len)
;;                         (flyspell-post-command-hook)) t t)))

;; Alternative solution for Flyspell only on typing, not on moving point
(add-hook 'flyspell-mode-hook
          (lambda ()
            (advice-add 'flyspell-check-pre-word-p :override (lambda () nil))
            ;; After evaluating the next, flyspell doesn't check the last word
            ;; in `auto-fill-mode' when typing SPC moves to the next line,
            ;; because the order of calls in `internal_self_insert' is:
            ;; 1. insert_and_inherit ();
            ;; 2. Frun_hook_with_args (Qafter_change_functions);
            ;; 3. auto_fill_result = call0 (BVAR (current_buffer, auto_fill_function));
            ;; 4. Frun_hooks (1, &Qpost_self_insert_hook);
            (advice-add 'flyspell-check-word-p     :override (lambda () nil))))


;;; view

(eval-after-load "view"
  '(progn
     ;; Shift-Space to scroll back (already added in bug#2145).
     ;; (define-key view-mode-map [?\S-\ ] 'View-scroll-page-backward)
     (define-key view-mode-map " " 'View-scroll-page-forward-set-page-size)
     (define-key view-mode-map "g" (lambda () (interactive) (revert-buffer nil t t)))
     (define-key view-mode-map "l" 'View-goto-line)
     (define-key view-mode-map [f2] 'toggle-truncate-lines)
     ;; (define-key view-mode-map [tab] 'other-window) ; used for next-ref
     ;; global: (define-key view-mode-map [(meta right)] 'find-file-at-point)
     ;; Commented out to use the global keybinding:
     ;; (define-key view-mode-map [(meta left)]
     ;;   (lambda ()
     ;;     (interactive)
     ;;     ;; Go to the top to not store emacs-places.
     ;;     (goto-char (point-min))
     ;;     (View-quit)))
     (define-key view-mode-map [(meta down)]
       (lambda ()
         (interactive)
         (if (>= (window-end) (point-max))
             (goto-char (point-max))
           (View-scroll-page-forward-set-page-size))))
     (define-key view-mode-map [(meta up)]
       (lambda ()
         (interactive)
         (if (<= (window-start) (point-min))
             (goto-char (point-min))
           (View-scroll-page-backward-set-page-size))))
     ;; qv http://thread.gmane.org/gmane.emacs.devel/111117/focus=112357
     (defadvice View-scroll-line-forward (after my-View-scroll-line-forward activate)
       "Fix point position to be at the bottom line."
       (move-to-window-line -1)
       (beginning-of-line))
     ))


;;; doc-view

(eval-after-load "doc-view"
  '(progn
     ;; Shift-Space to scroll back (already added in bug#2145).
     ;; (define-key doc-view-mode-map [?\S-\ ] 'doc-view-scroll-down-or-previous-page)
     (define-key doc-view-mode-map [(meta left)] 'quit-window-kill-buffer)
     ))


;;; image-mode

(eval-after-load "image-mode"
  '(progn
     ;; Shift-Space to scroll back (already added in bug#2145).
     ;; (define-key image-mode-map [?\S-\ ] 'image-scroll-down)
     (define-key image-mode-map "q" 'quit-window-kill-buffer)
     (define-key image-mode-map [(meta left)] 'quit-window-kill-buffer)))


;;; image

;; This is now in `image-dired-cmd-create-standard-thumbnail-command'
;; (used by `C-t C-t' in Dired).
(defun my-make-thumbnail (file)
  (let* ((image-file (concat "file://" (expand-file-name file)))
         (thumb-file (expand-file-name
                      (concat "~/.thumbnails/normal/" (md5 image-file) ".png")))
         (file-attrs (file-attributes file))
         (modif-time (float-time (nth 5 file-attrs))))
    (unless (file-exists-p thumb-file)
      (shell-command
       (mapconcat
        'identity
        (list
         "convert"
         (format "\"%s\"" file)
         ;; "-size 128x128"
         (format "-set \"Description\" \"Thumbnail of %s\"" image-file)
         (format "-set \"Software\" \"ImageMagick, GNU Emacs %s\"" emacs-version)
         (format "-set \"Thumb::URI\" \"%s\"" image-file)
         (format "-set \"Thumb::MTime\" \"%.0f\"" modif-time)
         "-set \"Thumb::Size\" \"%b\""
         "-set \"Thumb::Image::Width\" \"%w\""
         "-set \"Thumb::Image::Height\" \"%h\""
         "-set \"Thumb::Image::Mimetype\" \"image/jpeg\""
         "-resize 128x128" ;; "-resize 64x64"
         "+profile \"*\""
         "-type TrueColorMatte"
         ;; "-sharpen 11" ; TRY THIS
         (format "png:\"%s\"" thumb-file))
        " ")))
    thumb-file))


;;; thumbs

(defadvice thumbs-mode (after my-thumbs-mode activate)
  (toggle-truncate-lines -1))


;;; dired

(require 'dired-x)

;; HINT: the following expression is useful for `M-(' `dired-mark-sexp'
;; to mark files by their type:
;; (string-match "perl" (shell-command-to-string (concat "file " name)))

;; Uses editor/viewer info from /usr/bin/run-mailcap
(defun my-dired-run-find-file ()
  "My view file for dired."
  (interactive)
  (let* ((file (dired-get-filename)))
    (cond
     ((let* ((command
              (and (functionp 'mm-mime-info)
                   (mm-mime-info
                    (mm-extension-to-mime (file-name-extension file))))))
        (if (and command (stringp command))
            ;; always return `t' for `cond'
            (or (ignore (shell-command (concat (format command file) "&")))
                t))))
     ;; ((string-match "\\.html?$" file) (w3-open-local file))
     ((string-match "\\.html?$" file)
      (cond
       ((fboundp 'w3m-goto-url-new-session)
        (w3m-find-file-new-session file))
       ((fboundp 'browse-url)
        (browse-url file))))
     ((string-match "\\.elc?$" file)
      (load-file file))
     ((string-match "\\.info?$" file)
      (info file))
     (;; (or (string-match "\\.jpe?g$" file)
      ;;           (string-match "\\.gif$" file)
      ;;           (string-match "\\.pdf$" file))
      (let* ((file-list (list (dired-get-filename)))
             (command (dired-guess-default file-list)))
        (if (listp command)
            (setq command (car command)))
        (if command
            (shell-command
             (dired-shell-stuff-it command file-list nil 0)))))
     (t
      (message file)))))

(define-key dired-mode-map [(control return)] 'my-dired-run-find-file)

;; Add different directory sorting keys
(mapc (lambda (elt)
        (define-key dired-mode-map (car elt)
          `(lambda ()
            (interactive)
            (dired-sort-other (concat dired-listing-switches ,(cadr elt))))))
      '(([(control f3)]       ""     "by name")
        ([(control f4)]       " -X"  "by extension")
        ([(control f5)]       " -t"  "by date")
        ([(control f6)]       " -S"  "by size")
        ([(control shift f3)] " -r"  "by reverse name")
        ([(control shift f4)] " -rX" "by reverse extension")
        ([(control shift f5)] " -rt" "by reverse date")
        ([(control shift f6)] " -rS" "by reverse size")))

;; The following two bindings allow to quickly look to the file and return back
;; to dired by pressing [f3] twice (same keys are used in Midnight Commander)
(define-key dired-mode-map [f3]
  (lambda () (interactive) (let (dired-view-command-alist) (dired-view-file))))
(define-key global-map [f3] 'kill-current-buffer)
(define-key global-map [(control f3)] 'kill-current-buffer-and-dired-jump)
(define-key dired-mode-map [(shift f3)] 'dired-find-file-literally)

;; Alternative definitions on keyboards with problematic Fn keys
(define-key global-map "\C-q" 'quit-window-kill-buffer)
(define-key global-map "\C-xj" 'kill-current-buffer-and-dired-jump)
;; (define-key global-map "\C-n" 'kill-current-buffer)
;; (define-key global-map "\C-b" 'kill-current-buffer-and-dired-jump)
;; Other unused keys:
;; "\C-f" (use for search?)
;; "\C-p" (use for pretty-print)
;; "\C-i", "\C-v", "\C-m"

;; The following two bindings allow to open file for editing by [f4],
;; and return back to dired without killing the buffer.
(define-key dired-mode-map [f4] 'dired-find-file) ;; 'dired-view-file
(define-key global-map [f4] 'dired-jump)

(define-key dired-mode-map [(shift f5)] 'dired-count-sizes)

;; TEST
;; (define-key dired-mode-map [up]   'dired-previous-line)
;; (define-key dired-mode-map [down] 'dired-next-line)

;; The following keys resemble *Commander's bindings.
;; But currently I use original Emacs bindings: "C", "R", "D"
;; (define-key dired-mode-map [f5] 'dired-do-copy)
;; (define-key dired-mode-map [f6] 'dired-do-rename)
;; (define-key dired-mode-map [f8] 'dired-do-delete)
(define-key dired-mode-map [delete] 'dired-do-delete)
(define-key dired-mode-map [f7] 'dired-create-directory)
(define-key dired-mode-map [(shift f7)] 'find-dired)

(define-key dired-mode-map [(control meta ?=)] 'dired-compare-directories)

(define-key dired-mode-map [(meta left)]
  ;; Mozilla-like navigation
  (lambda (arg)
     (interactive "P")
     (if (not (and (memq ?R (append dired-actual-switches nil))
                   (dired-between-files)))
         (dired-up-directory)
       (if (dired-subdir-hidden-p (dired-current-directory))
           (dired-tree-up 1)
         (progn (dired-hide-subdir 1) (dired-previous-line 1))))))

(define-key dired-mode-map [(meta right)]
  ;; Mozilla-like navigation
  (lambda (arg)
     (interactive "P")
     (let (dired-view-command-alist)
       (if (not (and (memq ?R (append dired-actual-switches nil))
                     (dired-between-files)))
           (dired-view-file)
         (if (dired-subdir-hidden-p (dired-current-directory))
             (progn (dired-hide-subdir 1)
                    (dired-prev-subdir 1)
                    (dired-next-line 4))
           (dired-view-file))))))

(defun my-dired-move-to-next-dir (arg)
  (interactive "P")
  (if (not (memq ?R (append dired-actual-switches nil)))
      (dired-next-dirline-cycle 1)
    (progn (dired-next-subdir 1))))
(define-key dired-mode-map [(meta down)] 'dired-next-line-cycle) ; dired-next-line
(define-key dired-mode-map [(control meta down)] 'my-dired-move-to-next-dir)
(define-key dired-mode-map [tab] 'my-dired-move-to-next-dir) ;'other-window

(defun my-dired-move-to-prev-dir (arg)
  (interactive "P")
  (if (not (memq ?R (append dired-actual-switches nil)))
      (dired-prev-dirline-cycle 1)
    (progn (dired-prev-subdir 1))))
(define-key dired-mode-map [(meta up)] 'dired-previous-line-cycle) ; dired-previous-line
(define-key dired-mode-map [(control meta up)] 'my-dired-move-to-prev-dir)
(define-key dired-mode-map [(shift iso-lefttab)] 'my-dired-move-to-prev-dir)

(defun my-dired-do-shell-command-on-current-file (&optional arg)
  "Run a shell command on the current file instead of marked files."
  (interactive)
  (let ((dired-marker-char ?M))         ; ?M is unused marker char
    (call-interactively 'dired-do-shell-command)))
(define-key dired-mode-map "%!"
                           'my-dired-do-shell-command-on-current-file)
(define-key dired-mode-map [(control meta ?!)]
                           'my-dired-do-shell-command-on-current-file)

(defun my-dired-mark (arg)
  "Mark ARG files and print the total size of marked files."
  (interactive "P")
  (dired-mark arg)
  (dired-count-sizes dired-marker-char))
(define-key dired-mode-map [insert] 'my-dired-mark)

(defun my-dired-unmark-backward (arg)
  "Move up lines, remove deletion flag there and print size of marked files."
  (interactive "p")
  (dired-unmark-backward arg)
  (dired-count-sizes dired-marker-char))
(define-key dired-mode-map [backspace] 'my-dired-unmark-backward)

(define-key dired-mode-map [(control shift insert)]
  (lambda () (interactive) (dired-copy-filename-as-kill 0)))

;; qv http://thread.gmane.org/gmane.emacs.devel/153150/focus=153151
(define-key dired-mode-map [remap next-line] nil)
(define-key dired-mode-map [remap previous-line] nil)

;; qv http://thread.gmane.org/gmane.emacs.devel/153150
(define-key dired-mode-map "\M-=" 'dired-backup-diff)

;; Get coding from the file, so diff will output in the correct coding:
(defadvice dired-backup-diff (around my-dired-backup-diff act)
  (let* ((filename (dired-get-filename t))
         (coding-system (with-temp-buffer
                          (insert-file-contents filename nil 0 1024)
                          buffer-file-coding-system))
         (coding-system-for-read coding-system)
         (coding-system-for-write coding-system))
    ad-do-it))

;; Get coding from the file, so diff will output in the correct coding:
(defadvice dired-diff (around my-dired-diff act)
  (let* ((filename (dired-get-filename t))
         (coding-system (when (file-regular-p filename)
                          (with-temp-buffer
                            (insert-file-contents filename nil 0 1024)
                            buffer-file-coding-system)))
         (coding-system-for-read coding-system)
         (coding-system-for-write coding-system))
    ad-do-it))

;; (define-key dired-mode-map "\C-y" (lambda (&optional arg)
;;                                     (interactive)
;;                                     (dired-find-file)
;;                                     (goto-char (point-max))
;;                                     (yank arg)))

(define-key dired-mode-map "q" 'quit-window-kill-buffer)

(add-hook 'dired-after-readin-hook
	  (lambda ()
	    ;; Set name of dired buffers to absolute directory name.
	    ;; Use `generate-new-buffer-name' for vc-directory
	    ;; which creates duplicate buffers.
            ;; SEE ALSO http://emacs.stackexchange.com/questions/2123/how-can-i-make-dired-buffer-names-include-the-full-path
	    ;; TODO: Add this feature to `dired-internal-noselect' instead
	    ;; of `(create-file-buffer (directory-file-name dirname))'.
	    (when (stringp dired-directory)
              (rename-buffer (generate-new-buffer-name dired-directory)))))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Omit file extensions only in well-known directories, because
            ;; I don't want to miss omitted files in unknown directories!
            ;; Omit only in some large directories that I use often.
            (when (string-match-p "emacs/\\(git\\|bzr\\|cvs\\)" default-directory)
              (dired-omit-mode 1))
            ;; Use old "\M-o" instead of new "\C-x\M-o".
            (define-key dired-mode-map "\M-o" 'dired-omit-mode)))

(add-hook 'archive-mode-hook
          (lambda ()
            (define-key archive-mode-map [f3] 'archive-view)
            (define-key archive-mode-map "q" 'quit-window-kill-buffer)
            (define-key archive-mode-map [(meta right)] 'archive-view) ;; archive-extract
            (define-key archive-mode-map [(meta left)] 'quit-window-kill-buffer)))

(add-hook 'tar-mode-hook
          (lambda ()
            (define-key tar-mode-map [f3] 'tar-view)
            (define-key tar-mode-map "q" 'quit-window-kill-buffer)
            (define-key tar-mode-map [(meta right)] 'tar-view)
            (define-key tar-mode-map [(meta left)] 'quit-window-kill-buffer)
            (define-key tar-mode-map [(meta up)] 'tar-previous-line)
            (define-key tar-mode-map [(meta down)] 'tar-next-line)))


;;; wdired

;; http://mail.gnu.org/archive/html/emacs-devel/2004-04/msg01190.html
;; http://mail.gnu.org/archive/html/emacs-devel/2004-04/msg01247.html
;; (define-key dired-mode-map "r"        'wdired-change-to-wdired-mode)

;; (OLD) This is a bad keybinding because I sometimes type `C-x C-q'
;; in *Locate* buffer (which is in dired-mode) and do `flush-lines'
;; (define-key dired-mode-map "\C-x\C-q" 'wdired-change-to-wdired-mode)
;; UPDATE: http://thread.gmane.org/gmane.emacs.devel/118678/focus=118684

;; Beware: C-x C-q is bound in locate-mode and find-dired
(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map [return] 'wdired-finish-edit)
     ;; BAD, better to add a new rule at the end of `keyboard-escape-quit':
     ;; (define-key wdired-mode-map [escape] 'wdired-abort-changes)
     ))


;;; locate

;; Redefine `locate-default-make-command-line'.
(defun locate-make-command-line-ignore-case (search-string)
  (list locate-command "-i" search-string))
(setq locate-make-command-line 'locate-make-command-line-ignore-case)

;; Highlight all matches in the *Locate* buffer like in the *Occur* buffer
(add-hook
 'locate-post-command-hook
 (lambda ()
   (save-excursion
     (goto-char (point-min))
     (when (or (re-search-forward "Matches for .* using filter \\(.*\\):" nil t)
               (re-search-forward "Matches for \\(.*\\):" nil t))
       (highlight-regexp
        ;; Case-insensitive hack from `isearch-highlight-regexp'.
        (mapconcat
         (lambda (c)
           (let ((s (string c)))
             (if (string-match "[[:alpha:]]" s)
                 (format "[%s%s]" (upcase s) (downcase s))
               (regexp-quote s))))
         (match-string-no-properties 1) "")
        'match)))))


;;; shell

(eval-after-load "shell"
  '(progn
     (add-hook 'shell-mode-hook 'rename-uniquely)
     ;; Turn off dirtrack because it fails in Bash on Heroku.
     (add-hook 'shell-mode-hook (lambda () (shell-dirtrack-mode -1)))
     (define-key shell-mode-map "\C-d"
       (lambda (&optional arg)
	 (interactive "p")
	 ;; (let* ((proc (get-buffer-process (current-buffer)))))
	 (if (and (eobp)
		  (save-excursion
		    (let ((inhibit-field-text-motion t))
		      (goto-char (line-beginning-position))
		      (looking-at-p "^iex.*>\s*$"))))
	     (let ((process (get-buffer-process (current-buffer))))
	       (process-send-string process ":init.stop()\n"))
	   (comint-delchar-or-maybe-eof arg))))))

;; S-RET switches to the *Shell Command Output* buffer
;; instead of displaying output in the echo area.
;; TODO: Another idea from http://thread.gmane.org/gmane.emacs.bugs/4533
;;       Use M-RETURN to run `async-shell-command'.
(defadvice shell-command (around my-shell-command-around act)
  (let ((messages-buffer-max-lines
         ;; Don't add output to the *Messages* buffer
         ;; when S-RET displays the *Shell Command Output* buffer.
         (unless (memq last-input-event '(S-return ?\C-j))
           messages-buffer-max-lines)))
    ad-do-it
    (when (memq last-input-event '(S-return ?\C-j))
      (message "") ;; Clear the echo area
      (pop-to-buffer "*Shell Command Output*")
      (goto-char (point-min)))))

;; The same as the previous defadvice.
(defadvice dired-do-shell-command (around my-dired-do-shell-command act)
  (let ((messages-buffer-max-lines
         (unless (memq last-input-event '(S-return ?\C-j))
           messages-buffer-max-lines)))
    ad-do-it
    (when (memq last-input-event '(S-return ?\C-j))
      (message "")
      (pop-to-buffer "*Shell Command Output*")
      (goto-char (point-min)))))

;; The same as the previous defadvice.
(defadvice dired-smart-shell-command (around my-dired-smart-shell-command act)
  (let ((messages-buffer-max-lines
         (unless (memq last-input-event '(S-return ?\C-j))
           messages-buffer-max-lines)))
    ad-do-it
    (when (memq last-input-event '(S-return ?\C-j))
      (message "")
      (pop-to-buffer "*Shell Command Output*")
      (goto-char (point-min)))))

;; This command has so many bindings because it's difficult to type with AltGr.
(define-key esc-map "|"    'shell-command-on-region-or-buffer)
(define-key esc-map "\M-|" 'shell-command-on-region-or-buffer) ; `M-ESC |'
(define-key global-map [(control ?|)] 'shell-command-on-region-or-buffer)
(define-key my-map "|" 'shell-command-on-region-or-buffer)


;;; comint

(add-hook 'comint-mode-hook ;; 'comint-load-hook
          (lambda ()
            ;; See http://lists.gnu.org/archive/html/emacs-devel/2014-12/msg00299.html
            (define-key comint-mode-map [S-return] 'newline)
            ;; (define-key comint-mode-map "\C-zo" 'comint-kill-output-since-last-prompt)
            ;; define M-up and M-down instead of C-up and C-down
            (define-key comint-mode-map [(meta down)] 'comint-next-prompt)
            (define-key comint-mode-map [(meta up)] 'comint-previous-prompt)
            (define-key comint-mode-map [C-up]   nil)
            (define-key comint-mode-map [C-down] nil)
            (define-key comint-mode-map "\er" 'comint-history-isearch-backward)))

(when delete-selection-mode
  (put 'comint-delchar-or-maybe-eof 'delete-selection 'supersede))


;;; compile/grep

;; After running compilation/grep hide the header of the compilation/grep
;; buffer which contains information rarely useful to see (i.e. mode:
;; grep; default-directory:..., start time).  This header can be hidden by
;; setting the output window's start at position of the 4-th output line.
;; TODO: try to put 'invisible property on it, because next doesn't work well.
;; But commented out because I don't like this code anymore:
;; (add-hook 'compilation-finish-functions
;;        (lambda (buffer msg)
;;             (mapc (lambda (window)
;;                     (set-window-start window
;;                                       (save-excursion
;;                                         (goto-char (point-min))
;;                                         (line-beginning-position 4))))
;;                   (get-buffer-window-list buffer))))

(eval-after-load "compile"
  '(progn
     (add-hook 'compilation-mode-hook
               (lambda ()
                 ;; (rename-uniquely)
                 (setq buffer-read-only nil)))))

;; Create unique buffer name for `compile' and `grep'.
(setq compilation-buffer-name-function
      (lambda (mode-name)
        (generate-new-buffer-name
         (concat "*" (downcase mode-name) "*"))))

;; Currently NOT USED
(defun my-compilation-buffer-name-function (mode-name)
  (cond
   ((and (eq mode-command major-mode)
         (eq major-mode (nth 1 compilation-arguments)))
    (buffer-name))
   ((let ((window-buffers
           (delete-dups
            (delq nil (mapcar (lambda (w)
                                (with-current-buffer (window-buffer w)
                                  (if (eq mode-command major-mode)
                                      (window-buffer w))))
                              (window-list))))))
      (if (eq (length window-buffers) 1)
          (car window-buffers))))
   ((generate-new-buffer-name
     (concat "*" (downcase mode-name) "*")))))
(if (boundp 'mode-command)
    (setq compilation-buffer-name-function
          'my-compilation-buffer-name-function))

(eval-after-load "grep"
  '(progn
     (push '("rb" . "*.rb") grep-files-aliases)
     (push '("js" . "*.js") grep-files-aliases)
     (push '("clj" . "*.clj") grep-files-aliases)
     ))


;;; proced

(defadvice proced (after my-proced activate)
  (goto-char (point-max))
  (recenter -1))


;;; switch

;; Experiment with more convenient keys than `C-x o' and `M-- C-x o'.
;; (define-key global-map [(control ?\x8a7)] 'other-window)
;; (define-key global-map [(control ?\x8bd)] (lambda () (interactive) (other-window -1)))
(define-key global-map [(control ?\247)] 'other-window)
(define-key global-map [(control ?\275)] (lambda () (interactive) (other-window -1)))
(define-key global-map [(control ?`)] 'other-window)
(define-key global-map [(control ?~)] (lambda () (interactive) (other-window -1)))
(define-key global-map [(control ?<)] 'other-window)
(define-key global-map [(control ?>)] (lambda () (interactive) (other-window -1)))
(define-key global-map [(control ?,)] 'other-window)
(define-key global-map [(control ?.)] (lambda () (interactive) (other-window -1)))
(define-key global-map [(control print)] 'other-window)

;; Something is wrong with buffer lists in built-in functions.
;; TODO: support buffer creation order! (as in Tab-list in some www browsers)
;; (defun my-buffer-next ()
;;   "Primitive buffer navigation function: next-buffer."
;;   (interactive)
;;   (bury-buffer) ;; (switch-to-buffer (other-buffer))
;;   (my-display-prev-next-buffers))
;; (defun my-buffer-prev ()
;;   "Primitive buffer navigation function: prev-buffer."
;;   (interactive)
;;   (switch-to-buffer (car (last (buffer-list))))
;;   (my-display-prev-next-buffers))

;; TODO: use frame parameter `buffer-list': ("buf+1" "buf+2" nil "buf-2" "buf-1")

;; After switching a buffer, display names of adjacent buffers in the echo area.

(defadvice previous-buffer (after my-previous-buffer activate)
  (my-display-prev-next-buffers))

(defadvice next-buffer (after my-next-buffer activate)
  (my-display-prev-next-buffers))

(defun my-display-prev-next-buffers ()
  "Show two previous, current and two next buffer names in the echo area.
Example:
-2:*Messages* -1:*Help*    0:.emacs      1:*info*  2:*scratch*"
  (interactive)
  (let ((i -3) b (bl (buffer-list (selected-frame))) (message-log-max nil))
    (message "%s"
             (mapconcat
              (lambda (x)
                (setq i (+ i 1))
                (format "%d:%-12s"
                        i (substring (buffer-name x) 0
                                     (min (length (buffer-name x)) 11))))
              (append
               (nreverse
                (list
                 (setq b (get-next-valid-buffer (reverse bl) t))
                 (get-next-valid-buffer (cdr (memq b (reverse bl))) t)))
               (list (current-buffer))
               (list
                (setq b (get-next-valid-buffer (cdr bl) t))
                (get-next-valid-buffer (cdr (memq b bl)) t)))
              " "))))


;;; help

;; TODO: same-window-buffer-names and same-window-regexps are obsolete.
(defun describe-function-other-window ()
  (interactive)
  (let (same-window-buffer-names same-window-regexps)
    (call-interactively 'describe-function)))

(defun describe-variable-other-window ()
  (interactive)
  (let (same-window-buffer-names same-window-regexps)
    (call-interactively 'describe-variable)))

(define-key help-map "4f" 'describe-function-other-window)
(define-key help-map "4v" 'describe-variable-other-window)

(define-key help-map "A" 'apropos)
(define-key help-map "\M-f" 'find-function)

;; Separate Help sections with a line of dashes.
(defadvice describe-bindings (after my-describe-bindings activate)
  (with-current-buffer "*Help*"
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (re-search-forward "^\^L$" nil t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'display (concat "^L" (make-string 64 ?-)))
          ;; (overlay-put (make-overlay (match-beginning 0) (match-end 0) nil t)
          ;;              'display (concat (make-string 64 ?-) "^L"))
          ;; (overlay-put (make-overlay (match-beginning 0) (match-end 0) nil t)
          ;;              'before-string (make-string 64 ?-))
          )))
    ;; Comment out to remove startup warnings:
    ;; (set (make-local-variable 'outline-regexp) "^.*:$")
    ;; (outline-minor-mode 1)
    ))

;; Please note that 'help-next-ref' is better than 'Info-next-reference'
;; because it uses 'message' instead of 'error' if "No cross references".
(eval-after-load "help"
  '(progn
    ;; View mode steals key bindings from us.
    ;; doesn't work (set (make-local-variable 'overriding-local-map)
    ;;                   (copy-keymap view-mode-map))
    ;; Does a better method than (car (current-minor-mode-maps)) exist?
    ;; Mozilla-like navigation:
;; DOESN'T WORK (current-minor-mode-maps)
;;         (define-key (car (current-minor-mode-maps)) [(meta left)]  'help-go-back)
;;         (define-key (car (current-minor-mode-maps)) [(meta right)] 'help-follow)
    ;; Lynx-like navigation:
;; DOESN'T WORK (current-minor-mode-maps)
;;             (define-key (car (current-minor-mode-maps)) [(meta up)]
;;               (lambda () (interactive)
;;                  (my-prev-link-or-scroll-page-backward
;;                    (save-excursion
;;                      (ignore-errors (help-previous-ref))
;;                      (point)))))
;;             (define-key (car (current-minor-mode-maps)) [(meta down)]
;;               (lambda () (interactive)
;;                  (my-next-link-or-scroll-page-forward
;;                    (save-excursion
;;                      (ignore-errors (help-next-ref))
;;                      (point)))))
            ))

;; Clicking a link from the *Help* buffer opens source code in the same window.
;; This supposes display-buffer-alist to be customized to contain:
;; '((display-buffer-condition-from-help display-buffer-same-window) ...)
(defun display-buffer-condition-from-help (_buffer-name _action)
  (string-match-p "\\`\\*\\(Help\\)\\*\\(\\|<[0-9]+>\\)\\'" (buffer-name (current-buffer))))


;;; info

;; It's easier to type `C-5 C-h C-i' with control key pressed for all keys:
(define-key global-map "\C-h\C-i" 'info)

;; Info with look-and-feel of Midnight Commander, Lynx (Links) and Mozilla.
(eval-after-load "info"
  '(progn
     (define-key Info-mode-map [(control shift insert)]
       (lambda () (interactive) (Info-copy-current-node-name 0)))
     ;; Shift-Space to scroll back (already added in bug#2145).
     ;; (define-key Info-mode-map [?\S-\ ] 'Info-scroll-down)
     ;; Mozilla-like navigation:
     (define-key Info-mode-map [(meta right)] 'my-Info-forward)
     (define-key Info-mode-map [(meta left)]  'Info-last)
     ;; Lynx-like navigation:
     (define-key Info-mode-map [(meta up)]
       (lambda ()
         (interactive)
         (my-prev-link-or-scroll-page-backward
          (save-excursion
            (ignore-errors
              (Info-prev-reference))
            (point)))))
     (define-key Info-mode-map [(meta down)]
       (lambda ()
         (interactive)
         (my-next-link-or-scroll-page-forward
          (save-excursion
            (ignore-errors
              (Info-next-reference))
            (point)))))
     ;; more/less scrolling style
     (define-key Info-mode-map [return]
       (lambda ()
         (interactive)
         (if nil;;TODO: add predicate function to info.el to check (point) for Info refs
             (my-Info-forward)
           ;; (View-scroll-line-forward)
           (progn (scroll-up 1) (move-to-window-line -1) (beginning-of-line)))))
     ;; ThinkPad additional keys, try to use them
     (when (equal (upcase (system-name)) "THINKPAD")
       (define-key Info-mode-map [osfPrior] 'Info-last)
       (define-key Info-mode-map [osfNext] 'Info-follow-nearest-node))))

(defun my-Info-forward (&optional fork)
  "Follow the nearest node, or to go history forward, if point is not on ref."
  (interactive "P")
  (condition-case error
      (Info-follow-nearest-node fork)
    (error
     (if (equal "Point neither on reference nor in menu item description"
                (cadr error))
         (Info-history-forward)
       (message "%s" (cadr error))))))


;;; man

(eval-after-load "man"
  '(progn
     ;; Don't use `man-mode-syntax-table' that sets word syntax to `.', `_', `:'.
     (add-hook 'Man-mode-hook
               (lambda ()
                 (set-syntax-table text-mode-syntax-table)))
     ;; Mozilla-like navigation:
     (define-key Man-mode-map [(meta right)] 'push-button) ;; 'man-follow
     ;; No need to kill Man buffer because it is not saved to desktop.
     (define-key Man-mode-map [(meta left)]  'quit-window)
     ;; Lynx-like navigation:
     (define-key Man-mode-map [(meta up)]
       (lambda ()
         (interactive)
         (my-prev-link-or-scroll-page-backward
          (save-excursion
            (ignore-errors (Man-previous-section 1))
            (point)))))
     (define-key Man-mode-map [(meta down)]
       (lambda ()
         (interactive)
         (my-next-link-or-scroll-page-forward
          (save-excursion
            (ignore-errors (Man-next-section 1))
            (point)))))
     (define-key Man-mode-map [f2] 'toggle-truncate-lines)
     ;; (define-key view-mode-map [tab] 'other-window) ; used for next-ref
     ;; more/less scrolling style
     (define-key Man-mode-map [return] 'View-scroll-line-forward)))


;;; dictionary

(defvar my-dict-history nil
  "History list for previous word definitions.")

(defun my-dict-search-word (word &optional output-buffer)
  "Search the word under point (by default) or entered from minibuffer,
if prefix argument is not null. Search is preformed using
external program `dict2'. The output appears in the buffer `*Dictionary*'.
If the output is one line, it is displayed in the echo area.
If OUTPUT-BUFFER is not nil, or prefix argument is not nil or 0,
then output is inserted in the current buffer."
  (interactive
   (let* ((default (if (and transient-mark-mode mark-active)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (current-word)))
          (value (if t;; (not (null current-prefix-arg))
                     (read-from-minibuffer
                      "Search word: " default nil nil 'my-dict-history)
                   default)))
     (list (if (equal value "") default value)
           (if (not (equal current-prefix-arg 0)) current-prefix-arg))))
  (let* ((new-buffer-name (or output-buffer "*Dictionary*")))
    ;; (concat "*Dictionary " word "*")
    (shell-command (concat "dict2 \"" word "\"") new-buffer-name)
    (if (member new-buffer-name (mapcar (function buffer-name) (buffer-list)))
        (with-current-buffer new-buffer-name
          ;; (my-windows-balance)
          (goto-char (point-min))
          (while (re-search-forward "{+\\([^}]+\\)}+" nil t)
            (let* ((link-text (match-string 1))
                   (link-value link-text))
              (replace-match "")        ; create widget in place of text
              (while (string-match "\n\\s-*" link-value) ; multi-line links
                (setq link-value (replace-match " " t t link-value)))
              (widget-create 'link
                             :format (concat "%[" link-text "%]")
                             :button-face 'info-xref
                             :notify (lambda (widget &rest ignore)
                                       (push (widget-value widget) my-dict-history)
                                       (my-dict-search-word (widget-value widget)))
                             :button-prefix ""
                             :button-suffix ""
                             link-value)))
          (goto-char (point-min))
          (setq buffer-read-only nil)
          ;; (toggle-read-only 1) ;; don't use view mode, but instead use its keymap
          ;; Is it right? (another solution is in help-mode-hook)
          (select-window (get-buffer-window new-buffer-name))
          (setq view-return-to-alist
                (list (cons (selected-window)
                            (cons (next-window (selected-window)) t))))
          ;; Make major or minor mode for *Dictionary <word>* buffers
          ;; TODO: use (local-set-key) instead
          ;; (use-local-map widget-keymap)
          (use-local-map (copy-keymap view-mode-map))
          (set-keymap-parent (current-local-map) widget-keymap)
          ;; Mozilla-like navigation:
          (define-key (current-local-map) [(meta right)] 'widget-button-press)
          ;; Lynx-like navigation:
          (define-key (current-local-map) [(meta left)]
            (lambda () (interactive)
               (pop my-dict-history)
               (my-dict-search-word (car my-dict-history))))
          (define-key (current-local-map) [(meta up)]
            (lambda ()
               (interactive)
               (my-prev-link-or-scroll-page-backward
                (save-excursion
                  (ignore-errors
                    (widget-backward 1))
                  (point)))))
          (define-key (current-local-map) [(meta down)]
            (lambda ()
               (interactive)
               (my-next-link-or-scroll-page-forward
                (save-excursion
                  (ignore-errors
                    (widget-forward 1))
                  (point)))))
          (define-key (current-local-map) "q"
            ;; Works only in view-mode
            (lambda () (interactive) (view-mode) (View-quit)))
          )
      (delete-other-windows))))
;; (push "*Dictionary*" pop-up-frames)


;;; dictem

(when (require 'dictem nil t)
  (setq dictem-server "dict.org")
  (setq dictem-server "mova.org")
  (setq dictem-server "localhost")
  (setq dictem-port   "2628")
  (dictem-initialize)
  ;; (global-set-key "\C-cs" 'dictem-run-search)
  ;; (global-set-key "\C-cm" 'dictem-run-match)
  (define-key my-map "dm" 'my-dictem-run-search)
  ;; (global-set-key "\C-cd" 'dictem-run-define)
  ;; (global-set-key "\C-c\M-r" 'dictem-run-show-server)
  ;; (global-set-key "\C-c\M-i" 'dictem-run-show-info)
  ;; (global-set-key "\C-c\M-b" 'dictem-run-show-databases)

  (defun my-dictem-run-search (query)
    (interactive
     (list (dictem-read-query
            (let ((word (assq 'word (plist-get (text-properties-at (point))
                                               'link-data))))
              (or (cdr word) (thing-at-point 'word))))))
    (switch-to-buffer (get-buffer-create dictem-buffer-name))
    (dictem-mode)
    (dictem-run 'dictem-base-search "*" query ".")
    (goto-char (point-max))
    (recenter-top-bottom -1))

  (defun my-dictem-run-search-from-clipboard-or-word-at-point ()
    (interactive)
    (my-dictem-run-search
     (or (and kill-ring
              (string-match-p "\\`[A-Za-z]+\\'" (current-kill 0))
              (current-kill 0))
         (thing-at-point 'word)
         (dictem-read-query)))
    (goto-char (point-max)))

  (add-hook 'dictem-postprocess-match-hook
            'dictem-postprocess-match)
  (add-hook 'dictem-postprocess-definition-hook
            'dictem-postprocess-definition-separator)
  (add-hook 'dictem-postprocess-definition-hook
            'dictem-postprocess-definition-hyperlinks)
  (add-hook 'dictem-postprocess-show-info-hook
            'dictem-postprocess-definition-hyperlinks)

  (add-hook 'dictem-mode-hook
            (lambda ()
              (define-key dictem-mode-map [tab] 'dictem-next-link)
              (define-key dictem-mode-map [(backtab)] 'dictem-previous-link)
              (set (make-local-variable 'outline-regexp) "From")
              (outline-minor-mode 1)
              ;; (define-key dictem-mode-map [(meta left)]  'my-dictem-prev-word?)
              (define-key dictem-mode-map [(meta right)] 'my-dictem-run-search)))

  (setq dictem-use-existing-buffer t)
  (setq dictem-empty-initial-input t))

;; TODO: get default words from selected region


;;; calendar

;; (define-key my-map "cc" 'calendar)
;; (define-key my-map "cd" 'diary)


;;; diary

;; Problem: 'appt-activate commented out because it causes diary to be called twice
;; (the second call overrides the correct date entries with the current date):
;; (add-hook 'diary-hook 'appt-activate)
;; (add-hook 'diary-display-hook 'fancy-diary-display)

;; My diary entries are only in ISO date format, so override all other formats
;; If other date formats exist, then use next (add ISO to existing):
;; (setq diary-date-forms (cons '(year "-" month "-" day "[^0-9]") diary-date-forms))

;; (diary)
;; (calendar)


;;; w3

(setq w3-default-stylesheet "~/.default.css")
(eval-after-load "w3"
  '(progn
     ;; Mozilla-like navigation:
     (define-key w3-mode-map [(meta right)] 'widget-button-press)
     (define-key w3-mode-map [(meta left)] 'w3-history-backward)
     (define-key w3-mode-map [(meta down)] 'w3-widget-forward)
     (define-key w3-mode-map [(meta up)] 'w3-widget-backward)
     ;; more/less scrolling style
     (define-key w3-mode-map [return] 'View-scroll-line-forward)
     ;; (setq w3-use-terminal-glyphs nil)
     ;; (fset 'w3-fetch-orig (symbol-function 'w3-fetch))
     ;; (defun w3-fetch (&optional url target)
     ;;   (interactive (list (w3-read-url-with-default)))
     ;;   (if (eq major-mode 'gnus-article-mode)
     ;;       (browse-url url)
     ;;     (w3-fetch-orig url target)))
     ))


;;; w3m

(eval-after-load "w3m"
  '(progn
     ;; (setq browse-url-browser-function 'w3m-browse-url)
     ;; Mozilla-like navigation:
     (define-key w3m-mode-map [(meta right)] 'w3m-view-this-url)
     (define-key w3m-mode-map [(meta left)]  'w3m-view-previous-page)
     (define-key w3m-mode-map [(meta shift right)] 'w3m-view-this-url-new-session)
     (define-key w3m-mode-map [(control return)] 'w3m-view-this-url-new-session)
     ;; Lynx-like navigation:
     (define-key w3m-mode-map [(meta up)]
       (lambda ()
         (interactive)
         (my-prev-link-or-scroll-page-backward
          (save-excursion
            (ignore-errors (w3m-previous-anchor))
            (point)))))
     (define-key w3m-mode-map [(meta down)]
       (lambda ()
         (interactive)
         (my-next-link-or-scroll-page-forward
          (save-excursion
            (ignore-errors (w3m-next-anchor))
            (point)))))
     ;; more/less scrolling style if point is not on URL
     (define-key w3m-mode-map [return]
       (lambda ()
         (interactive)
         (if (or (not (w3m-anchor))
                 (eq (point) (save-excursion (move-to-window-line -1) (point))))
             (View-scroll-line-forward)
           (w3m-view-this-url))))
     ;; Tabs navigation (useful when tabs are visible):
     ;; to avoid conflict with (control tab) calling ee-buffers,
     ;; w3m could be used in a separate frame
     (define-key w3m-mode-map [(control tab)] 'w3m-next-buffer)
     (define-key w3m-mode-map [(control shift tab)] 'w3m-previous-buffer)
     (define-key w3m-mode-map [(control shift iso-lefttab)] 'w3m-previous-buffer)
     ;; Add emacs version and gnu/linux version.
     (setq w3m-user-agent (concat
                           "Emacs-w3m/" emacs-w3m-version
                           " " w3m-version
                           " Emacs/" emacs-version
                           (if (string-match "[Ll]inux" system-configuration)
                               (concat " (" system-configuration ")")
                             "")))
     ;; (my-faces-fix)
     ))

(add-hook
 'w3m-display-hook
 (lambda (url)
   ;; But better idea is to display these names only in the buffer list
   (rename-buffer
    (generate-new-buffer-name
     (concat "*w3m*<"
             w3m-current-title
             ;; (substring w3m-current-title 0 (min (length w3m-current-title) 11))
             ">")))))


;;; firefox

;; See http://www.forensicswiki.org/wiki/Mozilla_Firefox_3_History_File_Format
(defun firefox-history-sql (profile-dir)
  "Query FireFox browse history."
  (interactive
   (let ((profiles (directory-files "~/.mozilla/firefox/" t "\\.default\\'")))
     (if (= (length profiles) 1)
         (list (car profiles))
       (list (read-directory-name "Select profile directory name: "
                                  "~/.mozilla/firefox/" nil t nil)))))
  (let ((sql-sqlite-program "sqlite3")
        (sql-database (expand-file-name "places.sqlite" profile-dir)))
    (sql-sqlite)))


;;; gnus

(defun my-gnus ()
  "Start a new Gnus, or locate the existing buffer *Group*."
  (interactive)
  (if (buffer-live-p    (get-buffer "*Group*"))
      (switch-to-buffer (get-buffer "*Group*"))
    (gnus)))

(define-key my-map "g" 'my-gnus)
;; (define-key my-map "g" (lambda () (gnus 3)))
;; (define-key my-map "G" 'gnus-no-server)
(define-key my-map "G" (lambda () (interactive)
                          (gnus-no-server)
                          ;; BUG? mail groups don't come automatically
                          (gnus-group-jump-to-group "nnml:mail.inbox")))
(define-key my-map "Q" 'smtpmail-send-queued-mail)

(eval-after-load "gnus"
  '(progn
     (setq gnus-group-line-format
           "%M%m%S%p%P%4y:%B%(%-43,43g%) %3T %5t %2I %o\n")
     ;; (setq gnus-group-line-format
     ;;       "%M%m%S%p%P%4y:%B%(%-30,30g%) %3T %5t %2I %o %s\n")
     ;; (setq gnus-group-line-format
     ;;       "%M%S%5y: %(%-30,30g%) %9,9~(cut 4)d %5t %2I %2T %o %n %s\n")
     (define-key gnus-group-mode-map [tab] 'gnus-group-next-unread-group)
     (define-key gnus-group-mode-map [(shift iso-lefttab)] 'gnus-group-prev-unread-group)
     (setq gnus-message-archive-group
           '((lambda (group)
               (if (or (message-news-p)
                       (not group)
                       (and (stringp group)
                            (or (eq (length group) 0)
                                (string-match "^nnml:list\." gnus-newsgroup-name)
                                (not (string-match "^nnml:" gnus-newsgroup-name)))))
                   "nnml:archive"
                 group))))
     ))

(eval-after-load "gnus-sum"
  '(progn
     ;; (setq gnus-summary-line-format ; TEST
     ;;       "%U%R%z%I%(%[%1L: %1,1~(cut 1)L %4,4~(cut 4)o: %-20,20f%]%) %s\n")
     ;; (setq gnus-summary-line-format
     ;;       "%U%R%z%I%(%[%4L: %4,4~(cut 4)o: %-20,20n%]%) %s\n")
     (setq gnus-summary-line-format
           "%U%R%z%I%(%[%4L: %4,4~(cut 4)o: %-20,20f%]%) %s\n")
     ;; Shift-Space to scroll back (already added in bug#2145).
     ;; (define-key gnus-summary-mode-map [?\S-\ ] 'gnus-summary-prev-page)
     (define-key gnus-summary-mode-map [tab] 'gnus-summary-next-unread-article)
     (define-key gnus-summary-mode-map [(shift iso-lefttab)]
                                       'gnus-summary-prev-unread-article)
     (define-key gnus-summary-mode-map [(meta right)] 'gnus-summary-show-thread)
     (define-key gnus-summary-mode-map [(meta left)] 'gnus-summary-hide-thread)
     (define-key gnus-summary-mode-map [delete] 'gnus-summary-delete-article)
     ;; (define-key gnus-summary-mode-map [f6] 'gnus-summary-move-article)
     ;; (define-key gnus-summary-mode-map "!" 'my-gnus-summary-tick-article-forward)
     (define-key gnus-summary-mode-map "r" 'gnus-summary-reply-with-original)
     (define-key gnus-summary-mode-map "R" 'gnus-summary-wide-reply-with-original)

     ;; Use standard keybinding instead of stupid `gnus-summary-show-thread'
     (define-key gnus-summary-mode-map "\M-\C-s" 'isearch-forward-regexp)

;; Commented out to test multi-article Isearch
;; ;; "\M-s"
;; (define-key gnus-summary-mode-map   "s" 'gnus-summary-search-article-forward)
;; ;; "\M-S"
;; (define-key gnus-summary-mode-map "M-s" 'gnus-summary-repeat-search-article-forward)

     ;; (define-key gnus-summary-mode-map "\C-l" 'my-recenter)

     ;; Disable dangerous key bindings
     (define-key gnus-summary-mode-map [(meta ?g)] nil)
     (define-key gnus-summary-mode-map "x" nil)
     (define-key gnus-summary-mode-map "\C-x\C-s" nil)
     (setq gnus-thread-sort-functions
           '(gnus-thread-sort-by-number)
           ;;'(gnus-thread-sort-by-total-score (not gnus-thread-sort-by-number))
           )
     (add-hook 'gnus-group-mode-hook
      (lambda ()
        ;; I don't need line and column numbers in the group buffer
        (set (make-variable-buffer-local 'line-number-mode) nil)
        (set (make-variable-buffer-local 'column-number-mode) nil)))
     (add-hook 'gnus-summary-mode-hook
      (lambda ()
        ;; I don't need line and column numbers in the summary buffer
        (set (make-variable-buffer-local 'line-number-mode) nil)
        (set (make-variable-buffer-local 'column-number-mode) nil)))
     ;; Zerba stripes for the summary buffer
     ;; (from http://www.emacswiki.org/cgi-bin/wiki/StripesMode)
     ;; (add-hook 'gnus-summary-mode-hook 'turn-on-stripes-mode)
     ))

(eval-after-load "gnus-art"
  '(progn
     ;; I'm curious about what news readers do people use (Gnus or not ;)
     (setq gnus-visible-headers
           (append
            (if (listp gnus-visible-headers)
                gnus-visible-headers
              (list gnus-visible-headers))
            (list (concat "^User-Agent:\\|^X-User-Agent:\\|"
                          "^X-Mailer:\\|^X-Newsreader:\\|^X-FTN-Tearline:\\|"
                          "^X-Http-User-Agent:"))))
     ;; Highlight the beginning of the bug report.
     (setq gnus-emphasis-alist
           (cons
            '("\\(the precise symptoms of the bug\\)"
              0 1 gnus-emphasis-underline)
            gnus-emphasis-alist))
     (add-hook 'gnus-article-mode-hook
               (lambda ()
                 (visual-line-mode)
                 (setq bug-reference-url-format "http://debbugs.gnu.org/%s")
                 (bug-reference-mode 1))
               t)
     ;; Shift-Space to scroll back (already added in bug#2145).
     ;; (define-key gnus-article-mode-map [?\S-\ ] 'gnus-article-goto-prev-page)
     (define-key gnus-article-mode-map "R" 'gnus-summary-wide-reply-with-original)
     ;; RET scrolls the article one line at a time.
     (define-key gnus-article-mode-map [return]
       (lambda ()
         (interactive)
         (if (or (not (get-char-property (point) 'button))
                 ;; or point is on the bottom of the window while scrolling
                 (eq (point) (save-excursion (move-to-window-line -1) (point))))
             (progn (scroll-up 1) (move-to-window-line -1) (beginning-of-line))
           (widget-button-press (point)))))
     ;; Disable dangeruos key bindings
     (define-key gnus-article-mode-map [(meta ?g)] nil)))

;; TODO: move this command to gnus/gnus-ml.el and bind to `C-c C-n w'
(defun my-gnus-copy-link-gnu-lists (&optional arg)
  "Put the link to the article in the GNU archives into the kill ring.
Example:
\(browse-url (concat \"http://lists.gnu.org/archive/cgi-bin/namazu.cgi?idxname=\"
\"emacs-devel&query=\" (url-hexify-string (concat \"+message-id:\"
\"12345678.fsf@gnu.org\"))))"
  (interactive "P")
  (unless (eq major-mode 'gnus-summary-mode)
    (error "Not in a gnus group buffer"))
  (let ((list-archive
         (with-current-buffer gnus-original-article-buffer
           (gnus-fetch-field "list-archive")))
        (message-id
         (with-current-buffer gnus-original-article-buffer
           (replace-regexp-in-string
            "^<\\(.*\\)>$" "\\1" (gnus-fetch-field "message-id"))))
        (text-template "\
\(browse-url (concat \"http://lists.gnu.org/archive/cgi-bin/namazu.cgi?idxname=\"\n\
\"%s&query=\" (url-hexify-string (concat \"+message-id:\"\n\
\"%s\"))))")
        (text))
    (if (string-match "<http://lists\\.gnu\\.org/[^>]*/\\([^/>]+\\)>" list-archive)
        (setq text (format text-template (match-string 1 list-archive) message-id))
      (error "Not in a GNU mailing list"))
    (kill-new text)
    (message "%s" text)))

;; Actually, the above is not needed due to the supported url scheme like
;; http://thread.gmane.org/<Message-ID>
(defun my-gnus-copy-link-gmane (&optional arg)
  "Put the link to the article on gmane.org into the kill ring.
Example:
\(browse-url (concat \"http://thread.gmane.org/\"
 (url-hexify-string \"12345678.fsf@gnu.org\")))"
  (interactive "P")
  (unless (eq major-mode 'gnus-summary-mode)
    (error "Not in a gnus group buffer"))
  (let ((message-id
         (with-current-buffer gnus-original-article-buffer
           (replace-regexp-in-string
            "^<\\(.*\\)>$" "\\1" (gnus-fetch-field "message-id"))))
        (text-template "\(browse-url (concat \"http://thread.gmane.org/\"\
 (url-hexify-string \"%s\")))")
        (text))
    (setq text (format text-template message-id))
    (kill-new text)
    (message "%s" text)))

;; Improve `gnus-summary-tick-article-forward' to allow specifying tick character.
(defun my-gnus-summary-tick-article-forward (n &optional mark)
  "Tick N articles forwards.
If N is negative, tick backwards instead.
The difference between N and the number of articles ticked is returned."
  (interactive (list
                (prefix-numeric-value current-prefix-arg)
                (let ((mark (read-char "Tick char: " t)))
;;                   (if (memq mark (string-to-list " ?rREK$FXYCAFN*S.OQGM-+%="))
;;                       (error "Reserved mark"))
                  mark)))
  (gnus-summary-mark-forward n (or mark gnus-ticked-mark)))


;;; message

(require 'message) ;;(load-library "message")

;; (add-hook 'message-send-hook 'ispell-message)
;; Bilingual spell-checking of the mail message.
(add-hook 'message-send-hook
          (lambda ()
            (ispell-change-dictionary "american")
            (ispell-message)
            ;; (ispell-change-dictionary "russian")
            ;; (ispell-message)
            ))

(add-hook 'message-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            ;; Prevent premature sending when `C-c C-s'
            ;; is typed instead of `C-x C-s'
            (define-key message-mode-map "\C-c\C-s" nil)))
;; TODO: try to use (message-tab) in message mode


;;; mime

(when (require 'mm nil t)
  (mm-parse-mailcaps)
  (mm-parse-mimetypes))


;;; bbdb

;; (when (require 'bbdb nil t)
;;   (bbdb-initialize 'gnus 'message 'w3)
;;   (add-hook 'bbdb-mode-hook 'view-mode))


;;; ange-ftp

(eval-after-load "ange-ftp"
  '(progn
     ;; Ignore more messages in the FTP client with Finnish messages
     (setq ange-ftp-skip-msgs (concat ange-ftp-skip-msgs "\\|^Yhdistetty"))))


;;; gnuserv

(require 'server)
(unless (server-running-p)
   (server-start))

;; NOTE: (server-start) conflicts with (customize)!
;; (when (require 'gnuserv nil t)
;;  (setq gnuserv-frame (selected-frame))
;;  (gnuserv-start))

;; TEST: comment in again: [2005-05-09]!
;; (and (fboundp 'gnuserv-start) (gnuserv-start))


;;; term

(add-hook 'term-mode-hook
          (lambda ()
            ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
            ;; (set (make-local-variable 'mouse-yank-at-point) t)
            ;; (make-local-variable 'transient-mark-mode)
            (auto-fill-mode -1)
            (setq tab-width 8)))


;;; xterm

(when (featurep 'xterm)
  (xterm-mouse-mode))


;;; time-stamp

;; MOVED TO Local Variables:
;; (add-hook 'before-save-hook 'time-stamp)


;;; copyright

;; (add-hook 'before-save-hook 'copyright-update)


;;; cvs

(add-hook 'cvs-mode-hook
          (lambda ()
            (define-key cvs-mode-map [f3] 'cvs-mode-view-file)))


;;; add-log

;; NO NEED because ADDED (mode . bug-reference) to emacs/.dir-locals.el
;; (add-hook 'change-log-mode-hook 'bug-reference-mode)


;;; messages

(add-hook 'messages-buffer-mode-hook
          (lambda ()
            (setq buffer-read-only nil)
            (fundamental-mode)))


;;; css

;; If there is no css-mode available already, use c-mode for .css files.
(unless (rassoc 'css-mode auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . c-mode)))

(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
;; 'scss-mode' was added to Emacs 25.1
;; (add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . javascript-mode))
;; '.jsx' was added to auto-mode-alist in the new version of Emacs
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . conf-mode))


;;; wget

(require 'wget nil t)

(setq wget-download-directory ".")
(setq wget-download-log-file "DOWNLOAD")
(setq wget-download-log-format "%T %U\n")
(setq wget-download-log-time-format "%Y-%m-%d")

;; Set the default either to the URL in the clipboard or URL at point.
(defun my-wget (uri &optional arg)
  (interactive
   (list
    (read-string "URI: "
                 (if (and kill-ring
                          (string-match "^[hf]t?tps?:" (current-kill 0)))
                     (current-kill 0)
                   (require 'thingatpt)
                   (thing-at-point-url-at-point)))
    (when current-prefix-arg t)))
  (wget uri arg))

(define-key my-map "wg" 'my-wget)

(setenv "WGETRC" (expand-file-name "~/.wgetrc-emacs"))
;; where the file ~/.wgetrc-emacs contains just one line:
;; progress=dot


;;; dropbox

;; Don't make auto-saves and backups in the Dropbox directory:
(add-to-list 'auto-save-file-name-transforms '("\\`.*/Dropbox/.*" "/tmp/" t))
(add-to-list 'backup-directory-alist '("\\`.*/Dropbox/.*" . "/tmp/"))


;;; scroll-lock

;; Use ScrollLock key to activate scroll-lock-mode
;; (from http://lists.gnu.org/archive/html/emacs-devel/2005-06/msg01274.html)
(let ((key (if (eq window-system 'w32) "<scroll>" "<Scroll_Lock>")))
  (unless (lookup-key (current-global-map) (read-kbd-macro key))
    (define-key (current-global-map) (read-kbd-macro key) 'scroll-lock-mode)))

;; [2010-08-05] After recent changes in scrolling, this setting causes
;; very slow regexp search "C-M-s ^raven" in /usr/share/dictd/mueller7accent.dict.dz
;; (setq scroll-conservatively 1000000)


;;; fun

;; (fset 'undefined
;;       (lambda ()
;;         (interactive)
;;         (message "%s" (cookie ".../talker.msg" nil nil))))
;; The previous line redefines standard function 'undefined', which
;; is invoked, when key binding is suppressed in read-only buffers.
;; So unexpected funny messages can greatly confuse :-)
;; For example, when trying to visit a file by pressing
;; a wrong key sequence, you can get the following message:
;; ...File not found. Should I fake it? (Y/N)
;; It takes some time to realize that this was a joke.
;; There are many other confusing messages, like:
;;   Internal stack overflow, System halted.
;;   Enter any 11-digit prime number to continue.
;;   etc...
;; There is a Debian package `fortunes-mod-bofh-excuses' that
;; contains a collection of such excuses.  Some examples are:
;; 'Electromagnetic energy loss', 'disks spinning backwards -
;; toggle the hemisphere jumper.'
;; or (fset 'undefined 'yow)

(eval-after-load "tetris"
  '(progn
     (define-key tetris-mode-map [up]   'tetris-rotate-next)
     (define-key tetris-mode-map [down] 'tetris-move-bottom)))

(defvar my-digital-clock-timer nil)
(defvar my-digital-clock-frame nil)

(defun my-digital-clock (&optional arg)
  "Show digital clock in the separate Emacs frame.
Show digital clock in the same Emacs frame if called with C-0.
Cancel the clock if called with C-u."
  (interactive "P")
  (and (boundp 'my-digital-clock-timer) (timerp my-digital-clock-timer)
       (cancel-timer my-digital-clock-timer))
  (and (boundp 'my-digital-clock-frame) (framep my-digital-clock-frame)
       (delete-frame my-digital-clock-frame))
  (if (or (not arg) (numberp arg))
      (setq my-digital-clock-timer
            (run-at-time
             t 1
             (lambda ()
               (message "%s"
                        ;; (round (float-time)) ; e.g. 1234567890
                        (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
                        )))))
  (or arg
      (setq my-digital-clock-frame
            (make-frame
             `((top . 478) (left . 80) (width . 24) (height . 1)
               (name . "TIME")
               (minibuffer . only)
               (buffer-predicate . nil)
               (user-position . t)
               (vertical-scroll-bars . nil)
               (scrollbar-width . 0)
               (menu-bar-lines . 0)
               (foreground-color . "green")
               (background-color . "black")
               ,(cond
                 ((eq window-system 'x)
                  '(font . "-misc-fixed-bold-r-normal--18-*-*-*-*-*-iso8859-1")))
               (cursor-color . "gray2")
               (cursor-type . bar)
               (auto-lower . nil)
               (auto-raise . t)
               (border-width . 0)
               (internal-border-width . 0))))))


;;; save-place

;; TRY:
(setq save-place-skip-check-regexp
      (concat
       save-place-skip-check-regexp
       "\\|\\.\\(7z\\|apk\\|arc\\|jar\\|lzh\\|zip\\|zoo\\)$"
       "\\|\\.t\\(ar\\.\\)?gz$"
       "\\|\\.t\\(ar\\.bz2\\|bz\\)$"))


;;; desktop

(when (boundp 'desktop-modes-not-to-save)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'vc-dir-mode))

;; Add more globals to save between sessions.
(if (boundp 'desktop-globals-to-save)
    (setq desktop-globals-to-save
          (delete-dups
           (append
            '(;; buffer-name-history
              coding-system-history
              ;; command-history
              compile-history
              extended-command-history
              find-tag-history
              file-name-history
              find-args-history
              grep-history
              grep-find-history
              ;; Info-search-history
              ;; locate-history-list
              my-dict-history
              minibuffer-history
              minibuffer-history-search-history
              query-replace-history
              query-replace-defaults
              read-expression-history
              regexp-history
              dired-shell-command-history ;; TODO: merge with shell-command-history
              shell-command-history
              search-ring
              regexp-search-ring)
            (delq 'register-alist desktop-globals-to-save)))))

;; Prepare Emacs desktop after loading Emacs.
(add-hook 'after-init-hook
          (lambda ()
            ;; Show home directory on the left panel,
            ;; and the last visited file on the right.
            ;; (split-window-horizontally)
            ;; (dired "~")
            )
          ;; Please note that 3-rd argument of this `add-hook' should be `t'
          ;; to append the call of the `dired' after other hooked functions,
          ;; most importantly after `desktop-read'.
          t)

;; There are different ways to maximize initial frame after loading .emacs
;; "emacs -mm" that sets (setq initial-frame-alist '((fullscreen . maximized)))
;; or (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; or (toggle-frame-maximized) or (set-frame-size (selected-frame) 200 80)
;; But below is the only way that works reliably on GNU/Linux:
(add-hook 'after-init-hook
          (lambda ()
            ;; (set-frame-size (selected-frame) 210 80)
            ;; This works only in KDE.
            (run-at-time
             "1 second" nil
             'shell-command-to-string   ; to not overwrite the echo area
             "wmctrl -r :ACTIVE: -b add,maximized_vert,maximized_horz")
            ;; FIX a recent bug that breaks frame dimensions after desktop frame restore:
            ;; I get a maximized frame visually, but internally with unmaximized dimenstions,
            ;; i.e. mouse avoidance moves the mouse pointer to the middle of the frame
            ;; instead to the edges, etc.
            ;; (toggle-frame-maximized)
            ;; (toggle-frame-maximized)
            )
          t)

;; Display the time of the Emacs initialization.
(when (fboundp 'emacs-init-time)
  (add-hook 'after-init-hook (lambda () (message "%s" (emacs-init-time))) t))


;;; Local Variables:
;;; mode: emacs-lisp
;;; outline-regexp: ";;;;* "
;;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;;; time-stamp-start: "Version: "
;;; time-stamp-format: "%:y-%02m-%02d for GNU Emacs 27.0.50 (x86_64-pc-linux-gnu)"
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 15
;;; End:

;;; .emacs ends here
