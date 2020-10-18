;;; .emacs --- old init file  -*- lexical-binding: t; -*-

;; Copyright (C) 1989-2020  Juri Linkov <juri@linkov.net>

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: dotemacs, init
;; URL: <http://www.linkov.net/emacs>
;; Version: 2020-10-18 for GNU Emacs 28.0.50 (x86_64-pc-linux-gnu)


;; This file now contains semi-obsolete settings.
;; For more up-to-date settings,
;; please see the file README.org in the same directory.


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
    (set-face-background 'region "gray90" frame))
  (when (facep 'fringe)
    (set-face-background 'fringe (face-background 'default) frame)
    (set-face-foreground 'fringe (face-foreground 'default) frame)))

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
(define-key global-map [Multi_key] [t]) ; don't beep on accidental keypress

(defvar my-sunset-timer nil)
(defvar my-sunrise-timer nil)

;; Automatically switch to dark background after sunset
;; and to light background after sunrise.
;; (Note that `calendar-latitude' and `calendar-longitude'
;;  should be set before calling the `solar-sunrise-sunset')
(defun my-colors-set (&optional frame)
  (interactive)
  (require 'solar)
  (if (and (bound-and-true-p calendar-latitude)
           (bound-and-true-p calendar-longitude)
           (bound-and-true-p calendar-time-zone))
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
;; (add-hook 'after-make-frame-functions 'my-colors-set)


;;; faces

(defvar face nil)
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
      ;; (when (numberp (face-attribute face :height frame))
      ;;   (set-face-attribute face frame :height 'unspecified))

      ;; Fonts with different width decrease the amount of characters
      ;; on the line, so remove the width property
      ;; (when (numberp (face-attribute face :width frame))
      ;;   (set-face-attribute face frame :width 'unspecified))

      ;; Fonts with different weight decrease the height and width,
      ;; of the line, so remove the weight property
      ;; (when (numberp (face-attribute face :weight frame))
      ;;   (set-face-attribute face frame :weight 'unspecified))
      ;; (unless (string-match "^mode-line" (symbol-name face))
      ;;   ;; Reset all face attributes
      ;;   (modify-face face))

      ;; New feature in Emacs 27
      ;; (set-face-attribute face frame :extend t)
      ;; (set-face-extend face t frame)
      )))

;; 1. Fix existing faces
;; (let ((face t)) (my-faces-fix))
;; (add-hook 'after-init-hook (lambda () (let (face) (my-faces-fix))) t)
;; 2. Call `my-faces-fix' every time some new face gets defined
(add-hook 'custom-define-hook 'my-faces-fix)


;;; keybindings

;; Keybindings (meta up) (meta down) are free when windmove uses `super'.

;; Actually I don't use next two keybindings, use them for something useful
;; (define-key global-map [(control meta prior)] 'scroll-right)
;; (define-key global-map [(control meta next)]  'scroll-left)

;; (define-key global-map [(control return)]
;;   (lambda () (interactive) (let ((use-hard-newlines t)) (newline))))
;; (define-key global-map [(meta return)]
;;   (lambda () (interactive) (scroll-other-window 1))) ;; [(meta down)]
;; (define-key global-map [(meta backspace)]
;;   (lambda () (interactive) (scroll-other-window -1))) ;; [(meta up)]

;; (define-key global-map [(control backspace)] 'backward-kill-word)
;; (define-key global-map [(meta backspace)] 'undo)
;; (define-key global-map [(meta backspace)] 'backward-kill-word)
;; (define-key global-map [(control backspace)] 'join-lines)

;; These following two keybindings are standard default:
;; (define-key global-map [(meta /)] 'dabbrev-expand)
;; (define-key global-map [(control meta /)] 'dabbrev-completion)
;; The following key is not available:
;; (define-key global-map [(control meta kp-divide)] 'hippie-expand)

;; BAD key: (define-key global-map "\M-n" 'clone-buffer)
(define-key global-map [(control x) (c) (b)] 'clone-buffer)

;; (define-key global-map [(control escape)]
;;   (lambda () (interactive) (buffer-menu 1))) ; not needed
;; (define-key global-map [(control escape)] 'ibuffer)
;; (define-key global-map [(shift f10)] 'buffer-menu) ; not needed

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
;; (this is like venerable PuntoSwitcher)


;;; functions

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

;; (define-key my-map "s" 'my-scroll-auto)


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


;;; window

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


;;; isearch

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
;;       (let ((my-recenter-position 0.3))
;;         (recenter '(4)))))

;; Automatically reposition every found isearch match
;; (defadvice isearch-update (before my-isearch-reposite activate)
;;   (sit-for 0)
;;   (reposition-window))

;; TODO: try to use `isearch-update-post-hook', e.g.
;; (add-hook 'isearch-update-post-hook 'recenter)
;; (add-hook 'replace-update-post-hook 'recenter)
;; (defadvice isearch-update (before my-isearch-reposite activate)
;;   (sit-for 0)
;;   ;; While browsing patches, make the next hunk posited at the window's top:
;;   (when (and (derived-mode-p 'diff-mode) isearch-regexp (equal "^revno:" isearch-string))
;;     (recenter 1)))

;; Make Isearch mode-line string shorter, just " /" instead of " Isearch"
;; (add-hook 'isearch-mode-hook
;;           (lambda () (setq isearch-mode " /") (force-mode-line-update)))


;;; minibuffer

;; THE NEXT 3 FUNCTIONS WORK WITH BIG DELAY (try to use like icomplete.el)
;; see also PC-temp-minibuffer-message, file-cache-temp-minibuffer-message,
;; calc-temp-minibuffer-message and bug report in emacs-pretest-bug
;; Subject: bad doc string for PC-temp-minibuffer-message
;; Actually this is like isearch-count
(defun minibuffer-history-position-message ()
  (if (memq this-command '(next-history-element previous-history-element
                           next-line-or-history-element previous-line-or-history-element))
      (minibuffer-message
       (propertize
        (format "%s[%s]"
                (make-string
                 1
                 ;; (- (frame-width)
                 ;;    (minibuffer-prompt-width)
                 ;;    (length (minibuffer-contents-no-properties))
                 ;;    5)
                 ?\ )
                minibuffer-history-position)
        'face 'shadow))))
;; (defadvice next-history-element (after history-position-message activate)
;;   (minibuffer-history-position-message))
;; (defadvice previous-history-element (after history-position-message activate)
;;   (minibuffer-history-position-message))
;; (defadvice next-line-or-history-element (after history-position-message activate)
;;   (minibuffer-history-position-message))
;; (defadvice previous-line-or-history-element (after history-position-message activate)
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
;; (defadvice next-line-or-history-element (after my-next-history-element activate)
;;   (minibuffer-history-position-update))
;; (defadvice previous-line-or-history-element (after my-previous-history-element activate)
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

(defvar my-work-log-file)
(defun my-work-log-add (&optional _arg)
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
         ;; ‘ois’ is indexes of overlays sorted by start positions
         (ois (mapcar (lambda (o) (setq oi (1+ oi)) (cons o oi))
                      overlays))
         ;; ‘poss’ is list of positions of boundaries of text properties
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
                ;; Sort positions in the descending order
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
      (mapc (lambda (pos)
              (goto-char (car pos))
              ;; Insert markup from buffer end to the beginning
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


;;; wincows

;; Standalone wincows.el is replaced by `tab-switcher' above now.
(when nil ;; (require 'wincows nil t)
  (define-key global-map [(meta  ?\xa7)] 'wincows)
  ;; (define-key global-map [(meta ?\x8a7)] 'wincows)
  (define-key global-map [(meta     ?`)] 'wincows)
  (define-key global-map [(super    ?`)] 'wincows)
  (with-eval-after-load 'wincows
    (define-key wincows-mode-map [(meta  ?\xa7)] 'wincows-select)
    ;; (define-key wincows-mode-map [(meta ?\x8a7)] 'wincows-select)
    (define-key wincows-mode-map [(meta     ?`)] 'wincows-select)
    (define-key wincows-mode-map [(super    ?`)] 'wincows-select)
    (define-key wincows-mode-map [( ?\xa7)] 'wincows-next-line)
    ;; (define-key wincows-mode-map [(?\x8a7)] 'wincows-next-line)
    (define-key wincows-mode-map [(    ?`)] 'wincows-next-line)
    (define-key wincows-mode-map [( ?\xbd)] 'wincows-prev-line)
    ;; (define-key wincows-mode-map [(?\x8bd)] 'wincows-prev-line)
    (define-key wincows-mode-map [(    ?~)] 'wincows-prev-line)))


;;; lisp

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

;; Lisp mode
(tempo-define-template "lisp-print-map" '("(map (lambda (x) ) " p ")"))
(define-key lisp-mode-map "\C-zim" 'tempo-template-lisp-print-map)

;; Emacs Lisp mode
;; use C-M-i instead of
;; (define-key emacs-lisp-mode-map [(control meta tab)] 'lisp-complete-symbol)
;; use C-M-i instead of
;; (define-key emacs-lisp-mode-map "\C-ze\t" 'lisp-complete-symbol)

;; Lisp Interaction mode
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


;;; clojure

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local inferior-lisp-program
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
  ;; (define-clojure-indent
  ;;   (defroutes 'defun)
  ;;   (GET 2)
  ;;   (POST 2)
  ;;   (PUT 2)
  ;;   (DELETE 2)
  ;;   (HEAD 2)
  ;;   (ANY 2)
  ;;   (context 2))
  )

;; qv http://stackoverflow.com/questions/3528705/clojure-inferior-lisp-window-in-emacs-being-displayed-over-code-when-doing-c-c-c
;; (setq same-window-buffer-names (delete "*inferior-lisp*" same-window-buffer-names))


;;; snd

(autoload 'sndtr-mode "sndtr" "Major mode for editing Snd transcripts." t)
;; transcripts sndtr files
(add-to-list 'auto-mode-alist '("\\.trs\\'" . sndtr-mode))
;; marks snd files
(add-to-list 'auto-mode-alist '("\\.marks\\'" . scheme-mode))

(defun run-snd ()
  (interactive)
  (run-scheme "snd -notebook" "snd"))

(defvar inferior-lisp-prompt)
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

(with-eval-after-load 'autoinsert
  (add-to-list
   'auto-insert-alist
   '(perl-mode
     nil
     "#!/usr/bin/perl -w" \n
     "# -*- Perl -*-" \n
     ;; "# $Id$" \n
     ;; "# $RCSfile$$Revision$$Date$" \n
     "# $Revision$" \n
     \n
     "while (<>) {" \n
     > "chomp;" \n
     > _ \n
     > "print \"$_\\n\";\n"
     "}\n")))

(tempo-define-template "perl-skeleton"
 '("#!/usr/bin/perl -w\n# -*- Perl -*-\n# $Revision$\n\nwhile (<>) {\n  chomp;\n  "
   p "\n}\n"))
(tempo-define-template "perl-s-skeleton" '("s/" p "//;"))
(tempo-define-template "perl-print-skeleton" '("print \"$_" p "\\n\";"))
(tempo-define-template "perl-while-skeleton" '("while (<>) {\n  chomp;\n  " p "\n}\n"))

(with-eval-after-load 'perl-mode
  ;; (define-auto-insert 'perl-mode (lambda () (tempo-template-perl-skeleton)))
  (define-key perl-mode-map "\C-ziw" 'tempo-template-perl-while-skeleton)
  (define-key perl-mode-map "\C-zip" 'tempo-template-perl-print-skeleton)
  (define-key perl-mode-map "\C-zis" 'tempo-template-perl-s-skeleton))

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
(with-eval-after-load 'man
  (add-hook
   'Man-cooked-hook
   (lambda ()
     ;; TODO: add to perl-mode.el? and cperl-mode.el?
     ;; BAD: it breaks links followed with a dot!
     (if (string-match "\\`\\([0-9]+ *\\)?perl" Man-arguments)
         (Man-highlight-references0
          "DESCRIPTION"
          "\\(perl\\(?:[a-z0-9]+[a-z]\\|[a-z][a-z0-9]+\\)\\)[^a-z0-9]"
          1 0 'Man-xref-man-page)))))


;;; prolog

(with-eval-after-load 'prolog
  (setq prolog-system 'swi)
  (setq prolog-indent-width 8)
  (setq prolog-electric-dot-flag t)
  (setq prolog-program-switches
        '((sicstus ("-i"))
          (swi ("-G8M"))
          (t nil)))
  (setq prolog-info-predicate-index "(prolog)Predicates188"))

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
                )
              auto-mode-alist))

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
   (setq-local outline-regexp "%%%+")
   (setq-local outline-level (lambda () (- 5 (outline-level))))
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
   (setq-local outline-regexp "^[1-9][0-9]* \\?- ")
   (setq-local outline-level (lambda () 1))))

(defun my-search-prolog-doc-at-point ()
  (let* (;;(wordchars "a-zA-Z_0-9")
         (str
          (concat "\^L\n\n"
                  (current-word)
                  ;; (buffer-substring-no-properties
                  ;; (save-excursion (skip-chars-backward wordchars) (point))
                  ;; (save-excursion (skip-chars-forward  wordchars) (point)))
                  "(")))
    (view-file "~/doc/prog/prolog/PROLOG")
    ;; (setq-local outline-regexp "^\\(Chapter [0-9]\\|\\)")
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

(with-eval-after-load 'erlang
  (setq erlang-inferior-shell-split-window nil)
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq tab-width 2)
              (alchemist-mode 1))))

(add-to-list 'auto-mode-alist '("\\.eex\\'" . html-erb-mode))

(with-eval-after-load 'alchemist
  ;; (global-company-mode)
  ;; (define-key elixir-mode-map "\M-\t" 'company-complete)
  ;; (define-key company-active-map [escape] 'company-abort)
  )


;;; haskell

;; also qv comment in (qv "files.el" "^(defvar interpreter-mode-alist")
(add-to-list 'interpreter-mode-alist '("runhugs" . literate-haskell-mode))


;;; html

(add-to-list 'auto-mode-alist '("\\.mustache\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))

;; These are needed to set before loading sgml-mode.el:
;; (setq sgml-quick-keys t)
(with-eval-after-load 'sgml-mode
  (setq html-quick-keys t)
  (defvar sgml-mode-syntax-table)
  (modify-syntax-entry ?. "." sgml-mode-syntax-table)
  (modify-syntax-entry ?: "." sgml-mode-syntax-table)
  (defvar html-tag-face-alist)
  (setq html-tag-face-alist (append '(("a" . underline))
                                    html-tag-face-alist)))

(with-eval-after-load 'sgml-mode
  (define-skeleton html-headline-1
    "HTML level 1 headline tags."
    nil
    "<h1><a name=\"" (setq str (read-string "Name: "))
    "\" id=\"" str "\">" _ "</a></h1>")

  (define-skeleton html-headline-2
    "HTML level 2 headline tags."
    nil
    "<h2><a name=\"" (setq str (read-string "Name: "))
    "\" id=\"" str "\">" _ "</a></h2>")

  (add-hook 'sgml-mode-hook
          (lambda ()
            ;; Don't insert newlines after <span></span>
            (setq-local skeleton-end-newline nil))))

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


;;; web development

(with-eval-after-load 'cc-mode
  (add-hook 'java-mode-hook
            (lambda ()
              (setq tab-width 4))))

(with-eval-after-load 'css-mode
  (add-hook 'css-mode-hook
            (lambda ()
              (setq tab-width 2))))

(with-eval-after-load 'js
  (add-hook 'js-mode-hook
            (lambda ()
              (setq js-indent-level 2)
              (setq tab-width 2))))

;; (defvar ruby-use-smie nil)
(with-eval-after-load 'ruby-mode
  ;; (setq ruby-use-smie nil)
  (define-key ruby-mode-map [(control left)] 'ruby-backward-sexp)
  (define-key ruby-mode-map [(control right)] 'ruby-forward-sexp)
  (when delete-selection-mode
    (put 'ruby-end-return 'delete-selection t))
  (add-hook 'ruby-mode-hook
            (lambda ()
              (setq-local require-final-newline nil)
              ;; Don't enable flymake-mode in read-only buffers
              (flymake-mode 1)
              (add-hook 'view-mode-hook
                        (lambda ()
                          (flymake-mode (if view-mode -1 1)))
                        nil t))))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map [left-fringe mouse-1]
    'flymake-show-diagnostics-buffer))

(add-to-list 'auto-mode-alist '("\\.slang\\'" . slim-mode)) ; for Crystal/Amber


;;; css

;; If there is no css-mode available already, use c-mode for .css files.
(unless (rassoc 'css-mode auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . c-mode)))

(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
;; 'scss-mode' was added to Emacs 25.1
;; (add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
;; '.jsx' was added to auto-mode-alist in the new version of Emacs
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . conf-mode))


;;; sql

;; Pleroma uses .psql file extension
(add-to-list 'auto-mode-alist '("\\.p?sql\\'" . sql-mode))


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

(with-eval-after-load 'outline
;;   (define-key outline-mode-map [(control ?o)] outline-mode-prefix-map)
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
  (require 'foldout))

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
;;   (if (save-excursion (forward-line 1) (looking-at outline-regexp))
;;       ;; (save-excursion (outline-end-of-heading) (outline-visible))
;;       (outline-hide-subtree)
;;     (progn (outline-hide-entry) (beginning-of-line))))

(defun my-outline-hide-entry-or-subtree ()
  (interactive)
  (if (save-excursion (forward-line 1) (or (looking-at outline-regexp) (eobp)))
      (if (>= (funcall outline-level)
              (save-excursion (forward-line 1)
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
                             (outline-next-visible-heading 1) ; (forward-line 1)
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

;; Allow image-mode to open WEBP images.
;; After installing `sudo apt-get install webp`, the command
;; `identify -list delegate` confirms it's supported:
;; webp => "dwebp' -pam '%i' -o '%o"

(when (boundp 'imagemagick-enabled-types)
  (add-to-list 'imagemagick-enabled-types 'WEBP))

(advice-add 'imagemagick-types :around
            (lambda (orig-fun &rest args)
              (append (apply orig-fun args) '(WEBP)))
            '((name . add-webp)))

(imagemagick-register-types)

;; But what's BAD is that it writes to ~/.xsession-errors such info:
;; Decoded /tmp/magick-2449whQo5ntkvXs3. Dimensions: 635 x 846 . Format: lossy. Now saving...
;; Saved file /tmp/magick-2449WjsPHiXypqga


;;; thumbs

(defadvice thumbs-mode (after my-thumbs-mode activate)
  (toggle-truncate-lines -1))


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
    ;; (setq-local outline-regexp "^.*:$")
    ;; (outline-minor-mode 1)
    ))


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
                             :notify (lambda (widget &rest _ignore)
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
  ;; (define-key my-map "dm" 'my-dictem-run-search)
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

  (define-key my-map "wd" 'my-dictem-run-search)

  (defun my-dictem-run-search-from-clipboard-or-word-at-point ()
    (interactive)
    (my-dictem-run-search
     (or (and (use-region-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end)))
         (thing-at-point 'word)
         (and kill-ring
              (string-match-p "\\`[A-Za-z]+\\'" (current-kill 0))
              (current-kill 0))
         (dictem-read-query)))
    (goto-char (point-max)))

  (define-key my-map "ww" 'my-dictem-run-search-from-clipboard-or-word-at-point)
  (global-set-key "\M-s\M-d" 'my-dictem-run-search-from-clipboard-or-word-at-point)

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
              (setq-local outline-regexp "From")
              (outline-minor-mode 1)
              ;; (define-key dictem-mode-map [(meta left)]  'my-dictem-prev-word?)
              (define-key dictem-mode-map [(meta right)] 'my-dictem-run-search)))

  (push '("\\`\\*dictem.*" display-buffer-same-window)
        display-buffer-alist)

  ;; Stupid bug pushes an empty string to kill-ring
  (advice-add 'dictem-base-do-default-server :around
              (lambda (orig-fun &rest args)
                (let (kill-ring kill-ring-yank-pointer)
                  (apply orig-fun args)))
              '((name . dictem-base-do-default-server-no-kill-ring)))

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
(with-eval-after-load 'w3
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
  )


;;; w3m

(with-eval-after-load 'w3m
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
  )

(add-hook
 'w3m-display-hook
 (lambda (_url)
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


;;; bbdb

;; (when (require 'bbdb nil t)
;;   (bbdb-initialize 'gnus 'message 'w3)
;;   (add-hook 'bbdb-mode-hook 'view-mode))


;;; ange-ftp

(with-eval-after-load 'ange-ftp
  ;; Ignore more messages in the FTP client with Finnish messages
  (setq ange-ftp-skip-msgs (concat ange-ftp-skip-msgs "\\|^Yhdistetty")))


;;; gnuserv

;; NOTE: (server-start) conflicts with (customize)!
;; (when (require 'gnuserv nil t)
;;  (setq gnuserv-frame (selected-frame))
;;  (gnuserv-start))

;; TEST: comment in again: [2005-05-09]!
;; (and (fboundp 'gnuserv-start) (gnuserv-start))


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


;;; wget

(require 'wget nil t)

(setq wget-download-directory ".")
(setq wget-download-log-file "DOWNLOAD")
(setq wget-download-log-format "%U\n")
;; (setq wget-download-log-format "%T %U\n")
;; (setq wget-download-log-time-format "%Y-%m-%d")

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

(with-eval-after-load 'tetris
  (define-key tetris-mode-map [up]   'tetris-rotate-next)
  (define-key tetris-mode-map [down] 'tetris-move-bottom))

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


;;; Local Variables:
;;; mode: emacs-lisp
;;; outline-regexp: ";;;;* "
;;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;;; time-stamp-start: "Version: "
;;; time-stamp-format: "%:y-%02m-%02d for GNU Emacs 28.0.50 (x86_64-pc-linux-gnu)"
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 15
;;; End:

;;; .emacs ends here
