;;; custom

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(Man-overstrike-face (quote underline))
 '(apropos-do-all t)
 '(auto-insert-mode t)
 '(battery-mode-line-format "[%b%p%%,%t]")
 '(bbdb-use-pop-up nil)
 '(blink-matching-delay 0.1)
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-new-window-flag t)
 '(c-echo-syntactic-information-p t)
 '(calendar-date-display-form (quote ((format "%04s-%02d-%02d" year (string-to-int month) (string-to-int day)))))
 '(calendar-mark-diary-entries-flag t)
 '(calendar-time-display-form (quote (24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))))
 '(calendar-view-diary-initially-flag t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 65535)
 '(comint-move-point-for-output (quote others))
 '(comint-scroll-show-maximum-output t)
 '(compare-ignore-whitespace t)
 '(compare-windows-recenter (quote (15 15)))
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output (quote first-error))
 '(completion-show-help nil)
 '(completions-format (quote vertical))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(cperl-continued-statement-offset 0)
 '(cursor-in-non-selected-windows nil)
 '(custom-buffer-done-function (quote kill-buffer))
 '(debug-on-error t)
 '(delete-selection-mode t)
 '(describe-char-unidata-list t)
 '(desktop-save-mode t)
 '(diary-date-forms (quote ((year "-" month "-" day "[^0-9]") (dayname "\\W"))))
 '(diary-list-include-blanks t)
 '(diff-switches (quote ("-u")))
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-isearch-filenames (quote dwim))
 '(dired-keep-marker-rename 82)
 '(dired-listing-switches "-Al --group-directories-first --block-size='1")
 '(dired-omit-size-limit 65535)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote top))
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(doc-view-continuous t)
 '(edebug-save-windows nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(ee-ps-program-switches (quote ("aux" "--columns" "1024")))
 '(emacs-lisp-docstring-fill-column 75)
 '(enable-recursive-minibuffers t)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size nil)
 '(european-calendar-style t)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(ffap-machine-p-known (quote accept))
 '(fill-column 75)
 '(find-file-visit-truename t)
 '(find-grep-options "-iq")
 '(find-ls-option (quote ("-exec ls -ld {} \\;" . "-ld")))
 '(garbage-collection-messages t)
 '(gc-cons-threshold 40000000)
 '(global-mark-ring-max 1024)
 '(gnus-break-pages nil)
 '(gnus-extra-headers (quote (To Cc Newsgroups)))
 '(gnus-global-score-files (quote ("~/News/")))
 '(gnus-permanently-visible-groups "^nn")
 '(gnus-summary-ignore-duplicates t)
 '(gnus-treat-display-face nil)
 '(gnus-treat-display-smileys nil)
 '(gnus-treat-display-x-face nil)
 '(gnus-user-agent (quote (gnus emacs config)))
 '(grep-command "grep -inH -e ")
 '(grep-find-template "find . <X> -type f <F> -print0 | sort -z | xargs -0 -e grep <C> -nH -e <R>")
 '(highlight-nonselected-windows t)
 '(history-delete-duplicates t)
 '(history-length t)
 '(htmlize-css-name-prefix "emacs-")
 '(htmlize-html-major-mode (quote html-mode))
 '(icomplete-mode t)
 '(image-dired-thumbnail-storage (quote standard))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote ((up . right) (down . right))))
 '(inhibit-startup-echo-area-message "juri")
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice nil)
 '(isearch-allow-scroll t)
 '(isearch-buffers-pause nil)
 '(kill-do-not-save-duplicates t)
 '(kill-ring-max 1024)
 '(kill-whole-line t)
 '(large-file-warning-threshold 27000000)
 '(lazy-highlight-max-at-a-time nil)
 '(list-command-history-max nil)
 '(ls-lisp-dirs-first t)
 '(mail-source-delete-incoming 14)
 '(mark-even-if-inactive nil)
 '(mark-ring-max 1024)
 '(message-log-max t)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(mm-discouraged-alternatives (quote ("text/html")))
 '(mm-text-html-renderer (quote w3m))
 '(mouse-highlight 1)
 '(nnmail-crosspost-link-function (quote copy-file))
 '(nnmail-extra-headers (quote (To Cc)))
 '(parens-require-spaces nil)
 '(proced-descend nil)
 '(proced-filter (quote all))
 '(proced-format (quote medium))
 '(proced-sort (quote pid))
 '(read-file-name-completion-ignore-case t)
 '(read-quoted-char-radix 16)
 '(recenter-positions (quote (0.15 top)))
 '(recentf-exclude (quote ("/[^/]+[@:]")))
 '(recentf-mode t)
 '(require-final-newline t)
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(scroll-preserve-screen-position t)
 '(send-mail-function (quote smtpmail-send-it))
 '(set-mark-command-repeat-pop t)
 '(sgml-xml-mode t)
 '(smtpmail-debug-info t)
 '(smtpmail-queue-mail t)
 '(sql-sqlite-program "sqlite3")
 '(thumbs-per-line 65536)
 '(thumbs-relief 0)
 '(track-eol t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(vc-bzr-diff-switches "-F ^(")
 '(view-inhibit-help-message t)
 '(view-read-only t)
 '(visible-bell t)
 '(w3m-default-display-inline-images t)
 '(w3m-display-inline-image t)
 '(w3m-home-page "http://www.jurta.org/")
 '(w3m-key-binding (quote info))
 '(w3m-view-this-url-new-session-in-background t)
 '(wget-debug t)
 '(woman-use-own-frame nil)
 '(x-select-enable-clipboard t)
 '(yank-excluded-properties t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(buffer-menu-buffer ((t (:foreground "black"))))
 '(comint-highlight-input ((t (:underline t))))
 '(compare-windows ((t (:inherit region))))
 '(compilation-info ((t (:foreground "ForestGreen"))))
 '(compilation-warning ((t (:foreground "DarkRed"))))
 '(completions-common-part ((t (:inherit shadow))))
 '(completions-first-difference ((t (:underline t))))
 '(cperl-array ((t (:foreground "blue"))))
 '(cperl-hash ((t (:foreground "blue"))))
 '(cperl-nonoverridable ((((class color) (background light)) (:foreground "blue"))))
 '(custom-group-tag ((t (:foreground "brown"))))
 '(custom-variable-tag ((t (:foreground "brown"))))
 '(cvs-handled ((((class color) (background light)) (:foreground "slate gray"))))
 '(cvs-header ((t (:foreground "SteelBlue"))))
 '(cvs-marked ((t (:foreground "DarkRed"))))
 '(cvs-need-action ((((class color) (background light)) (:foreground "darkgreen"))))
 '(diary ((((class color)) (:foreground "blue"))))
 '(diff-file-header ((((class color) (background light)) (:background "grey70" :underline t :weight normal)) (((class color) (background dark)) (:background "grey45"))))
 '(diff-header ((((class color) (background light)) (:background "grey85")) (((class color) (background dark)) (:background "grey35"))))
 '(diff-indicator-added ((t (:inherit diff-indicator-changed))))
 '(diff-indicator-changed ((((class color) (background light)) (:foreground "blue")) (((class color) (background dark)) (:foreground "RoyalBlue"))))
 '(diff-indicator-removed ((t (:inherit diff-indicator-changed))))
 '(diff-refine-change ((((class color) (min-colors 88) (background light)) (:background "khaki1"))))
 '(eshell-prompt ((t (:foreground "DarkRed" :underline t :weight normal))))
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "Sienna")) (((class color) (background dark)) (:foreground "Sienna"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "DeepSkyBlue")) (t (:foreground "Blue1"))))
 '(font-lock-negation-char-face ((t (:foreground "DarkRed"))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "DarkGreen")) (((class color) (background dark)) (:foreground "ForestGreen"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "SteelBlue")) (((class color) (background dark)) (:foreground "SteelBlue"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Blue3")) (((class color) (background dark)) (:foreground "SteelBlue"))))
 '(font-lock-warning-face ((t (:foreground "Red"))))
 '(fringe ((t (:inherit default))))
 '(gnus-cite-5 ((((class color) (background light)) (:foreground "DarkOliveGreen"))))
 '(gnus-group-mail-2 ((t (:foreground "magenta4" :underline t :weight normal))))
 '(gnus-group-mail-2-empty ((((class color) (background light)) (:foreground "magenta4"))))
 '(gnus-summary-cancelled ((((class color) (background light)) (:foreground "grey50")) (((class color) (background dark)) (:foreground "grey30"))))
 '(gnus-summary-high-ticked ((t (:foreground "firebrick" :weight normal))))
 '(gnus-summary-high-unread ((t (:foreground "blue"))))
 '(help-argument-name ((t (:foreground "SkyBlue4"))))
 '(holiday ((((class color)) (:background "tan"))))
 '(html-tag-face ((t (:foreground "blue"))))
 '(isearch ((((class color) (background light)) (:background "magenta4" :foreground "lightskyblue1")) (((class color) (background dark)) (:background "palevioletred2" :foreground "brown4"))))
 '(message-cited-text ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(mm-uu-extract ((((class color) (background light)) (:background "AntiqueWhite3"))))
 '(mode-line-buffer-id ((t nil)))
 '(mode-line-inactive ((t (:inherit mode-line :box (:line-width -1 :style pressed-button)))))
 '(region ((((class color) (background light)) (:background "DarkGrey"))))
 '(sh-heredoc ((((class color) (background light)) (:foreground "ForestGreen"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey30")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(show-paren-match ((((class color)) (:background "DarkGrey"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "pink"))))
 '(w3m-arrived-anchor-face ((((class color) (background light)) (:foreground "DarkMagenta"))))
 '(w3m-current-anchor-face ((t (:foreground "red"))))
 '(widget-button ((t nil))))
