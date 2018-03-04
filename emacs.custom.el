;;; custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 25)
 '(Info-fontify-maximum-menu-size t)
 '(Man-notify-method 'pushy)
 '(Man-overstrike-face 'underline)
 '(apropos-do-all t)
 '(async-shell-command-buffer 'confirm-rename-buffer)
 '(async-shell-command-display-buffer nil)
 '(auto-insert-mode t)
 '(battery-mode-line-format "[%b%p%%,%t]")
 '(bbdb-use-pop-up nil)
 '(bidi-paragraph-direction 'left-to-right)
 '(blink-matching-delay 0.1)
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-new-window-flag t)
 '(c-echo-syntactic-information-p t)
 '(calendar-date-display-form
   '((format "%04s-%02d-%02d" year
             (string-to-number month)
             (string-to-number day))))
 '(calendar-mark-diary-entries-flag t)
 '(calendar-mark-holidays-flag t)
 '(calendar-time-display-form
   '(24-hours ":" minutes
              (if time-zone " (")
              time-zone
              (if time-zone ")")))
 '(calendar-view-diary-initially-flag t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(comint-history-isearch 'dwim)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 65535)
 '(comint-move-point-for-output nil)
 '(comint-terminfo-terminal "xterm")
 '(company-selection-wrap-around t)
 '(compare-ignore-whitespace t)
 '(compare-windows-recenter '(15 15))
 '(compilation-ask-about-save nil)
 '(compilation-environment '("LANG=C"))
 '(compilation-scroll-output 'first-error)
 '(compile-command "make")
 '(completion-show-help nil)
 '(completions-format 'vertical)
 '(confirm-kill-emacs 'y-or-n-p)
 '(cperl-continued-statement-offset 0)
 '(css-indent-offset 2)
 '(cursor-in-non-selected-windows nil)
 '(custom-buffer-done-function 'kill-buffer)
 '(debug-on-error t)
 '(delete-selection-mode t)
 '(describe-char-unidata-list t)
 '(desktop-auto-save-timeout 60)
 '(desktop-restore-in-current-display t)
 '(desktop-save-mode t)
 '(diary-date-forms '((year "-" month "-" day "[^0-9]") (dayname "\\W")))
 '(diary-list-include-blanks t)
 '(diff-switches '("-u"))
 '(dired-auto-revert-buffer t)
 '(dired-create-destination-dirs 'ask)
 '(dired-dwim-target t)
 '(dired-isearch-filenames 'dwim)
 '(dired-keep-marker-rename 82)
 '(dired-listing-switches "-Al --group-directories-first --block-size='1")
 '(dired-omit-size-limit 65535)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'top)
 '(display-buffer-alist
   '((display-buffer-condition-from-help display-buffer-same-window)
     ("\\`\\*\\(Apropos\\|Buffer List\\|Colors\\|Command History\\|Locate\\|Messages\\|Proced\\|eww\\|snd\\|vc-log\\)\\*\\'" display-buffer-same-window)
     ("\\`\\*\\(compilation\\|grep\\|erlang\\|haskell\\|Diff\\|Help\\|shell\\|Shell Command Output\\)\\*\\(\\|<[0-9]+>\\)\\'" display-buffer-same-window)
     ("\\`\\*\\(vc-dir\\)\\*\\(\\|<[0-9a-z-]+>\\)\\'" display-buffer-same-window)
     ("\\`\\*dictem.*" display-buffer-same-window)))
 '(display-line-numbers-grow-only t)
 '(display-raw-bytes-as-hex t)
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(doc-view-continuous t)
 '(doc-view-resolution 200)
 '(edebug-save-windows nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ee-ps-program-switches '("aux" "--columns" "1024"))
 '(electric-indent-mode nil)
 '(emacs-lisp-docstring-fill-column 75)
 '(enable-recursive-minibuffers t)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size nil)
 '(european-calendar-style t)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(eval-expression-print-maximum-character 65535)
 '(ffap-machine-p-known 'accept)
 '(fill-column 75)
 '(find-file-visit-truename t)
 '(find-grep-options "-iq")
 '(find-ls-option '("-exec ls -ld {} \\;" . "-ld"))
 '(garbage-collection-messages t)
 '(gc-cons-threshold 40000000)
 '(global-eldoc-mode nil)
 '(global-mark-ring-max 1024)
 '(gnus-article-truncate-lines nil)
 '(gnus-article-update-date-headers nil)
 '(gnus-break-pages nil)
 '(gnus-extra-headers '(To Cc Newsgroups))
 '(gnus-global-score-files '("~/News/"))
 '(gnus-permanently-visible-groups "^nn")
 '(gnus-summary-ignore-duplicates t)
 '(gnus-treat-display-face nil)
 '(gnus-treat-display-smileys nil)
 '(gnus-treat-display-x-face nil)
 '(gnus-user-agent '(gnus emacs config))
 '(grep-command "grep --color -inH -e ")
 '(grep-find-template
   "find . <X> -type f <F> -print0 | sort -z | xargs -0 -e grep <C> --color -inH -e <R>")
 '(grep-use-null-device nil)
 '(grep-use-null-filename-separator nil)
 '(highlight-nonselected-windows t)
 '(history-delete-duplicates t)
 '(history-length t)
 '(htmlize-css-name-prefix "emacs-")
 '(htmlize-html-major-mode 'html-mode)
 '(htmlize-output-type 'inline-css)
 '(icomplete-mode t)
 '(image-dired-thumbnail-storage 'standard)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries '((up . right) (down . right)))
 '(inhibit-startup-echo-area-message "juri")
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice nil)
 '(isearch-allow-scroll t)
 '(js-indent-level 2 t)
 '(kill-do-not-save-duplicates t)
 '(kill-ring-max 1024)
 '(kill-whole-line t)
 '(large-file-warning-threshold 27000000)
 '(lazy-highlight-max-at-a-time nil)
 '(list-command-history-max nil)
 '(list-matching-lines-buffer-name-face 'header-line)
 '(list-matching-lines-current-line-face 'region)
 '(list-matching-lines-jump-to-current-line t)
 '(log-edit-confirm t)
 '(log-edit-hook
   '(log-edit-insert-changelog log-edit-insert-filenames-without-changelog))
 '(ls-lisp-dirs-first t)
 '(mail-source-delete-incoming 30)
 '(mark-even-if-inactive nil)
 '(mark-ring-max 1024)
 '(message-log-max t)
 '(message-send-mail-function 'smtpmail-send-it)
 '(minibuffer-depth-indicate-mode t)
 '(mm-discouraged-alternatives '("text/html"))
 '(mm-text-html-renderer 'w3m)
 '(mode-line-percent-position '(6 "%q"))
 '(mouse-avoidance-banish-position
   '((frame-or-window . frame)
     (side . right)
     (side-pos . -3)
     (top-or-bottom . bottom)
     (top-or-bottom-pos . 6)))
 '(mouse-avoidance-mode 'banish nil (avoid))
 '(mouse-highlight 1)
 '(mouse-select-region-move-to-beginning t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control))))
 '(multi-isearch-pause nil)
 '(next-error-recenter 15)
 '(nnmail-crosspost-link-function 'copy-file)
 '(nnmail-extra-headers '(To Cc))
 '(parens-require-spaces nil)
 '(perl-indent-level 2)
 '(perl-prettify-symbols nil)
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(proced-descend nil)
 '(proced-filter 'all)
 '(proced-format 'medium)
 '(proced-sort 'pid)
 '(read-file-name-completion-ignore-case t)
 '(read-quoted-char-radix 16)
 '(read-regexp-defaults-function 'find-tag-default-as-regexp)
 '(recenter-positions '(0.15 top))
 '(recentf-exclude '("/[^/]+[@:]"))
 '(recentf-mode t)
 '(require-final-newline t)
 '(safe-local-variable-values '((encoding . utf-8)))
 '(save-interprogram-paste-before-kill t)
 '(save-place-limit nil)
 '(save-place-mode t)
 '(save-place-save-skipped nil)
 '(scroll-error-top-bottom t)
 '(scroll-preserve-screen-position t)
 '(search-exit-option 'shift-move)
 '(select-active-regions nil)
 '(select-enable-primary t)
 '(send-mail-function 'smtpmail-send-it)
 '(set-mark-command-repeat-pop t)
 '(sgml-xml-mode t)
 '(shift-select-mode 'permanent)
 '(smtpmail-debug-info t)
 '(smtpmail-queue-mail t)
 '(split-height-threshold 100)
 '(sql-sqlite-login-params '((database :file ".*")))
 '(sql-sqlite-program "sqlite3")
 '(switch-to-buffer-preserve-window-point nil)
 '(temp-buffer-resize-mode t)
 '(thumbs-per-line 65536)
 '(thumbs-relief 0)
 '(tool-bar-style 'both-horiz)
 '(track-eol t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-bzr-diff-switches "-F ^(")
 '(vc-bzr-status-switches nil)
 '(vc-command-messages t)
 '(vc-git-print-log-follow t)
 '(view-inhibit-help-message t)
 '(view-read-only t)
 '(visible-bell t)
 '(visual-order-cursor-movement t)
 '(w3m-default-display-inline-images t)
 '(w3m-display-inline-image t)
 '(w3m-home-page "http://www.jurta.org/")
 '(w3m-key-binding 'info)
 '(w3m-view-this-url-new-session-in-background t)
 '(wget-debug t)
 '(windmove-wrap-around t)
 '(woman-use-own-frame nil)
 '(yank-excluded-properties t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-menu-buffer ((t (:foreground "black"))))
 '(button ((t (:inherit link :underline t))))
 '(c-annotation-face ((t (:inherit font-lock-comment-face))))
 '(comint-highlight-input ((t (:underline t))))
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
 '(diff-context ((t nil)))
 '(diff-file-header ((((class color) (background light)) (:background "grey70" :underline t :weight normal)) (((class color) (background dark)) (:background "grey45"))))
 '(diff-header ((((class color) (background light)) (:background "grey85")) (((class color) (background dark)) (:background "grey35"))))
 '(diff-indicator-added ((t (:inherit diff-added :foreground "blue"))))
 '(diff-indicator-changed ((t (:inherit diff-changed :foreground "blue"))))
 '(diff-indicator-removed ((t (:inherit diff-removed :foreground "blue"))))
 '(dired-directory ((t (:inherit link :underline nil))))
 '(eshell-prompt ((t (:foreground "DarkRed" :underline t :weight normal))))
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "Sienna")) (((class color) (background dark)) (:foreground "Sienna"))))
 '(font-lock-comment-face ((t (:foreground "Firebrick"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "DeepSkyBlue")) (t (:foreground "Blue1"))))
 '(font-lock-negation-char-face ((t (:foreground "DarkRed"))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "ForestGreen"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "SteelBlue")) (((class color) (background dark)) (:foreground "SteelBlue"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Blue3")) (((class color) (background dark)) (:foreground "SteelBlue"))))
 '(font-lock-warning-face ((t (:foreground "Red"))))
 '(fringe ((t (:inherit default))))
 '(glyphless-char ((t (:background "red"))))
 '(gnus-button ((t (:inherit button))))
 '(gnus-cite-5 ((((class color) (background light)) (:foreground "DarkOliveGreen"))))
 '(gnus-group-mail-2 ((t (:foreground "magenta4" :underline t :weight normal))))
 '(gnus-group-mail-2-empty ((((class color) (background light)) (:foreground "magenta4"))))
 '(gnus-summary-cancelled ((t (:inherit shadow))))
 '(gnus-summary-high-ancient ((t (:foreground "RoyalBlue"))))
 '(gnus-summary-high-ticked ((t (:foreground "firebrick" :weight normal))))
 '(gnus-summary-high-unread ((t (:foreground "blue"))))
 '(gnus-summary-low-read ((t (:foreground "light goldenrod"))))
 '(gnus-summary-normal-ancient ((t (:foreground "gray30"))))
 '(help-argument-name ((t (:foreground "SkyBlue4"))))
 '(holiday ((((class color)) (:background "tan"))))
 '(html-tag-face ((t (:foreground "blue"))))
 '(icomplete-first-match ((t nil)))
 '(info-xref ((t (:inherit link :underline t))))
 '(isearch ((((class color) (background light)) (:background "magenta4" :foreground "lightskyblue1")) (((class color) (background dark)) (:background "palevioletred2" :foreground "brown4"))))
 '(link ((t (:foreground "medium blue"))))
 '(message-cited-text ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(mm-uu-extract ((t (:background "gainsboro"))))
 '(mode-line-buffer-id ((t nil)))
 '(mode-line-inactive ((t (:inherit mode-line :box (:line-width -1 :style pressed-button)))))
 '(region ((((class color) (background light)) (:background "gainsboro"))))
 '(sh-heredoc ((((class color) (background light)) (:foreground "ForestGreen"))))
 '(show-paren-match ((((class color)) (:background "gainsboro"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "pink"))))
 '(w3m-arrived-anchor-face ((((class color) (background light)) (:foreground "DarkMagenta"))))
 '(w3m-current-anchor-face ((t (:foreground "red"))))
 '(widget-button ((t nil))))
