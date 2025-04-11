;;; custom  -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 25)
 '(Info-fontify-maximum-menu-size t)
 '(Man-notify-method 'pushy)
 '(Man-overstrike-face 'underline)
 '(Man-width-max 94)
 '(add-log-keep-changes-together t)
 '(apropos-do-all t)
 '(async-shell-command-buffer 'confirm-rename-buffer)
 '(async-shell-command-display-buffer t)
 '(async-shell-command-width 222)
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
 '(calendar-date-style 'iso)
 '(calendar-mark-diary-entries-flag t)
 '(calendar-mark-holidays-flag t)
 '(calendar-time-display-form
   '(24-hours ":" minutes
              (if time-zone " (")
              time-zone
              (if time-zone ")")))
 '(calendar-view-diary-initially-flag t)
 '(calendar-week-start-day 1)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(comint-history-isearch 'dwim)
 '(company-selection-wrap-around t)
 '(compare-ignore-whitespace t)
 '(compilation-ask-about-save nil)
 '(compilation-environment '("LANG=C" "TERM=dumb"))
 '(compilation-scroll-output 'first-error)
 '(compile-command "make")
 '(completion-auto-select 'second-tab)
 '(completion-category-overrides
   '((buffer
      (display-sort-function . identity))
     (imenu
      (display-sort-function . minibuffer-sort-alphabetically))
     (project-file
      (styles substring))
     (unicode-name
      (group-p . t))))
 '(completion-preview-minimum-symbol-length 2)
 '(completion-show-help nil)
 '(completions-detailed t)
 '(completions-format 'vertical)
 '(completions-group-sort 'alphabetical)
 '(confirm-kill-emacs 'y-or-n-p)
 '(cperl-continued-statement-offset 0)
 '(css-indent-offset 2)
 '(cursor-in-non-selected-windows nil)
 '(custom-buffer-done-function 'kill-buffer)
 '(custom-unlispify-tag-names nil)
 '(debug-on-error t)
 '(default-transient-input-method "compose")
 '(delete-old-versions 'other)
 '(delete-pair-blink-delay 0.3)
 '(delete-pair-push-mark t)
 '(delete-selection-mode t)
 '(delete-window-choose-selected 'pos)
 '(describe-bindings-outline t)
 '(describe-bindings-outline-rules
   '((match-regexp . "Key translations\\|Minor Mode Bindings")))
 '(describe-char-unidata-list t)
 '(desktop-auto-save-timeout 60)
 '(desktop-globals-to-save
   '(compile-history extended-command-history find-args-history find-command-history grep-history read-expression-history regexp-history dired-shell-command-history shell-command-history vc-git-history vc-revision-history gud-gdb-history))
 '(desktop-restore-in-current-display t)
 '(desktop-save-mode t)
 '(diary-date-forms '((year "-" month "-" day "[^0-9]") (dayname "\\W")))
 '(diary-list-include-blanks t)
 '(dictionary-create-buttons nil)
 '(diff-default-read-only t)
 '(diff-font-lock-syntax 'hunk-also)
 '(diff-switches '("-u"))
 '(dired-auto-revert-buffer t)
 '(dired-clean-confirm-killing-deleted-buffers nil)
 '(dired-create-destination-dirs 'ask)
 '(dired-do-revert-buffer '(lambda (dir) (not (file-remote-p dir))))
 '(dired-dwim-target 'dired-dwim-target-recent)
 '(dired-filename-display-length 'window)
 '(dired-free-space 'separate)
 '(dired-isearch-filenames 'dwim)
 '(dired-keep-marker-rename 82)
 '(dired-listing-switches
   "-Alv --group-directories-first --time-style=long-iso --block-size='1")
 '(dired-movement-style 'cycle-files)
 '(dired-omit-size-limit 65535)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'top)
 '(dired-vc-rename-file t)
 '(display-fill-column-indicator-warning t)
 '(display-line-numbers-grow-only t)
 '(display-raw-bytes-as-hex t)
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(doc-view-continuous t)
 '(doc-view-resolution 200)
 '(duplicate-line-final-position 1)
 '(duplicate-region-final-position 1)
 '(edebug-print-length 1024)
 '(edebug-save-displayed-buffer-points t)
 '(edebug-save-windows nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ee-ps-program-switches '("aux" "--columns" "1024"))
 '(eglot-confirm-server-edits '((eglot-rename . diff) (t . maybe-summary)))
 '(electric-indent-mode nil)
 '(emacs-lisp-docstring-fill-column 75)
 '(enable-recursive-minibuffers t)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size nil)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(eval-expression-print-maximum-character 65535)
 '(ffap-machine-p-known 'accept)
 '(fill-column 75)
 '(find-dired-refine-function nil)
 '(find-file-visit-truename t)
 '(find-grep-options "-iq")
 '(find-ls-option '("-print0 | sort -z | xargs -0 -e ls -ld" . "-ld"))
 '(flymake-no-changes-timeout nil)
 '(flymake-show-diagnostics-at-end-of-line t)
 '(gdb-display-io-buffer nil)
 '(global-eldoc-mode nil)
 '(global-mark-ring-max 1024)
 '(gnus-article-truncate-lines nil)
 '(gnus-article-update-date-headers nil)
 '(gnus-break-pages nil)
 '(gnus-button-message-level 4)
 '(gnus-extra-headers '(To Cc Newsgroups))
 '(gnus-global-score-files '("~/News/"))
 '(gnus-permanently-visible-groups "^nn")
 '(gnus-summary-ignore-duplicates t)
 '(gnus-treat-display-face nil)
 '(gnus-treat-display-smileys nil)
 '(gnus-treat-display-x-face nil)
 '(gnus-treat-strip-trailing-blank-lines 'last)
 '(gnus-user-agent '(gnus emacs config))
 '(grep-command "grep --color -inH -e ")
 '(grep-find-template
   "find <D> <X> -type f <F> -print0 | sort -z | xargs -0 -e rg <C> -nH --no-heading --null -j8 --sort path -M 200 --max-columns-preview -e <R>")
 '(grep-find-use-xargs 'gnu-sort)
 '(grep-program "rg")
 '(grep-use-headings t)
 '(grep-use-null-device nil)
 '(grep-use-null-filename-separator nil)
 '(gud-highlight-current-line t)
 '(help-clean-buttons t)
 '(help-enable-variable-value-editing t)
 '(hi-lock-file-patterns-policy 'always)
 '(highlight-nonselected-windows t)
 '(history-delete-duplicates t)
 '(history-length t)
 '(htmlize-css-name-prefix "emacs-")
 '(htmlize-html-major-mode 'html-mode)
 '(htmlize-output-type 'inline-css)
 '(icomplete-mode t)
 '(icomplete-prospects-height 1)
 '(image-converter 'imagemagick)
 '(image-dired-thumbnail-storage 'standard)
 '(image-use-external-converter t)
 '(imenu-eager-completion-buffer nil)
 '(imenu-flatten 'annotation)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries '((up . right) (down . right)))
 '(inf-ruby-console-environment "development")
 '(inf-ruby-prompt-read-only nil)
 '(inhibit-startup-echo-area-message "juri")
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice nil)
 '(ispell-silently-savep t)
 '(js-indent-level 2 t)
 '(kill-buffer-quit-windows t)
 '(kill-do-not-save-duplicates t)
 '(kill-ring-max 1024)
 '(kill-whole-line t)
 '(large-file-warning-threshold 64000000)
 '(list-command-history-max nil)
 '(list-matching-lines-buffer-name-face 'header-line)
 '(list-matching-lines-current-line-face 'region)
 '(list-matching-lines-jump-to-current-line t)
 '(load-prefer-newer t)
 '(log-edit-confirm t)
 '(log-edit-hook
   '(log-edit-insert-changelog log-edit-insert-filenames-without-changelog))
 '(ls-lisp-dirs-first t)
 '(mail-source-delete-incoming 30)
 '(mark-even-if-inactive nil)
 '(mark-ring-max 1024)
 '(markdown-fontify-code-blocks-natively t)
 '(max-image-size 32.0 t)
 '(message-log-max t)
 '(message-send-mail-function 'smtpmail-send-it)
 '(minibuffer-default-prompt-format " [%s]")
 '(minibuffer-depth-indicate-mode t)
 '(minibuffer-visible-completions t)
 '(mm-discouraged-alternatives nil)
 '(mm-text-html-renderer 'shr)
 '(mmm-submode-decoration-level 0)
 '(mode-line-percent-position '(6 "%q"))
 '(mouse-avoidance-banish-position
   '((frame-or-window . frame)
     (side . right)
     (side-pos . -3)
     (top-or-bottom . bottom)
     (top-or-bottom-pos . -1)))
 '(mouse-avoidance-mode 'banish nil (avoid))
 '(mouse-highlight 1)
 '(mouse-prefer-closest-glyph t)
 '(mouse-select-region-move-to-beginning t)
 '(mouse-wheel-scroll-amount-horizontal 5)
 '(multi-isearch-pause nil)
 '(next-error-message-highlight 'keep)
 '(nnmail-crosspost-link-function 'copy-file)
 '(nnmail-expiry-wait 30)
 '(nnmail-extra-headers '(To Cc))
 '(nnmail-treat-duplicates 'delete)
 '(org-agenda-include-diary t)
 '(org-babel-results-keyword "results")
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(org-fontify-whole-heading-line t)
 '(org-startup-with-inline-images t)
 '(org-todo-keywords '((sequence "TODO" "|" "DONE" "NONEED")))
 '(outline-minor-mode-use-buttons 'in-margins)
 '(parens-require-spaces nil)
 '(perl-indent-level 2)
 '(perl-prettify-symbols nil)
 '(pp-default-function 'pp-29)
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(proced-descend nil)
 '(proced-filter 'all)
 '(proced-format 'medium)
 '(proced-sort 'pid)
 '(project-file-history-behavior 'relativize)
 '(project-kill-buffer-conditions '(buffer-file-name))
 '(project-kill-buffers-display-buffer-list t)
 '(project-mode-line t)
 '(project-prompter 'project-prompt-project-dir)
 '(project-switch-commands 'project-prefix-or-any-command)
 '(rainbow-delimiters-max-face-count 5)
 '(read-char-by-name-sort 'code)
 '(read-file-name-completion-ignore-case t)
 '(read-minibuffer-restore-windows nil)
 '(read-quoted-char-radix 16)
 '(recentf-exclude '("/[^/]+[@:]"))
 '(repeat-keep-prefix t)
 '(require-final-newline t)
 '(revert-buffer-quick-short-answers t)
 '(ruby-insert-encoding-magic-comment nil)
 '(save-interprogram-paste-before-kill t)
 '(save-place-limit nil)
 '(save-place-mode t)
 '(save-place-save-skipped nil)
 '(scroll-conservatively 10)
 '(scroll-error-top-bottom t)
 '(scroll-preserve-screen-position t)
 '(select-active-regions nil)
 '(send-mail-function 'smtpmail-send-it)
 '(set-mark-command-repeat-pop t)
 '(sgml-xml-mode t)
 '(shell-command-guess-functions
   '(shell-command-guess-open shell-command-guess-mailcap shell-command-guess-xdg shell-command-guess-dired-default shell-command-guess-dired-user))
 '(shift-select-mode 'permanent)
 '(show-paren-mode nil)
 '(shr-use-fonts nil)
 '(smiley-style 'emoji)
 '(smtpmail-debug-info t)
 '(smtpmail-queue-mail t)
 '(split-height-threshold 100)
 '(sql-sqlite-login-params '((database :file ".*")))
 '(sql-sqlite-program "sqlite3")
 '(suggest-key-bindings nil)
 '(switch-to-buffer-obey-display-actions t)
 '(switch-to-buffer-preserve-window-point nil)
 '(tab-line-tab-face-functions nil)
 '(tar-mode-show-date t)
 '(temp-buffer-resize-mode t)
 '(thumbs-per-line 65536)
 '(thumbs-relief 0)
 '(tool-bar-style 'both-horiz)
 '(track-eol t)
 '(tramp-allow-unsafe-temporary-files t)
 '(truncate-lines t)
 '(vc-annotate-background nil)
 '(vc-bzr-diff-switches "-F ^(")
 '(vc-bzr-status-switches nil)
 '(vc-command-messages 'log)
 '(vc-deduce-backend-nonvc-modes t)
 '(vc-display-status 'no-backend)
 '(vc-find-revision-no-save t)
 '(vc-git-diff-switches '("--diff-algorithm=patience"))
 '(vc-git-log-switches '("--pretty=fuller"))
 '(vc-git-revision-complete-only-branches t)
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
 '(what-cursor-show-names t)
 '(woman-use-own-frame nil)
 '(xref-file-name-display 'project-relative)
 '(xref-history-storage 'xref-window-local-history)
 '(xref-search-program 'ripgrep)
 '(xref-search-program-alist
   '((ripgrep . "xargs -0 rg <C> --null -nH --sort path --no-heading --null --no-messages -g '!*/' -e <R>")))
 '(xref-show-definitions-function 'xref--show-defs-minibuffer)
 '(xref-truncation-width 200)
 '(yank-excluded-properties t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-menu-buffer ((t (:foreground "black"))))
 '(button ((t (:inherit link :underline t))))
 '(c-annotation-face ((t (:inherit font-lock-comment-face))))
 '(comint-highlight-input ((t (:background "gray94"))))
 '(completions-common-part ((t (:weight bold))))
 '(completions-first-difference ((t (:underline t))))
 '(cperl-array ((t (:foreground "blue"))))
 '(cperl-hash ((t (:foreground "blue"))))
 '(cperl-nonoverridable ((((class color) (background light)) (:foreground "blue"))))
 '(custom-group-tag ((t (:foreground "brown"))))
 '(custom-variable-tag ((t (:foreground "brown"))))
 '(cvs-handled ((((class color) (background light)) (:foreground "slate gray"))))
 '(cvs-header ((t (:foreground "SteelBlue"))))
 '(cvs-marked ((t (:foreground "DarkRed"))))
 '(cvs-need-action ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(diary ((((class color)) (:foreground "blue"))))
 '(dictionary-word-definition-face ((t nil)))
 '(dictionary-word-entry-face ((t (:foreground "darkblue" :weight bold))))
 '(diff-hl-change ((t (:inherit diff-refine-changed))))
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "#d4ffd4"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "#ffd4d4"))))
 '(dired-directory ((t (:inherit link :underline nil))))
 '(dired-special ((t (:foreground "Sienna"))))
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
 '(gnus-header ((t nil)))
 '(gnus-summary-cancelled ((t (:inherit shadow))))
 '(gnus-summary-high-ancient ((t (:foreground "RoyalBlue"))))
 '(gnus-summary-high-ticked ((t (:foreground "firebrick" :weight normal))))
 '(gnus-summary-high-unread ((t (:foreground "blue"))))
 '(gnus-summary-low-read ((t (:inherit shadow :extend t))))
 '(gnus-summary-normal-ancient ((t (:foreground "gray30"))))
 '(help-argument-name ((t (:foreground "SkyBlue4"))))
 '(hl-line ((t (:extend t :background "AntiqueWhite1"))))
 '(holiday ((((class color)) (:background "tan"))))
 '(html-tag-face ((t (:foreground "blue"))))
 '(info-xref ((t (:inherit link :underline t))))
 '(isearch ((((class color) (background light)) (:background "magenta4" :foreground "lightskyblue1")) (((class color) (background dark)) (:background "palevioletred2" :foreground "brown4"))))
 '(isearch-group-2 ((t (:background "magenta3" :foreground "lightskyblue1"))))
 '(link ((t (:foreground "medium blue"))))
 '(log-view-commit-body ((t (:extend t :background "gray98"))))
 '(match ((t (:background "#ffff88"))))
 '(message-cited-text-1 ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(mm-uu-extract ((t (:extend t :background "grey98"))))
 '(mode-line-buffer-id ((t nil)))
 '(next-error-message ((t (:extend t :background "#ffffbb"))))
 '(org-agenda-diary ((t nil)))
 '(org-agenda-structure ((t (:background "#ddeeff"))))
 '(org-block ((t nil)))
 '(org-block-begin-line ((t (:inherit org-meta-line :extend t :background "grey95"))))
 '(org-code ((t (:background "grey95"))))
 '(org-link ((t (:inherit link :underline t))))
 '(org-scheduled ((t (:foreground "Blue1"))))
 '(org-scheduled-today ((t (:foreground "Blue1"))))
 '(org-verbatim ((t (:background "grey95"))))
 '(pulse-highlight-start-face ((t (:extend t :background "#FF0000"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "Purple"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "Blue1"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "Red1"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "gold1"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "green2"))))
 '(region ((((class color) (background light)) (:background "gray90" :extend t))))
 '(sh-heredoc ((((class color) (background light)) (:foreground "ForestGreen"))))
 '(show-paren-match ((((class color)) (:background "gainsboro"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "pink"))))
 '(variable-pitch ((t (:family "DejaVu Sans"))))
 '(variable-pitch-text ((t (:inherit variable-pitch))))
 '(vc-conflict-state ((t (:inherit vc-state-base :foreground "red"))))
 '(vc-edited-state ((t (:inherit vc-state-base))))
 '(vc-locally-added-state ((t (:inherit vc-state-base :foreground "forestgreen"))))
 '(vc-locked-state ((t (:inherit vc-state-base :foreground "red"))))
 '(vc-missing-state ((t (:inherit vc-state-base :foreground "red"))))
 '(vc-needs-update-state ((t (:inherit vc-state-base :foreground "red"))))
 '(vc-removed-state ((t (:inherit vc-state-base :foreground "red"))))
 '(vc-state-base ((t nil)))
 '(w3m-arrived-anchor-face ((((class color) (background light)) (:foreground "DarkMagenta"))) t)
 '(w3m-current-anchor-face ((t (:foreground "red"))) t)
 '(widget-button ((t nil)))
 '(xref-file-header ((t (:extend t :background "grey90"))))
 '(xref-line-number ((t (:inherit shadow)))))
