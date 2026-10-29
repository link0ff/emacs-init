;;; jeanne-darc-theme.el --- Jeanne d'Arc theme   -*- lexical-binding: t -*-

(deftheme jeanne-darc
  "Jeanne d'Arc theme."
  :family 'jeanne
  :kind 'color-scheme
  :background-mode 'dark)

(custom-theme-set-faces
 'jeanne-darc
 '(default ((t (:background "black" :foreground "grey90" :height 169))))
 '(dictionary-reference-face ((t (:inherit link))))
 '(dictionary-word-definition-face ((t nil)))
 '(diff-added ((t (:inherit diff-changed :extend t :background "#002200"))))
 '(diff-file-header ((t (:extend t :background "grey40" :weight bold))))
 '(diff-header ((t (:extend t :background "grey25"))))
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "#005500"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "#660000"))))
 '(diff-removed ((t (:inherit diff-changed :extend t :background "#331111"))))
 '(escape-glyph ((t (:foreground "magenta3"))))
 ;; '(font-lock-builtin-face ((t (:foreground "Sienna3"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 ;; '(font-lock-function-name-face ((t (:foreground "DeepSkyBlue"))))
 '(font-lock-string-face ((t (:foreground "LimeGreen"))))
 ;; '(font-lock-type-face ((t (:foreground "SteelBlue"))))
 ;; '(font-lock-variable-name-face ((t (:foreground "SteelBlue"))))
 ;; '(fringe ((t (:background "black" :foreground "grey96"))))
 '(glyphless-char ((t (:background "red"))))
 '(hi-yellow ((t (:background "yellow3" :foreground "black"))))
 '(isearch ((t (:background "magenta4" :foreground "lightskyblue1"))))
 '(nobreak-hyphen ((t (:foreground "red"))))
 '(org-block ((t nil)))
 '(org-block-begin-line ((t (:inherit org-meta-line :extend t :background "grey25"))))
 '(region ((t (:background "gray20")))))

(provide-theme 'jeanne-darc)

;;; jeanne-darc-theme.el ends here
