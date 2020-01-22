
(defun load-literate-org (file)
  "Load literate org config."
  (with-temp-buffer
    ;; Set ‘buffer-file-name’ to help *Help* buffer point to the right source file.
    (setq buffer-file-name file)
    (insert-file-contents file)
    (while (re-search-forward (rx (*? anything)
                                  "#+begin_src emacs-lisp"
                                  (group-n 1 (*? anything))
                                  "#+end_src")
                              nil t)
      (replace-match (match-string 1) nil nil))
    (delete-region (point) (point-max))
    (set-buffer-modified-p nil)
    (setq lexical-binding t)
    (eval-buffer)))

(load-literate-org "README.org")
