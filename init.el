(org-babel-load-file (locate-file "README.org" load-path))

;; TODO: later move all that is below to README.org
;; TODO: qv (info "(use-package) after")

(require 'package)
(setq package-enable-at-startup nil)
(unless (assoc "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless package--initialized
  (package-initialize))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
