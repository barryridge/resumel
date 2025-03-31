(setq user-emacs-directory (expand-file-name "./tests/test_emacs.d/"))

;; Install packages
(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(use-package org :ensure t)
(use-package org-contrib :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)))

;; Load ox-extra for additional export options
(require 'ox-extra)
(ox-extras-activate '(latex-header-blocks ignore-headlines))

;; Add current directory to load-path for resumel.el and templates
(add-to-list 'load-path (expand-file-name "./"))

;; Require resumel
(require 'resumel)
