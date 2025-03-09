;;; resumel.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Barry Ridge
;;
;; Author: Barry Ridge <barry@barr.ai>
;; Maintainer: Barry Ridge <barry@barr.ai>
;; Created: January 04, 2025
;; Modified: January 04, 2025
;; Version: 0.0.1
;; Keywords: bib convenience docs tex wp
;; Homepage: https://github.com/barryridge/resumel
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;
(require 'org)           ;; Org
(require 'ox-latex)      ;; Org LaTeX Export
(require 'ox-extra)      ;; For ignore-headlines
(require 'subr-x)        ;; For string-trim

(defun expand-cvtags (&rest strings)
  "Return a string of \\cvtag{...} expansions from each argument in STRINGS.
Ignores nil or empty entries."
  ;; Remove any nil arguments
  (setq strings (delete nil strings))
  ;; Trim leading/trailing whitespace
  (setq strings (mapcar #'string-trim strings))
  ;; Remove empty strings
  (setq strings (delete "" strings))
  ;; Now build the final string
  (mapconcat (lambda (skill)
               (format "\\cvtag{%s}" skill))
             strings
             " "))

(defun expand-cvltags (&rest strings)
  "Return a string of \\cvtag{Skill}[Level] expansions for each 'Skill,Level' argument pair in STRINGS."
  (let (result)
    ;; While we have at least 2 arguments left...
    (while (>= (length strings) 2)
      (let* ((skill (pop strings))   ;; pop the first
             (level (pop strings)))  ;; pop the second
        (when (and (stringp skill) (stringp level))
          (setq skill (string-trim skill))
          (setq level (string-trim level))
          ;; Build the final
          (push (format "\\cvtag{%s}[%s]" skill level) result))))
    (string-join (nreverse result) " ")))

(defun expand-wheelchart (&rest args)
  "Generate LaTeX wheelchart command from ARGS (outer-radius inner-radius value/text-width/color/detail...)"
  (unless (>= (length args) 2)
    (error "wheelchart requires at least outer and inner radius"))
  (let* ((outer (pop args))
         (inner (pop args))
         (segments (cl-loop while (>= (length args) 4)
                            for a = (pop args)
                            for b = (pop args)
                            for c = (pop args)
                            for d = (pop args)
                            when (and a b c d)
                            collect (list a b c d))))
    (concat "@@latex:\\wheelchart{" outer "}{" inner "}{%\n"
            (mapconcat (lambda (s)
                         (format "  %s/%s/%s/{%s}"
                                 (nth 0 s)
                                 (nth 1 s)
                                 (nth 2 s)
                                 (replace-regexp-in-string
                                  "\\\\," "," (nth 3 s) t t)))
                       segments
                       ",\n")
            "\n}@@")))

(defgroup resumel nil
  "Customization group for resumel."
  :group 'convenience
  :prefix "resumel-")

(defcustom resumel-default-template "moderncv"
  "Default resume template to use."
  :type '(choice (const "moderncv") (const "altacv") (const "modaltacv"))
  :group 'resumel)

;; Set the directory where resumel.el is located
(defvar resumel-base-dir
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Base directory for resumel package files.")

;; Set the directory where resumel templates are stored
(defvar resumel-templates-dir
  (expand-file-name "templates" resumel-base-dir)
  "Directory where resumel templates are stored.")

;; Load a resumel template
(defun resumel--load-template (template) "Load the specified TEMPLATE from `resumel-templates-dir`."
  (let* ((base-org (expand-file-name "resumel.org" resumel-base-dir))
         (template-dir (expand-file-name template resumel-templates-dir))
         (template-el (expand-file-name (format "%s.el" template) template-dir))
         (template-org (expand-file-name (format "%s.org" template) template-dir)))
    ;; Check if central macros file exists
    (unless (file-exists-p base-org)
      (error "Base macros file not found: %s" base-org))
    ;; Load central macros
    (org-babel-load-file base-org)
    ;; Check if template files exist
    (unless (file-exists-p template-el)
      (error "Template Emacs Lisp file not found: %s" template-el))
    (unless (file-exists-p template-org)
      (error "Template Org file not found: %s" template-org))
    ;; Load the template Emacs Lisp file
    (load-file template-el)
    ;; Load the template Org macros
    (org-babel-load-file template-org)))

;;;###autoload
(defun resumel-select-template (template)
  "Select a resume TEMPLATE to use for exports."
  (interactive
   (list (completing-read "Select template: " '("moderncv" "altacv" "modaltacv") nil t)))
  (setq resumel-default-template template)
  (message "resumel template set to: %s" template))

(defvar resumel-selected-template resumel-default-template
  "Currently selected resume template.")

(defun resumel-setup ()
  "Set up resumel with the selected template."
  (interactive)
  ;; Initialize template variable
  (let ((template nil))
    ;; Parse the Org buffer and search for RESUMEL_TEMPLATE keyword
    (org-element-map (org-element-parse-buffer 'element) 'keyword
      (lambda (el)
        (when (string= (org-element-property :key el) "RESUMEL_TEMPLATE")
          (setq template (org-element-property :value el)))))
    ;; If the keyword is found, use it; otherwise, use the default
    (setq resumel-selected-template (or template resumel-default-template))
    ;; Activate necessary org-export extras
    (ox-extras-activate '(latex-header-blocks ignore-headlines))
    ;; Load the selected template
    (resumel--load-template resumel-selected-template)
    ;; Ensure resumel-template-class is defined
    (unless resumel-template-class
      (error "resumel-template-class is not defined in template %s" resumel-selected-template))
    ;; Set org-latex-default-class in the current buffer
    (setq-local org-latex-default-class resumel-template-class)
    (message "resumel setup complete with template: %s" resumel-selected-template)))

(defun resumel-setup ()
  "Set up resumel with the selected template."
  (interactive)
  ;; Read the RESUMEL_TEMPLATE keyword from the buffer
  (let ((template (save-excursion
                    (goto-char (point-min))
                    (if (re-search-forward "^#\\+RESUMEL_TEMPLATE:[ \t]+\\(.*\\)$" nil t)
                        (match-string 1)
                      nil))))
    ;; If the keyword is found, use it; otherwise, use the default
    (setq resumel-selected-template (or template resumel-default-template))
    ;; Activate necessary org-export extras
    (ox-extras-activate '(latex-header-blocks ignore-headlines))
    ;; Load the selected template
    (resumel--load-template resumel-selected-template)
    ;; Ensure resumel-template-class is defined
    (unless (boundp 'resumel-template-class)
      (error "resumel-template-class is not defined in template %s" resumel-selected-template))
    ;; Set org-latex-default-class in the current buffer
    (setq-local org-latex-default-class resumel-template-class)
    (message "resumel setup complete with template: %s" resumel-selected-template)))

;;;###autoload
(defun resumel-export ()
  "Export the current Org buffer to PDF using the selected resumel template."
  (interactive)
  ;; Ensure we're in an Org buffer
  (unless (derived-mode-p 'org-mode)
    (error "resumel-export must be called from an Org buffer"))
  ;; Setup resumel
  (resumel-setup)
  ;; Export to PDF
  (org-latex-export-to-pdf))

(provide 'resumel)

;;; resumel.el ends here
