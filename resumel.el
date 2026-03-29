;;; resumel.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Barry Ridge
;;
;; Author: Barry Ridge <barry@barr.ai>
;; Maintainer: Barry Ridge <barry@barr.ai>
;; Created: January 04, 2025
;; Modified: March 15, 2025
;; Version: 0.0.1
;; Keywords: convenience docs tex wp
;; Homepage: https://github.com/barryridge/resumel
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  resumel is an Emacs package for creating professional resumes and CVs using Org Mode and LaTeX templates.
;;
;;; Code:
;;;
(require 'org)           ;; Org
(require 'ox-latex)      ;; Org LaTeX Export
(require 'ox-extra)      ;; For ignore-headlines
(require 'subr-x)        ;; For string-trim

(defun resumel-expand-cvtags (&rest strings)
  "Return a string of \\cvtag{...} expansions from each argument in
STRINGS (skill skill skill...).  Ignores nil or empty entries."
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

(defun resumel-expand-cvltags (&rest strings)
  "Return a string of \\cvtag{Skill}[Level] expansions for each (skill level)
argument pair in STRINGS (skill level skill level...)."
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

(defun resumel-expand-wheelchart (&rest args)
  "Generate LaTeX wheelchart command from ARGS (outer-radius inner-radius value
text-width color detail value text-width color detail...)."
  (unless (>= (length args) 2)
    (error "Resumel: resumel-expand-wheelchart requires at least outer and inner radius"))
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
  "Default resumel template to use."
  :type '(choice (const "moderncv") (const "altacv") (const "modaltacv") (const "awesomecv"))
  :group 'resumel)

;; Buffer-local alist of RESUMEL_* keyword values parsed from the Org file.
;; Declared as defvar so template .el files can read it via dynamic binding.
(defvar resumel-template-vars nil
  "Alist of (VAR-NAME . value) pairs parsed from RESUMEL_* Org keywords.
Set during `resumel-setup'.  Template .el files read this to resolve their
configuration variables.")

;; Set the directory where resumel.el is located
(defvar resumel-base-dir
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Base directory for resumel package files.")

;; Set the directory where resumel templates are stored
(defvar resumel-templates-dir
  (expand-file-name "templates" resumel-base-dir)
  "Directory where resumel templates are stored.")

;; Helper function for including template Org file
(defun resumel-insert-template-include ()
  "Insert #+INCLUDE directive for the selected template's org file."
  (let* ((template resumel-selected-template)
         (template-dir (expand-file-name template resumel-templates-dir))
         (template-org (expand-file-name (format "%s.org" template) template-dir))
         (include-line (format "#+INCLUDE: \"%s\"\n" template-org)))
    (unless (file-exists-p template-org)
      (error "Template Org file not found: %s" template-org))
    (save-excursion
      (goto-char (point-min))
      ;; Check if the INCLUDE already exists to avoid duplication
      (unless (re-search-forward (format "^#\\+INCLUDE: \"%s\"" template-org) nil t)
        ;; Insert after the initial configuration block if present
        (if (re-search-forward "^\\* Config" nil t)
            (progn
              (goto-char (line-end-position))
              (insert "\n" include-line))
          ;; Otherwise, insert at the beginning
          (goto-char (point-min))
          (insert include-line))))))

;; Remove existing template entries from org­latex­classes
(defun resumel--remove-existing-template (template)
  "Remove any existing entry from `org-latex-classes` that has the key TEMPLATE."
  (setq org-latex-classes
        (seq-remove (lambda (entry)
                      (string= (car entry) template))
                    org-latex-classes)))

;; Load a resumel template
(defun resumel--load-template (template) "Load the specified TEMPLATE from `resumel-templates-dir`."
  (let* ((template-dir (expand-file-name template resumel-templates-dir))
         (template-el (expand-file-name (format "%s.el" template) template-dir)))
    ;; Check if template .el files exist
    (unless (file-exists-p template-el)
      (error "Template Emacs Lisp file not found: %s" template-el))
    ;; Remove any existing entry from org-latex-classes
    (resumel--remove-existing-template (format "resumel-%s" template))
    ;; Load the template .el file
    (load-file template-el)))

;;;###autoload
(defun resumel-select-template (template)
  "Select a resumel TEMPLATE to use for exports."
  (interactive
   (list (completing-read "Select template: " '("moderncv" "altacv" "modaltacv" "awesomecv") nil t)))
  (setq resumel-default-template template)
  (message "resumel template set to: %s" template))

(defvar-local resumel-selected-template resumel-default-template
  "Currently selected resumel template.")

(defun resumel-setup ()
  "Set up resumel with the selected template."
  (interactive)
  (let ((template nil)
        (vars '()))
    ;; Parse the Org buffer and collect RESUMEL_* keywords
    (let ((parsed (org-element-parse-buffer)))
      (org-element-map parsed 'keyword
        (lambda (el)
          (let ((key (org-element-property :key el))
                (value (org-element-property :value el)))
            (when (and key (string-prefix-p "RESUMEL_" key))
              (if (string= key "RESUMEL_TEMPLATE")
                  (setq template value)
                ;; Remove 'RESUMEL_' prefix and store the variable
                (push (cons (substring key (length "RESUMEL_")) (string-trim value)) vars)))))))
    ;; Set the selected template, defaulting if necessary
    (setq resumel-selected-template (or template resumel-default-template))
    ;; Store variables in a buffer-local variable
    (setq-local resumel-template-vars (nreverse vars))
    ;; Activate necessary org-export extras
    (ox-extras-activate '(latex-header-blocks ignore-headlines))
    ;; Load the selected template
    (resumel--load-template resumel-selected-template)
    ;; Insert the #+INCLUDE directive for the template's .org file
    (resumel-insert-template-include)
    ;; Set org-latex-default-class in the current buffer
    (setq-local org-latex-default-class (format "resumel-%s" resumel-selected-template))
    (message "resumel setup complete with template: %s" resumel-selected-template)))

;;;###autoload
(defun resumel-export ()
  "Export the current Org buffer to PDF using the selected resumel template."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Resumel export must be called from an Org buffer"))
  (let ((orig-buf (current-buffer)))
    ;; Create a new temporary buffer and insert a copy of the original content
    (with-temp-buffer
      (insert-buffer-substring orig-buf)
      ;; Switch to Org-mode in the temporary buffer
      (org-mode)
      ;; Set up resumel in the temporary buffer
      (resumel-setup)
      ;; Export to PDF
      (org-latex-export-to-pdf))))

;;; Template variable introspection

(defun resumel--get-buffer-template ()
  "Return the template selected in the current buffer, or the global default."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+RESUMEL_TEMPLATE: *\\(.*\\)$" nil t)
        (string-trim (match-string 1))
      resumel-default-template)))

(defun resumel--get-buffer-vars ()
  "Parse all RESUMEL_* keywords from the current buffer.
Returns an alist of (VAR-NAME . value), excluding RESUMEL_TEMPLATE."
  (let (vars)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+RESUMEL_\\([^:[:space:]]+\\): *\\(.*\\)$" nil t)
        (let ((key (string-trim (match-string 1)))
              (val (string-trim (match-string 2))))
          (unless (string= key "TEMPLATE")
            (push (cons key val) vars)))))
    (nreverse vars)))

(defun resumel--get-template-defaults (template)
  "Return the default variables alist for TEMPLATE.
Loads the template .el file if the defaults are not yet available."
  (let* ((defaults-sym (intern (format "resumel-%s-variable-defaults" template)))
         (template-dir (expand-file-name template resumel-templates-dir))
         (template-el (expand-file-name (format "%s.el" template) template-dir)))
    (unless (boundp defaults-sym)
      (unless (file-exists-p template-el)
        (error "Template file not found: %s" template-el))
      ;; Load with empty vars so the let* in the template file doesn't error
      (let ((resumel-template-vars '()))
        (load-file template-el)))
    (if (boundp defaults-sym)
        (symbol-value defaults-sym)
      nil)))

;;;###autoload
(defun resumel-get-template-variable (var)
  "Return the effective value of template variable VAR.
Checks the current buffer's #+RESUMEL_VAR keywords first, then falls
back to the selected template's compiled-in defaults.

When called interactively, prompts with completion over known variables
and displays the result in the minibuffer."
  (interactive
   (let* ((template (resumel--get-buffer-template))
          (defaults (resumel--get-template-defaults template))
          (buf-vars (resumel--get-buffer-vars))
          (names (delete-dups
                  (append (mapcar #'car defaults)
                          (mapcar #'car buf-vars)))))
     (list (completing-read "Variable: " names nil nil))))
  (let* ((template (resumel--get-buffer-template))
         (buf-vars (resumel--get-buffer-vars))
         (defaults (resumel--get-template-defaults template))
         (buf-val  (cdr (assoc var buf-vars)))
         (def-val  (cdr (assoc var defaults)))
         (value    (or buf-val def-val)))
    (when (called-interactively-p 'interactive)
      (message "#+RESUMEL_%s = %s  [%s]"
               var
               (or value "(not set)")
               (cond (buf-val  "set in buffer")
                     (def-val  "template default")
                     (t        "unknown variable"))))
    value))

;;;###autoload
(defun resumel-set-template-variable (var value)
  "Set template variable VAR to VALUE in the current Org buffer header.
Inserts or updates the #+RESUMEL_VAR: value keyword.  Does not modify
the template source files — the change applies only to this buffer."
  (interactive
   (let* ((template (resumel--get-buffer-template))
          (defaults (resumel--get-template-defaults template))
          (buf-vars (resumel--get-buffer-vars))
          (names    (delete-dups
                     (append (mapcar #'car defaults)
                             (mapcar #'car buf-vars))))
          (v        (completing-read "Variable: " names nil nil))
          (current  (resumel-get-template-variable v))
          (val      (read-string
                     (format "#+RESUMEL_%s (current: %s): " v (or current "not set"))
                     current)))
     (list v val)))
  (unless (derived-mode-p 'org-mode)
    (error "resumel-set-template-variable must be called from an Org buffer"))
  (let ((keyword    (format "#+RESUMEL_%s" var))
        (search-re  (format "^#\\+RESUMEL_%s:" (regexp-quote var))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward search-re nil t)
          ;; Update the existing keyword line
          (progn
            (beginning-of-line)
            (kill-line)
            (insert (format "%s: %s" keyword value)))
        ;; Insert a new keyword line
        (goto-char (point-min))
        (if (re-search-forward "^#\\+RESUMEL_" nil t)
            ;; Append after the last existing RESUMEL_ keyword
            (let (last-pos)
              (goto-char (point-min))
              (while (re-search-forward "^#\\+RESUMEL_" nil t)
                (setq last-pos (line-end-position)))
              (goto-char last-pos)
              (insert (format "\n%s: %s" keyword value)))
          ;; No RESUMEL_ keywords yet — insert after the last top-level keyword
          (goto-char (point-min))
          (let (last-kw-end)
            (while (looking-at "^#\\+")
              (setq last-kw-end (line-end-position))
              (forward-line 1))
            (if last-kw-end
                (progn (goto-char last-kw-end)
                       (insert (format "\n%s: %s" keyword value)))
              (insert (format "%s: %s\n" keyword value)))))))))

;;;###autoload
(defun resumel-show-all-template-variables ()
  "Display all template variables and their effective values in a help buffer.
For each variable, shows whether its value comes from the current buffer
or from the selected template's defaults."
  (interactive)
  (let* ((template  (resumel--get-buffer-template))
         (defaults  (resumel--get-template-defaults template))
         (buf-vars  (resumel--get-buffer-vars))
         (all-names (delete-dups
                     (append (mapcar #'car defaults)
                             (mapcar #'car buf-vars))))
         (buf (get-buffer-create "*resumel: template variables*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Resumel template variables  [template: %s]\n" template))
        (insert (make-string 70 ?=) "\n\n")
        (insert (format "  %-42s %-9s  %s\n" "KEYWORD" "SOURCE" "EFFECTIVE VALUE"))
        (insert "  " (make-string 68 ?-) "\n")
        (dolist (name all-names)
          (let* ((bval    (cdr (assoc name buf-vars)))
                 (dval    (cdr (assoc name defaults)))
                 (src     (if bval "buffer" "default"))
                 (display (or bval dval "(not set)")))
            (insert (format "  #+RESUMEL_%-32s [%-7s]  %s\n"
                            name src display))))
        (insert "\n")
        (insert "Use M-x resumel-set-template-variable to override a value in this buffer.\n")
        (insert "Use M-x resumel-get-template-variable to query a single variable.\n"))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

(provide 'resumel)
;;; resumel.el ends here
