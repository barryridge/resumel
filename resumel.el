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
  "Select a resumel TEMPLATE to use for exports.
Sets `resumel-default-template' globally.  When called from an Org
buffer, also inserts or updates #+RESUMEL_TEMPLATE: in the file header."
  (interactive
   (list (completing-read "Select template: " '("moderncv" "altacv" "modaltacv" "awesomecv") nil t)))
  (setq resumel-default-template template)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+RESUMEL_TEMPLATE:" nil t)
          (progn
            (beginning-of-line)
            (kill-line)
            (insert (format "#+RESUMEL_TEMPLATE: %s" template)))
        (goto-char (point-min))
        (insert (format "#+RESUMEL_TEMPLATE: %s\n" template)))))
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

(defconst resumel-core-variable-names
  '("COMPILER" "GEOMETRY" "DOCUMENTCLASS_OPTIONS"
    "MAIN_FONT_XELATEX" "SANS_FONT_XELATEX" "MONO_FONT_XELATEX" "MATH_FONT_XELATEX"
    "MAIN_FONT_PDFLATEX" "SANS_FONT_PDFLATEX" "MONO_FONT_PDFLATEX" "MATH_FONT_PDFLATEX"
    "TITLE_FONT" "AUTHOR_FONT" "SECTION_FONT" "SUBSECTION_FONT"
    "CVTAG_INTENSITY_DEFAULT" "CVTAG_FONT_DEFAULT" "CVTAG_BASELINE_DEFAULT"
    "CVTAG_INNER_X_SEP_DEFAULT" "CVTAG_INNER_Y_SEP_DEFAULT"
    "CVTAG_TEXT_HEIGHT_DEFAULT" "CVTAG_TEXT_DEPTH_DEFAULT" "CVTAG_CORNER_DEFAULT")
  "Variable names common to all resumel templates.")

;; ---------------------------------------------------------------------------
;; Private helpers

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
         (template-el  (expand-file-name (format "%s.el" template) template-dir)))
    (unless (boundp defaults-sym)
      (unless (file-exists-p template-el)
        (error "Template file not found: %s" template-el))
      (let ((resumel-template-vars '()))
        (load-file template-el)))
    (if (boundp defaults-sym)
        (symbol-value defaults-sym)
      nil)))

;; ---------------------------------------------------------------------------
;; Variables display buffer (resumel-variables-mode)

(defvar-local resumel-variables--source-buffer nil
  "The Org buffer this variables display buffer is associated with.")

(defvar-local resumel-variables--show-fn nil
  "The function used to populate this variables buffer (for 'g' refresh).")

(defvar-local resumel-variables--populating nil
  "Non-nil while the buffer is being programmatically populated.
Suppresses live-sync so that displaying default values does not write
them into the Org buffer.")

(defface resumel-variables-active-face
  '((t (:weight bold :slant italic :foreground "#2080c0")))
  "Face for variables that are explicitly set in the current Org buffer.
Variables shown in this face override the template default.
Uses bold, italic, and colour so the distinction is visible in any theme."
  :group 'resumel)

(defun resumel--variables-after-change (beg _end _len)
  "Sync an edited variable line back to the source Org buffer."
  (unless resumel-variables--populating
    (when (and resumel-variables--source-buffer
               (buffer-live-p resumel-variables--source-buffer))
      (save-excursion
        (goto-char beg)
        (beginning-of-line)
        (when (looking-at "^[ \t]*#\\+RESUMEL_\\([^:[:space:]]+\\): *\\(.*\\)$")
          (let* ((var      (string-trim (match-string 1)))
                 (val      (string-trim (match-string 2)))
                 (line-beg (line-beginning-position))
                 (line-end (line-end-position))
                 (src-buf  resumel-variables--source-buffer)
                 (default  (with-current-buffer src-buf
                             (cdr (assoc var (resumel--get-template-defaults
                                             (resumel--get-buffer-template)))))))
            ;; Update face immediately — this is safe in the vars buffer.
            (if (and default (string= val default))
                (remove-text-properties line-beg line-end '(face nil))
              (add-text-properties line-beg line-end
                                   '(face resumel-variables-active-face)))
            ;; Emacs sets inhibit-modification-hooks = t while running
            ;; after-change-functions, which would suppress org-indent-mode's
            ;; own after-change hook when we write to the Org buffer.
            ;; Binding it to nil here re-enables those hooks so org-mode
            ;; (including org-indent-mode) processes our insertion normally.
            (let ((inhibit-modification-hooks nil))
              (with-current-buffer src-buf
                (resumel-set-template-variable var val)))))))))

(defvar resumel-variables-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'resumel--variables-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `resumel-variables-mode'.")

(define-derived-mode resumel-variables-mode nil "Resumel-Vars"
  "Major mode for viewing and live-editing resumel template variables.
Variable values can be edited directly on their #+RESUMEL_VAR: lines;
changes are immediately synced to the associated Org buffer.

\\{resumel-variables-mode-map}"
  :group 'resumel
  (add-hook 'after-change-functions #'resumel--variables-after-change nil t))

(defun resumel--variables-refresh ()
  "Refresh the current variables buffer by re-running its show function."
  (interactive)
  (when resumel-variables--show-fn
    (let ((src resumel-variables--source-buffer))
      (if (buffer-live-p src)
          (with-current-buffer src
            (funcall resumel-variables--show-fn))
        (message "Source Org buffer no longer exists.")))))

(defun resumel--populate-variables-buffer (buf org-buf template show-fn filter
                                               &optional set-only)
  "Populate the variables display BUF for ORG-BUF and TEMPLATE.
SHOW-FN is stored for refresh.  FILTER is one of: \\='all, \\='core,
\\='template-specific.  When SET-ONLY is non-nil, only variables
explicitly set in the Org buffer are displayed."
  (let* ((defaults  (resumel--get-template-defaults template))
         (buf-vars  (with-current-buffer org-buf (resumel--get-buffer-vars)))
         (core-names (seq-filter
                      (lambda (n) (member n resumel-core-variable-names))
                      (mapcar #'car defaults)))
         (tmpl-names (seq-filter
                      (lambda (n) (not (member n resumel-core-variable-names)))
                      (delete-dups
                       (append (mapcar #'car defaults)
                               (mapcar #'car buf-vars))))))
    ;; In set-only mode restrict each list to variables present in the buffer.
    (when set-only
      (setq core-names (seq-filter (lambda (n) (assoc n buf-vars)) core-names))
      (setq tmpl-names (seq-filter (lambda (n) (assoc n buf-vars)) tmpl-names)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (saved-pt (point)))
        (unless (resumel-variables-mode-p)
          (resumel-variables-mode))
        (setq resumel-variables--source-buffer org-buf)
        (setq resumel-variables--show-fn show-fn)
        (setq resumel-variables--populating t)
        (unwind-protect
            (progn
              (erase-buffer)
              (insert (format "Resumel template variables  [template: %s]\n" template))
              (insert (make-string 70 ?=) "\n")
              (insert "\nEdit #+RESUMEL_* values directly — ")
              (insert "changes sync live to your Org buffer.\n")
              (if set-only
                  (insert "Showing only variables currently set in this buffer.\n")
                (progn
                  (insert "Variables set in the buffer are shown ")
                  (let ((start (point)))
                    (insert "highlighted")
                    (add-text-properties start (point)
                                         '(face resumel-variables-active-face)))
                  (insert "; others show template defaults.\n")))
              (insert "Press 'g' to refresh, 'q' to close.\n\n")
              ;; Core variables section
              (when (member filter '(all core))
                (insert "CORE VARIABLES  (shared by all templates)\n")
                (insert (make-string 70 ?-) "\n")
                (if (null core-names)
                    (insert "(none set)\n")
                  (dolist (name core-names)
                    (let* ((bval (cdr (assoc name buf-vars)))
                           (dval (cdr (assoc name defaults)))
                           (val  (or bval dval "")))
                      (let ((line-start (point)))
                        (insert (format "#+RESUMEL_%s: %s\n" name val))
                        (when bval
                          (add-text-properties line-start (1- (point))
                                               '(face resumel-variables-active-face)))))))
                (insert "\n"))
              ;; Template-specific section
              (when (member filter '(all template-specific))
                (insert (format "TEMPLATE-SPECIFIC VARIABLES  [%s]\n" template))
                (insert (make-string 70 ?-) "\n")
                (if (null tmpl-names)
                    (insert "(none set)\n")
                  (dolist (name tmpl-names)
                    (let* ((bval (cdr (assoc name buf-vars)))
                           (dval (cdr (assoc name defaults)))
                           (val  (or bval dval "")))
                      (let ((line-start (point)))
                        (insert (format "#+RESUMEL_%s: %s\n" name val))
                        (when bval
                          (add-text-properties line-start (1- (point))
                                               '(face resumel-variables-active-face)))))))
                (insert "\n")))
          (setq resumel-variables--populating nil))
        (goto-char (min saved-pt (point-max))))
      (display-buffer buf))))
(defun resumel-variables-mode-p ()
  "Return non-nil if the current buffer is in `resumel-variables-mode'."
  (eq major-mode 'resumel-variables-mode))

;; ---------------------------------------------------------------------------
;; Public interactive commands — get / set / show

;;;###autoload
(defun resumel-get-template-variable (var)
  "Return the effective value of template variable VAR.
Checks the current buffer's #+RESUMEL_VAR keywords first, then falls
back to the selected template's defaults.

When called interactively, prompts with completion and echoes the result."
  (interactive
   (let* ((template (resumel--get-buffer-template))
          (defaults (resumel--get-template-defaults template))
          (buf-vars (resumel--get-buffer-vars))
          (names    (delete-dups
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
               (cond (buf-val "set in buffer")
                     (def-val "template default")
                     (t       "unknown variable"))))
    value))

;;;###autoload
(defun resumel-set-template-variable (var value)
  "Set template variable VAR to VALUE in the current Org buffer header.
Inserts or updates #+RESUMEL_VAR: value.  Does not modify template
source files — the change applies to this buffer only."
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
  (let ((keyword   (format "#+RESUMEL_%s" var))
        (search-re (format "^[ \t]*#\\+RESUMEL_%s:" (regexp-quote var)))
        (any-re    "^[ \t]*#\\+RESUMEL_"))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward search-re nil t)
          ;; Update existing line in-place, reading indent directly from buffer.
          (let* ((bol (line-beginning-position))
                 (indent (save-excursion
                           (goto-char bol)
                           (skip-chars-forward " \t")
                           (buffer-substring-no-properties bol (point)))))
            (delete-region bol (line-end-position))
            (insert (format "%s%s: %s" indent keyword value)))
        ;; Variable not yet in buffer — find the right insertion point.
        (let ((insert-pos nil)
              (insert-indent ""))
          (goto-char (point-min))
          (while (re-search-forward any-re nil t)
            ;; Read indent directly from buffer bytes, not from match-data.
            (let ((bol (line-beginning-position)))
              (setq insert-indent
                    (save-excursion
                      (goto-char bol)
                      (skip-chars-forward " \t")
                      (buffer-substring-no-properties bol (point))))
              (setq insert-pos (line-end-position))))
          (if insert-pos
              ;; Insert after the last #+RESUMEL_ line, inheriting its indent.
              (progn
                (goto-char insert-pos)
                (insert (format "\n%s%s: %s" insert-indent keyword value)))
            ;; No #+RESUMEL_ lines yet: insert after the last #+keyword block.
            (goto-char (point-min))
            (let (last-kw-end)
              (while (looking-at "^[ \t]*#\\+")
                (setq last-kw-end (line-end-position))
                (forward-line 1))
              (if last-kw-end
                  (progn (goto-char last-kw-end)
                         (insert (format "\n%s: %s" keyword value)))
                (insert (format "%s: %s\n" keyword value))))))))))

;;;###autoload
(defun resumel-show-variables ()
  "Show variables currently set in the Org buffer (core and template-specific).
Only variables with an explicit #+RESUMEL_* keyword in the buffer are shown.
Use \[resumel-show-all-variables] to also see available template defaults."
  (interactive)
  (let* ((org-buf  (if (resumel-variables-mode-p)
                       resumel-variables--source-buffer
                     (current-buffer)))
         (template (with-current-buffer org-buf (resumel--get-buffer-template)))
         (buf      (get-buffer-create "*resumel: template variables*")))
    (resumel--populate-variables-buffer
     buf org-buf template #'resumel-show-variables 'all t)))

;;;###autoload
(defun resumel-show-all-variables ()
  "Show all available template variables (core and template-specific).
Variables explicitly set in the buffer are highlighted; others show template
defaults.  Use \[resumel-show-variables] to see only variables set in the buffer."
  (interactive)
  (let* ((org-buf  (if (resumel-variables-mode-p)
                       resumel-variables--source-buffer
                     (current-buffer)))
         (template (with-current-buffer org-buf (resumel--get-buffer-template)))
         (buf      (get-buffer-create "*resumel: template variables*")))
    (resumel--populate-variables-buffer
     buf org-buf template #'resumel-show-all-variables 'all nil)))

;;;###autoload
(defun resumel-show-core-variables ()
  "Show core variables currently set in the Org buffer.
Only core #+RESUMEL_* keywords present in the buffer are shown.
Use \[resumel-show-all-core-variables] to also see available core defaults."
  (interactive)
  (let* ((org-buf  (if (resumel-variables-mode-p)
                       resumel-variables--source-buffer
                     (current-buffer)))
         (template (with-current-buffer org-buf (resumel--get-buffer-template)))
         (buf      (get-buffer-create "*resumel: template variables*")))
    (resumel--populate-variables-buffer
     buf org-buf template #'resumel-show-core-variables 'core t)))

;;;###autoload
(defun resumel-show-all-core-variables ()
  "Show all core variables (shared by all templates), including defaults.
Variables explicitly set in the buffer are highlighted; others show template
defaults.  Use \[resumel-show-core-variables] to see only variables set in the buffer."
  (interactive)
  (let* ((org-buf  (if (resumel-variables-mode-p)
                       resumel-variables--source-buffer
                     (current-buffer)))
         (template (with-current-buffer org-buf (resumel--get-buffer-template)))
         (buf      (get-buffer-create "*resumel: template variables*")))
    (resumel--populate-variables-buffer
     buf org-buf template #'resumel-show-all-core-variables 'core nil)))

;;;###autoload
(defun resumel-show-template-variables ()
  "Show template-specific variables currently set in the Org buffer.
Only template-specific #+RESUMEL_* keywords present in the buffer are shown.
Use \[resumel-show-all-template-variables] to also see available template defaults."
  (interactive)
  (let* ((org-buf  (if (resumel-variables-mode-p)
                       resumel-variables--source-buffer
                     (current-buffer)))
         (template (with-current-buffer org-buf (resumel--get-buffer-template)))
         (buf      (get-buffer-create "*resumel: template variables*")))
    (resumel--populate-variables-buffer
     buf org-buf template #'resumel-show-template-variables 'template-specific t)))

;;;###autoload
(defun resumel-show-all-template-variables ()
  "Show all template-specific variables, including defaults.
Variables explicitly set in the buffer are highlighted; others show template
defaults.  Use \[resumel-show-template-variables] to see only variables set in the buffer."
  (interactive)
  (let* ((org-buf  (if (resumel-variables-mode-p)
                       resumel-variables--source-buffer
                     (current-buffer)))
         (template (with-current-buffer org-buf (resumel--get-buffer-template)))
         (buf      (get-buffer-create "*resumel: template variables*")))
    (resumel--populate-variables-buffer
     buf org-buf template #'resumel-show-all-template-variables 'template-specific nil)))

(provide 'resumel)
;;; resumel.el ends here
