(require 'ert)
(require 'cl-lib)
(require 'resumel)
(require 'resumel-mode)
(require 'resumel-install)

;; Declare completion-UI variables as special so let-bindings in tests
;; work correctly for simulating vertico/ivy state.
(defvar vertico-mode)
(defvar vertico--index)
(defvar vertico--candidates)
(defvar ivy-mode)

;; Define test directories
(defvar resumel-test-dir (file-name-directory (or load-file-name buffer-file-name))
  "Root directory for resumel tests.")

(defvar resumel-fixture-dir (expand-file-name "fixtures" resumel-test-dir)
  "Directory containing fixture Org files for testing.")

(defvar resumel-expected-dir (expand-file-name "expected" resumel-test-dir)
  "Directory containing expected PDF outputs for testing.")

(defvar resumel-results-dir (expand-file-name "results" resumel-test-dir)
  "Directory where test-generated PDF results are stored.")

;; Function to export Org to PDF
(defun resumel-test-export-org-to-pdf (org-file)
  "Export ORG-FILE to PDF and return the PDF filename."
  (let* ((base-name (file-name-base org-file))
         (pdf-file (expand-file-name (concat base-name ".pdf") resumel-results-dir))
         (output-dir resumel-results-dir))
    ;; Print test info
    (message "Resumel Test - Exporting: %s" org-file)
    (message "Resumel Test - Output PDF: %s" pdf-file)
    (message "Resumel Test - Output directory: %s" output-dir)

    ;; Ensure output directory exists
    (make-directory output-dir t)
    (with-current-buffer (find-file-noselect org-file)
      ;; Set LaTeX export settings
      (setq-local org-latex-output-directory output-dir)

      ;; Export to PDF and capture any error details
      (condition-case err
          (progn
            (resumel--setup)
            (resumel--merge-profile-export-keywords)
            (org-latex-export-to-pdf)
            (unless (file-exists-p pdf-file)
              (message "LaTeX Output:\n%s" (with-current-buffer "*Org PDF LaTeX Output*" (buffer-string)))
              (error "File \"%s\" wasn't produced. See \"*Org PDF LaTeX Output*\" for details" pdf-file))
            pdf-file)  ; Return pdf-file
        (error
         (message "Export error: %S" err)
         (when (get-buffer "*Org PDF LaTeX Output*")
           (with-current-buffer "*Org PDF LaTeX Output*"
             (message "LaTeX Output:\n%s" (buffer-string))))
         (signal (car err) (cdr err)))))))

(defun resumel-files-equal-p (file1 file2)
  "Compare FILE1 and FILE2 using vslavik's diff-pdf CLI.
See https://github.com/vslavik/diff-pdf .  Uses RESUMEL_DIFF_PDF (full path)
when set; otherwise the program name diff-pdf on exec-path (from the
environment that started Emacs)."
  (let ((channel-tolerance (or (getenv "DIFF_PDF_CHANNEL_TOLERANCE") "0"))
        (per-page-pixel-tolerance (or (getenv "DIFF_PDF_PER_PAGE_PIXEL_TOLERANCE") "0"))
        (diff-pdf (or (getenv "RESUMEL_DIFF_PDF") "diff-pdf")))
    (zerop (call-process diff-pdf nil nil nil
                         "--channel-tolerance" channel-tolerance
                         "--per-page-pixel-tolerance" per-page-pixel-tolerance
                         file1 file2))))

(ert-deftest resumel-test-expand-tags ()
  "resumel-expand-tags and resumel-expand-ltags produce correct LaTeX."
  (should (string-match-p "\\\\cvtag{A}" (resumel-expand-tags "A" "B")))
  (should (string-match-p "\\\\cvtag{X}" (resumel-expand-ltags "X" "1" "Y" "2"))))

;; List of test cases
(defvar resumel-test-cases
  '(("moderncv-basic-blue.org" "moderncv-basic-blue.pdf")
    ("moderncv-basic-green.org" "moderncv-basic-green.pdf")
    ("moderncv-complex.org" "moderncv-complex.pdf")
    ("altacv-basic.org" "altacv-basic.pdf")
    ("altacv-complex.org" "altacv-complex.pdf")
    ("modaltacv-basic.org" "modaltacv-basic.pdf")
    ("modaltacv-complex.org" "modaltacv-complex.pdf")
    ("awesomecv-basic.org" "awesomecv-basic.pdf")
    ("awesomecv-complex.org" "awesomecv-complex.pdf")
    ("jakes-basic.org" "jakes-basic.pdf")
    ("jakes-complex.org" "jakes-complex.pdf")
    ("jakes-orig.org" "jakes-orig.pdf"))
  "List of test cases. Each entry is a list of Org file and expected PDF file.")

;; Define a test for each test case
(dolist (test-case resumel-test-cases)
  (let ((org-file (car test-case))
        (expected-pdf (cadr test-case)))
    (eval
     `(ert-deftest ,(intern (format "resumel-test-%s" (file-name-base org-file))) ()
        ,(format "Test export of %s and compare with %s" org-file expected-pdf)
        (let* ((org-file-path (expand-file-name ,org-file resumel-fixture-dir))
               (generated-pdf (resumel-test-export-org-to-pdf org-file-path))
               (expected-pdf-path (expand-file-name ,expected-pdf resumel-expected-dir)))
          ;; Debug output
          (message "Testing PDF paths:")
          (message "Generated: %s" generated-pdf)
          (message "Expected: %s" expected-pdf-path)
          ;; Verify files exist
          (should (file-exists-p generated-pdf))
          (should (file-exists-p expected-pdf-path))
          ;; Compare PDFs
          (should (resumel-files-equal-p generated-pdf expected-pdf-path)))))))

;;; ---------------------------------------------------------------------------
;;; Unit tests for template variable introspection
;;; ---------------------------------------------------------------------------

;; Helper: create a temporary Org buffer with the given content, run BODY,
;; then kill the buffer.
(defmacro resumel-test-with-org-buffer (content &rest body)
  "Evaluate BODY in a temporary Org-mode buffer containing CONTENT."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer " *resumel-test-org*")))
     (unwind-protect
         (with-current-buffer buf
           (org-mode)
           (insert ,content)
           ,@body)
       (kill-buffer buf))))

;; ---- resumel--get-buffer-template -------------------------------------------

(ert-deftest resumel-test-get-buffer-template-explicit ()
  "Returns the template named in #+RESUMEL_TEMPLATE."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: altacv\n#+TITLE: Test\n"
    (should (string= (resumel--get-buffer-template) "altacv"))))

(ert-deftest resumel-test-get-buffer-template-default ()
  "Falls back to `resumel-default-template' when no keyword is present."
  (resumel-test-with-org-buffer
      "#+TITLE: Test\n"
    (let ((resumel-default-template "moderncv"))
      (should (string= (resumel--get-buffer-template) "moderncv")))))

;; ---- resumel--get-buffer-vars -----------------------------------------------

(ert-deftest resumel-test-get-buffer-vars-basic ()
  "Parses RESUMEL_* keywords from the buffer."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_MODERNCV_COLOR: green\n#+TITLE: Test\n"
    (let ((vars (resumel--get-buffer-vars)))
      ;; TEMPLATE is excluded; MODERNCV_COLOR should be present
      (should (null (assoc "TEMPLATE" vars)))
      (should (string= (cdr (assoc "MODERNCV_COLOR" vars)) "green")))))

(ert-deftest resumel-test-get-buffer-vars-empty ()
  "Returns nil when no RESUMEL_* keywords (other than TEMPLATE) are present."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+TITLE: Test\n"
    (should (null (resumel--get-buffer-vars)))))

;; ---- resumel--get-template-defaults -----------------------------------------

(ert-deftest resumel-test-get-template-defaults-moderncv ()
  "Loads and returns defaults for the moderncv template."
  (let ((defaults (resumel--get-template-defaults "moderncv")))
    (should (listp defaults))
    (should (assoc "MODERNCV_COLOR" defaults))
    (should (string= (cdr (assoc "MODERNCV_COLOR" defaults)) "blue"))
    (should (assoc "COMPILER" defaults))
    (should (string= (cdr (assoc "COMPILER" defaults)) "pdflatex"))))

(ert-deftest resumel-test-get-template-defaults-altacv ()
  "Loads and returns defaults for the altacv template."
  (let ((defaults (resumel--get-template-defaults "altacv")))
    (should (listp defaults))
    (should (assoc "ALTACV_COLUMNRATIO" defaults))
    (should (string= (cdr (assoc "ALTACV_COLUMNRATIO" defaults)) "0.6"))))

(ert-deftest resumel-test-get-template-defaults-awesomecv ()
  "Loads and returns defaults for the awesomecv template."
  (let ((defaults (resumel--get-template-defaults "awesomecv")))
    (should (listp defaults))
    (should (assoc "AWESOMECV_COLOR" defaults))
    (should (string= (cdr (assoc "AWESOMECV_COLOR" defaults)) "awesome-red"))))

(ert-deftest resumel-test-get-template-defaults-modaltacv ()
  "Loads and returns defaults for the modaltacv template."
  (let ((defaults (resumel--get-template-defaults "modaltacv")))
    (should (listp defaults))
    (should (assoc "MODALTACV_COLUMNRATIO" defaults))
    (should (string= (cdr (assoc "MODALTACV_COLUMNRATIO" defaults)) "0.6"))))

(ert-deftest resumel-test-get-template-defaults-jakes ()
  "Loads and returns defaults for the jakes template."
  (let ((defaults (resumel--get-template-defaults "jakes")))
    (should (listp defaults))
    (should (assoc "JAKES_FONT" defaults))
    (should (string= (cdr (assoc "JAKES_FONT" defaults)) "default"))
    (should (assoc "COMPILER" defaults))
    (should (string= (cdr (assoc "COMPILER" defaults)) "pdflatex"))))

;; ---- resumel-get-template-variable ------------------------------------------

(ert-deftest resumel-test-get-template-variable-default ()
  "Returns the template default when the variable is not set in the buffer."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+TITLE: Test\n"
    (should (string= (resumel-get-template-variable "MODERNCV_COLOR") "blue"))))

(ert-deftest resumel-test-get-template-variable-buffer-overrides ()
  "Returns the buffer value when a #+RESUMEL_* keyword is present."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_MODERNCV_COLOR: green\n#+TITLE: Test\n"
    (should (string= (resumel-get-template-variable "MODERNCV_COLOR") "green"))))

(ert-deftest resumel-test-get-template-variable-unknown ()
  "Returns nil for an unknown variable."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+TITLE: Test\n"
    (should (null (resumel-get-template-variable "NONEXISTENT_VAR")))))

;; ---- resumel-set-template-variable ------------------------------------------

(ert-deftest resumel-test-set-template-variable-insert-new ()
  "Inserts a new #+RESUMEL_VAR keyword when it does not exist."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+TITLE: Test\n"
    (resumel-set-template-variable "MODERNCV_COLOR" "red")
    (should (string= (resumel-get-template-variable "MODERNCV_COLOR") "red"))))

(ert-deftest resumel-test-set-template-variable-update-existing ()
  "Updates an existing #+RESUMEL_VAR keyword in place."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_MODERNCV_COLOR: green\n#+TITLE: Test\n"
    (resumel-set-template-variable "MODERNCV_COLOR" "burgundy")
    ;; Only one occurrence should remain
    (let ((count 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#\\+RESUMEL_MODERNCV_COLOR:" nil t)
          (setq count (1+ count))))
      (should (= count 1)))
    (should (string= (resumel-get-template-variable "MODERNCV_COLOR") "burgundy"))))

(ert-deftest resumel-test-set-template-variable-no-existing-keywords ()
  "Inserts keyword at top when no RESUMEL_ keywords exist yet."
  (resumel-test-with-org-buffer
      "#+TITLE: Test\n#+AUTHOR: Jane\n"
    (resumel-set-template-variable "MODERNCV_COLOR" "purple")
    (should (string= (resumel-get-template-variable "MODERNCV_COLOR") "purple"))))

;; ---- resumel-show-all-variables / resumel-show-core-variables / etc. -------

(ert-deftest resumel-test-show-variables ()
  "resumel-show-variables shows only variables set in the buffer (both sections)."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv
#+RESUMEL_MODERNCV_COLOR: orange
#+TITLE: Test
"
    (resumel-show-variables)
    (let ((buf (get-buffer "*resumel: template variables*")))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match "TEMPLATE-SPECIFIC" content))
          ;; MODERNCV_COLOR is set — must appear
          (should (string-match "#\\+RESUMEL_MODERNCV_COLOR: orange" content))
          ;; COMPILER is not set — must not appear
          (should-not (string-match "#\\+RESUMEL_COMPILER:" content)))))))

(ert-deftest resumel-test-show-all-variables ()
  "resumel-show-all-variables shows all variables including defaults (both sections)."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv
#+RESUMEL_MODERNCV_COLOR: orange
#+TITLE: Test
"
    (resumel-show-all-variables)
    (let ((buf (get-buffer "*resumel: template variables*")))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match "moderncv" content))
          (should (string-match "CORE VARIABLES" content))
          (should (string-match "TEMPLATE-SPECIFIC" content))
          ;; Default core variable must appear
          (should (string-match "#\\+RESUMEL_COMPILER:" content))
          (should (string-match "#\\+RESUMEL_MODERNCV_COLOR: orange" content)))))))

(ert-deftest resumel-test-show-core-variables ()
  "resumel-show-core-variables shows only core variables set in the buffer."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv
#+RESUMEL_COMPILER: xelatex
#+TITLE: Test
"
    (resumel-show-core-variables)
    (let ((buf (get-buffer "*resumel: template variables*")))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match "CORE VARIABLES" content))
          ;; COMPILER is set — must appear
          (should (string-match "#\\+RESUMEL_COMPILER: xelatex" content))
          ;; Template-specific section must not appear
          (should-not (string-match "TEMPLATE-SPECIFIC" content))
          (should-not (string-match "MODERNCV_COLOR" content)))))))

(ert-deftest resumel-test-show-all-core-variables ()
  "resumel-show-all-core-variables shows all core variables including defaults."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv
#+TITLE: Test
"
    (resumel-show-all-core-variables)
    (let ((buf (get-buffer "*resumel: template variables*")))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match "CORE VARIABLES" content))
          ;; Default core variable must appear even though not set
          (should (string-match "#\\+RESUMEL_COMPILER:" content))
          ;; Template-specific section must not appear
          (should-not (string-match "TEMPLATE-SPECIFIC" content))
          (should-not (string-match "MODERNCV_COLOR" content)))))))

(ert-deftest resumel-test-show-template-variables ()
  "resumel-show-template-variables shows only template-specific variables set in the buffer."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv
#+RESUMEL_MODERNCV_COLOR: orange
#+TITLE: Test
"
    (resumel-show-template-variables)
    (let ((buf (get-buffer "*resumel: template variables*")))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match "TEMPLATE-SPECIFIC" content))
          ;; MODERNCV_COLOR is set — must appear
          (should (string-match "#\\+RESUMEL_MODERNCV_COLOR: orange" content))
          ;; Core section must not appear
          (should-not (string-match "CORE VARIABLES" content))
          (should-not (string-match "#\\+RESUMEL_COMPILER:" content)))))))

(ert-deftest resumel-test-show-all-template-variables ()
  "resumel-show-all-template-variables shows all template-specific variables including defaults."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv
#+TITLE: Test
"
    (resumel-show-all-template-variables)
    (let ((buf (get-buffer "*resumel: template variables*")))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match "TEMPLATE-SPECIFIC" content))
          ;; Default template variable must appear even though not set
          (should (string-match "#\\+RESUMEL_MODERNCV_COLOR:" content))
          ;; Core section must not appear
          (should-not (string-match "CORE VARIABLES" content))
          (should-not (string-match "#\\+RESUMEL_COMPILER:" content)))))))
;; ---- resumel-show-* does not pollute the Org buffer ------------------------

(ert-deftest resumel-test-show-variables-no-pollution ()
  "Calling resumel-show-all-variables must not write default vars to the Org buffer."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_MODERNCV_COLOR: blue\n#+TITLE: Test\n"
    (let* ((org-buf  (current-buffer))
           (initial  (buffer-string)))
      (resumel-show-all-variables)
      ;; The Org buffer content must be identical after showing variables.
      (should (string= (buffer-string) initial)))))

;; ---- resumel-variables-mode live sync ---------------------------------------

(ert-deftest resumel-test-variables-mode-live-sync ()
  "Editing a #+RESUMEL_VAR line in the variables buffer syncs to the Org buffer."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_MODERNCV_COLOR: blue\n#+TITLE: Test\n"
    (let ((org-buf (current-buffer)))
      (resumel-show-all-variables)
      (let ((vars-buf (get-buffer "*resumel: template variables*")))
        (should vars-buf)
        (with-current-buffer vars-buf
          ;; Edit the MODERNCV_COLOR line (already in buffer — pre-highlighted)
          (goto-char (point-min))
          (re-search-forward "^#\\+RESUMEL_MODERNCV_COLOR: ")
          (delete-region (point) (line-end-position))
          (insert "burgundy")
          ;; The entire line must carry the active face after the edit
          (goto-char (point-min))
          (re-search-forward "^#\\+RESUMEL_MODERNCV_COLOR:")
          (should (eq (get-text-property (line-beginning-position) 'face)
                      'resumel-variables-active-face)))
        ;; Check that the Org buffer was updated
        (should (string= (with-current-buffer org-buf
                           (resumel-get-template-variable "MODERNCV_COLOR"))
                         "burgundy"))))))

(ert-deftest resumel-test-variables-mode-reverts-highlight-to-default ()
  "Changing a variable back to its default removes the active face."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_MODERNCV_COLOR: burgundy\n#+TITLE: Test\n"
    (let ((org-buf (current-buffer)))
      (resumel-show-all-variables)
      (let ((vars-buf (get-buffer "*resumel: template variables*")))
        (should vars-buf)
        (with-current-buffer vars-buf
          ;; MODERNCV_COLOR is set in the buffer — line should be highlighted
          (goto-char (point-min))
          (re-search-forward "^#\\+RESUMEL_MODERNCV_COLOR: ")
          (should (eq (get-text-property (line-beginning-position) 'face)
                      'resumel-variables-active-face))
          ;; Change it back to the template default value
          (delete-region (point) (line-end-position))
          (insert "blue")
          ;; The active face must now be gone
          (goto-char (point-min))
          (re-search-forward "^#\\+RESUMEL_MODERNCV_COLOR:")
          (should-not (eq (get-text-property (line-beginning-position) 'face)
                          'resumel-variables-active-face)))))))

(ert-deftest resumel-test-variables-mode-default-promotes-to-highlighted ()
  "Editing a default-value line immediately highlights it in the variables buffer."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+TITLE: Test\n"
    (let ((org-buf (current-buffer)))
      (resumel-show-all-variables)
      (let ((vars-buf (get-buffer "*resumel: template variables*")))
        (should vars-buf)
        (with-current-buffer vars-buf
          ;; MODERNCV_COLOR is not set in the buffer — it shows the default
          (goto-char (point-min))
          (re-search-forward "^#\\+RESUMEL_MODERNCV_COLOR: ")
          ;; Not yet highlighted
          (should-not (eq (get-text-property (line-beginning-position) 'face)
                          'resumel-variables-active-face))
          ;; Edit it (promoting from default to buffer-set)
          (delete-region (point) (line-end-position))
          (insert "burgundy")
          ;; Must now carry the active face
          (goto-char (point-min))
          (re-search-forward "^#\\+RESUMEL_MODERNCV_COLOR:")
          (should (eq (get-text-property (line-beginning-position) 'face)
                      'resumel-variables-active-face)))
        ;; Variable must be set in Org buffer
        (should (string= (with-current-buffer org-buf
                           (resumel-get-template-variable "MODERNCV_COLOR"))
                         "burgundy"))))))

;; ---- resumel-select-template updates Org header ----------------------------

(ert-deftest resumel-test-select-template-updates-org-buffer ()
  "resumel-select-template inserts #+RESUMEL_TEMPLATE when in an Org buffer."
  (resumel-test-with-org-buffer
      "#+TITLE: Test\n"
    (resumel-select-template "altacv")
    (should (string= (resumel--get-buffer-template) "altacv"))
    ;; Keyword should now be present in the buffer
    (save-excursion
      (goto-char (point-min))
      (should (re-search-forward "^#\\+RESUMEL_TEMPLATE: altacv" nil t)))))

(ert-deftest resumel-test-select-template-updates-existing-keyword ()
  "resumel-select-template updates an existing #+RESUMEL_TEMPLATE keyword."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+TITLE: Test\n"
    (resumel-select-template "awesomecv")
    (should (string= (resumel--get-buffer-template) "awesomecv"))
    ;; Only one RESUMEL_TEMPLATE line should remain
    (let ((count 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#\\+RESUMEL_TEMPLATE:" nil t)
          (setq count (1+ count))))
      (should (= count 1)))))

;; ---- resumel-core-variable-names --------------------------------------------

(ert-deftest resumel-test-core-variable-names-defined ()
  "resumel-core-variable-names is a non-empty list of strings."
  (should (listp resumel-core-variable-names))
  (should (> (length resumel-core-variable-names) 0))
  (should (member "COMPILER" resumel-core-variable-names))
  (should (member "CVTAG_CORNER_DEFAULT" resumel-core-variable-names))
  (should (member "NAME" resumel-core-variable-names)))

;;; ---------------------------------------------------------------------------
;;; Unit tests for template preview
;;; ---------------------------------------------------------------------------

;; ---- resumel--find-preview-pdf ----------------------------------------------

(ert-deftest resumel-test-find-preview-pdf-finds-matching ()
  "resumel--find-preview-pdf returns a PDF path matching the template name."
  (let ((resumel-preview-pdf-dir (expand-file-name "expected" resumel-test-dir)))
    (let ((result (resumel--find-preview-pdf "moderncv")))
      (should (stringp result))
      (should (file-exists-p result))
      (should (string-match "moderncv-" (file-name-nondirectory result))))))

(ert-deftest resumel-test-find-preview-pdf-prefers-pattern-match ()
  "resumel--find-preview-pdf returns the first PDF matching resumel-preview-pdf-pattern."
  (let ((resumel-preview-pdf-dir (expand-file-name "expected" resumel-test-dir))
        (resumel-preview-pdf-pattern "-complex\\.pdf$"))
    ;; moderncv has basic-blue, basic-green, complex — pattern selects complex
    (let ((result (resumel--find-preview-pdf "moderncv")))
      (should (string-match "moderncv-complex\\.pdf$" result)))))

(ert-deftest resumel-test-find-preview-pdf-falls-back-without-pattern-match ()
  "resumel--find-preview-pdf falls back to first alphabetical when pattern matches nothing."
  (let ((resumel-preview-pdf-dir (expand-file-name "expected" resumel-test-dir))
        (resumel-preview-pdf-pattern "-XXXXNOMATCH\\.pdf$"))
    ;; Pattern matches nothing, so fall back to first alphabetically: basic-blue
    (let ((result (resumel--find-preview-pdf "moderncv")))
      (should (string-match "moderncv-basic-blue\\.pdf$" result)))))

(ert-deftest resumel-test-find-preview-pdf-returns-nil-for-unknown-template ()
  "resumel--find-preview-pdf returns nil when no matching PDF exists."
  (let ((resumel-preview-pdf-dir (expand-file-name "expected" resumel-test-dir)))
    (should (null (resumel--find-preview-pdf "nonexistent-template")))))

(ert-deftest resumel-test-find-preview-pdf-returns-nil-for-missing-dir ()
  "resumel--find-preview-pdf returns nil when the preview directory does not exist."
  (let ((resumel-preview-pdf-dir "/nonexistent/path/to/pdfs"))
    (should (null (resumel--find-preview-pdf "moderncv")))))

;; ---- resumel-select-template preview integration ----------------------------

(ert-deftest resumel-test-select-template-no-preview-non-interactive ()
  "Calling resumel-select-template non-interactively never triggers preview.
Preview only fires via the interactive form's minibuffer hooks."
  (let ((preview-called nil))
    (cl-letf (((symbol-function 'resumel--show-preview)
               (lambda (_tmpl) (setq preview-called t))))
      (resumel-test-with-org-buffer
          "#+TITLE: Test\n"
        (resumel-select-template "altacv")))
    (should-not preview-called)))

(ert-deftest resumel-test-select-template-uses-with-live-preview ()
  "resumel-select-template delegates to resumel--with-live-preview interactively."
  (let ((called-templates nil)
        (called-prompt nil))
    (cl-letf (((symbol-function 'resumel--with-live-preview)
               (lambda (templates prompt &optional _force)
                 (setq called-templates templates
                       called-prompt prompt)
                 "altacv")))
      (call-interactively #'resumel-select-template))
    (should (equal called-templates '("moderncv" "altacv" "modaltacv" "awesomecv" "jakes")))
    (should (stringp called-prompt))))

;; ---- resumel--with-live-preview ---------------------------------------------

(ert-deftest resumel-test-with-live-preview-returns-selection-when-disabled ()
  "resumel--with-live-preview returns the completing-read selection (disabled path)."
  (let ((resumel-show-preview-on-select nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "altacv")))
      (should (string= (resumel--with-live-preview
                        '("moderncv" "altacv" "modaltacv" "awesomecv")
                        "Test: ")
                       "altacv")))))

(ert-deftest resumel-test-with-live-preview-returns-selection-when-enabled ()
  "resumel--with-live-preview returns the completing-read selection (enabled path)."
  ;; When enabled, uses minibuffer-with-setup-hook which still calls completing-read.
  ;; In batch/test mode the post-command-hook never fires, but the selection
  ;; is still returned correctly.
  (let ((resumel-show-preview-on-select t))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "modaltacv")))
      (should (string= (resumel--with-live-preview
                        '("moderncv" "altacv" "modaltacv" "awesomecv")
                        "Test: ")
                       "modaltacv")))))

;; ---- resumel--minibuffer-current-candidate ----------------------------------

(ert-deftest resumel-test-minibuffer-current-candidate-exact-input ()
  "Returns exact match when minibuffer input equals a template name."
  (let ((templates '("moderncv" "altacv" "modaltacv" "awesomecv"))
        (vertico-mode nil)
        (ivy-mode nil))
    (cl-letf (((symbol-function 'minibuffer-contents-no-properties)
               (lambda () "altacv")))
      (should (string= (resumel--minibuffer-current-candidate templates)
                       "altacv")))))

(ert-deftest resumel-test-minibuffer-current-candidate-prefix-input ()
  "Returns first prefix match when minibuffer input is a partial name."
  (let ((templates '("moderncv" "altacv" "modaltacv" "awesomecv"))
        (vertico-mode nil)
        (ivy-mode nil))
    (cl-letf (((symbol-function 'minibuffer-contents-no-properties)
               (lambda () "mod")))
      ;; "mod" is a prefix of "moderncv" and "modaltacv"; first match wins
      (should (string= (resumel--minibuffer-current-candidate templates)
                       "moderncv")))))

(ert-deftest resumel-test-minibuffer-current-candidate-empty-input ()
  "Returns first template when minibuffer input is empty."
  (let ((templates '("moderncv" "altacv" "modaltacv" "awesomecv"))
        (vertico-mode nil)
        (ivy-mode nil))
    (cl-letf (((symbol-function 'minibuffer-contents-no-properties)
               (lambda () "")))
      ;; Empty string is a prefix of everything; returns first
      (should (string= (resumel--minibuffer-current-candidate templates)
                       "moderncv")))))

(ert-deftest resumel-test-minibuffer-current-candidate-no-match ()
  "Returns nil when minibuffer input matches no template."
  (let ((templates '("moderncv" "altacv" "modaltacv" "awesomecv"))
        (vertico-mode nil)
        (ivy-mode nil))
    (cl-letf (((symbol-function 'minibuffer-contents-no-properties)
               (lambda () "xyz-no-such-template")))
      (should (null (resumel--minibuffer-current-candidate templates))))))

(ert-deftest resumel-test-minibuffer-current-candidate-vertico ()
  "Returns vertico's highlighted candidate when vertico-mode is active."
  (let ((templates '("moderncv" "altacv" "modaltacv" "awesomecv"))
        (vertico-mode t)
        (vertico--candidates '("altacv" "awesomecv" "modaltacv" "moderncv"))
        (vertico--index 2))          ; highlights "modaltacv"
    (cl-letf (((symbol-function 'minibuffer-contents-no-properties)
               (lambda () "")))    ; input is empty — would default to "moderncv"
      (should (string= (resumel--minibuffer-current-candidate templates)
                       "modaltacv")))))

(ert-deftest resumel-test-minibuffer-current-candidate-vertico-ignores-non-templates ()
  "Ignores vertico candidate when it is not in the templates list."
  (let ((templates '("moderncv" "altacv" "modaltacv" "awesomecv"))
        (vertico-mode t)
        (vertico--candidates '("some-other-value"))
        (vertico--index 0))
    (cl-letf (((symbol-function 'minibuffer-contents-no-properties)
               (lambda () "alt")))
      ;; vertico candidate is not in templates; falls through to prefix match
      (should (string= (resumel--minibuffer-current-candidate templates)
                       "altacv")))))

;;; ---------------------------------------------------------------------------
;;; Unit tests for template file viewing
;;; ---------------------------------------------------------------------------

;; ---- resumel-view-template-el -----------------------------------------------

(ert-deftest resumel-test-view-template-el-opens-correct-file ()
  "resumel-view-template-el calls find-file-other-window on the correct .el file."
  (let (opened-file)
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (f) (setq opened-file f))))
      (resumel-view-template-el "moderncv"))
    (should (stringp opened-file))
    (should (string-suffix-p "moderncv/moderncv.el" opened-file))
    (should (file-exists-p opened-file))))

(ert-deftest resumel-test-view-template-el-all-templates ()
  "resumel-view-template-el resolves .el paths for all five templates."
  (dolist (tmpl '("moderncv" "altacv" "modaltacv" "awesomecv" "jakes"))
    (let (opened-file)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f) (setq opened-file f))))
        (resumel-view-template-el tmpl))
      (should (file-exists-p opened-file))
      (should (string-suffix-p (format "%s/%s.el" tmpl tmpl) opened-file)))))

(ert-deftest resumel-test-view-template-el-errors-for-nonexistent ()
  "resumel-view-template-el signals an error for an unknown template."
  (cl-letf (((symbol-function 'find-file-other-window) #'ignore))
    (should-error (resumel-view-template-el "not-a-real-template"))))

;; ---- resumel-view-template-org ----------------------------------------------

(ert-deftest resumel-test-view-template-org-opens-correct-file ()
  "resumel-view-template-org calls find-file-other-window on the correct .org file."
  (let (opened-file)
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (f) (setq opened-file f))))
      (resumel-view-template-org "altacv"))
    (should (stringp opened-file))
    (should (string-suffix-p "altacv/altacv.org" opened-file))
    (should (file-exists-p opened-file))))

(ert-deftest resumel-test-view-template-org-all-templates ()
  "resumel-view-template-org resolves .org paths for all five templates."
  (dolist (tmpl '("moderncv" "altacv" "modaltacv" "awesomecv" "jakes"))
    (let (opened-file)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f) (setq opened-file f))))
        (resumel-view-template-org tmpl))
      (should (file-exists-p opened-file))
      (should (string-suffix-p (format "%s/%s.org" tmpl tmpl) opened-file)))))

(ert-deftest resumel-test-view-template-org-errors-for-nonexistent ()
  "resumel-view-template-org signals an error for an unknown template."
  (cl-letf (((symbol-function 'find-file-other-window) #'ignore))
    (should-error (resumel-view-template-org "not-a-real-template"))))

;; ---- resumel-view-template-pdf ----------------------------------------------

(ert-deftest resumel-test-view-template-pdf-opens-correct-file ()
  "resumel-view-template-pdf calls find-file-other-window on the matching PDF."
  (let (opened-file)
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (f) (setq opened-file f))))
      (let ((resumel-preview-pdf-dir (expand-file-name "expected" resumel-test-dir))
            (resumel-preview-pdf-pattern "-complex\\.pdf$"))
        (resumel-view-template-pdf "moderncv")))
    (should (stringp opened-file))
    (should (string-match "moderncv-complex\\.pdf$" opened-file))
    (should (file-exists-p opened-file))))

(ert-deftest resumel-test-view-template-pdf-all-templates ()
  "resumel-view-template-pdf resolves a PDF for all five templates."
  (dolist (tmpl '("moderncv" "altacv" "modaltacv" "awesomecv" "jakes"))
    (let (opened-file)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f) (setq opened-file f))))
        (let ((resumel-preview-pdf-dir (expand-file-name "expected" resumel-test-dir)))
          (resumel-view-template-pdf tmpl)))
      (should (file-exists-p opened-file))
      (should (string-match (concat "^" (regexp-quote tmpl) "-")
                             (file-name-nondirectory opened-file))))))

(ert-deftest resumel-test-view-template-pdf-messages-when-no-pdf ()
  "resumel-view-template-pdf messages the user when no PDF is found."
  (let (last-message)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq last-message (apply #'format fmt args)))))
      (let ((resumel-preview-pdf-dir "/nonexistent/path"))
        (resumel-view-template-pdf "moderncv")))
    (should (stringp last-message))
    (should (string-match "moderncv" last-message))))

(ert-deftest resumel-test-view-template-pdf-uses-with-live-preview-forced ()
  "resumel-view-template-pdf calls resumel--with-live-preview with force=t."
  (let (called-force)
    (cl-letf (((symbol-function 'resumel--with-live-preview)
               (lambda (_templates _prompt &optional force)
                 (setq called-force force)
                 "moderncv"))
              ((symbol-function 'find-file-other-window) #'ignore))
      (let ((resumel-preview-pdf-dir (expand-file-name "expected" resumel-test-dir)))
        (call-interactively #'resumel-view-template-pdf)))
    (should called-force)))

;;; ---------------------------------------------------------------------------
;;; resumel-mode macro index and completion-at-point
;;; ---------------------------------------------------------------------------

(ert-deftest resumel-test-mode-merged-macros-include-shared-bf ()
  "Merged file macros include `bf' from resumel.org."
  (let ((resumel-mode--merged-file-macros-cache nil))
    (should (string-match-p "\\\\textbf"
                            (cdr (assoc "bf" (resumel-mode--merged-file-macros "moderncv")))))))

(ert-deftest resumel-test-mode-wheelchart-altacv-is-template-latex ()
  "AltaCV template defines wheelchart with @@latex, shadowing shared eval macro."
  (let ((resumel-mode--merged-file-macros-cache nil))
    (let ((w (cdr (assoc "wheelchart" (resumel-mode--merged-file-macros "altacv")))))
      (should (string-prefix-p "@@latex" w)))))

(ert-deftest resumel-test-mode-wheelchart-moderncv-uses-eval-from-shared ()
  "moderncv.org does not redefine wheelchart; expansion comes from resumel.org (eval)."
  (let ((resumel-mode--merged-file-macros-cache nil))
    (let ((w (cdr (assoc "wheelchart" (resumel-mode--merged-file-macros "moderncv")))))
      (should (string-match-p "eval" w)))))

(ert-deftest resumel-test-mode-buffer-macro-overrides-file ()
  "Buffer-local #+MACRO: overrides merged file definitions."
  (resumel-test-with-org-buffer
      "#+MACRO: bf buffer-override\n#+RESUMEL_TEMPLATE: moderncv\n"
    (let ((resumel-mode--merged-file-macros-cache nil))
      (should (string= (cdr (assoc "bf" (resumel-mode--macro-index)))
                       "buffer-override")))))

(ert-deftest resumel-test-mode-macro-capf-inside-braces ()
  "`resumel-mode-macro-completion-at-point' returns bounds for the macro name prefix."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n\n{{{bf"
    (resumel-mode 1)
    (goto-char (point-max))
    (let ((capf (resumel-mode-macro-completion-at-point)))
      (should capf)
      (should (equal (buffer-substring-no-properties (nth 0 capf) (nth 1 capf))
                     "bf"))
      (should (member "bf" (nth 2 capf))))))

(ert-deftest resumel-test-mode-macro-annotation-uses-description ()
  "Macro completion annotation includes the hand-written description."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n\n{{{bf"
    (resumel-mode 1)
    (goto-char (point-max))
    (let* ((capf (resumel-mode-macro-completion-at-point))
           (ann-fn (plist-get (nthcdr 3 capf) :annotation-function)))
      (should ann-fn)
      (should (string-match-p "bold text" (funcall ann-fn "bf"))))))

;;; ---------------------------------------------------------------------------
;;; resumel-mode auto-enable detection
;;; ---------------------------------------------------------------------------

(ert-deftest resumel-test-mode-auto-detect-with-template-keyword ()
  "resumel-mode--buffer-has-resumel-template-p returns non-nil for resumel files."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+TITLE: Test\n"
    (should (resumel-mode--buffer-has-resumel-template-p))))

(ert-deftest resumel-test-mode-auto-detect-without-template-keyword ()
  "resumel-mode--buffer-has-resumel-template-p returns nil for plain Org files."
  (resumel-test-with-org-buffer
      "#+TITLE: Test\n#+AUTHOR: Jane\n"
    (should-not (resumel-mode--buffer-has-resumel-template-p))))

(ert-deftest resumel-test-mode-auto-enable-activates-on-template ()
  "resumel-mode--maybe-turn-on enables the mode when #+RESUMEL_TEMPLATE is present."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: altacv\n#+TITLE: Test\n"
    (let ((resumel-mode-auto-enable nil))
      (resumel-mode--maybe-turn-on)
      (should resumel-mode))))

(ert-deftest resumel-test-mode-auto-enable-skips-plain-org ()
  "resumel-mode--maybe-turn-on does not activate in plain Org buffers."
  (resumel-test-with-org-buffer
      "#+TITLE: Test\n"
    (let ((resumel-mode-auto-enable nil))
      (resumel-mode--maybe-turn-on)
      (should-not resumel-mode))))

;;; ---------------------------------------------------------------------------
;;; resumel-mode keyword completion bounds
;;; ---------------------------------------------------------------------------

(ert-deftest resumel-test-mode-keyword-bounds-after-underscore ()
  "Keyword bounds are detected after #+RESUMEL_ with partial name."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_MOD"
    (goto-char (point-max))
    (let ((bounds (resumel-mode--resumel-keyword-name-bounds)))
      (should bounds)
      (should (string= "MOD"
                        (buffer-substring-no-properties
                         (car bounds) (cdr bounds)))))))

(ert-deftest resumel-test-mode-keyword-bounds-before-underscore ()
  "Keyword bounds are detected at #+RESUMEL without trailing underscore."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL"
    (goto-char (point-max))
    (let ((bounds (resumel-mode--resumel-keyword-name-bounds)))
      (should bounds)
      (should (string= ""
                        (buffer-substring-no-properties
                         (car bounds) (cdr bounds)))))))

(ert-deftest resumel-test-mode-keyword-capf-exit-inserts-colon ()
  "Keyword CAPF exit-function inserts ': ' after the completed keyword."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_COMPILER"
    (resumel-mode 1)
    (goto-char (point-max))
    (let* ((capf (resumel-mode-resumel-keyword-completion-at-point))
           (exit-fn (plist-get (nthcdr 3 capf) :exit-function)))
      (should exit-fn)
      (funcall exit-fn "COMPILER" 'finished)
      (should (string-match-p "COMPILER: $"
                               (buffer-substring-no-properties
                                (line-beginning-position) (point)))))))

;;; ---------------------------------------------------------------------------
;;; resumel-init scaffold
;;; ---------------------------------------------------------------------------

(ert-deftest resumel-test-init-inserts-moderncv-scaffold ()
  "resumel-init inserts moderncv boilerplate."
  (resumel-test-with-org-buffer ""
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) t)))
      (let ((resumel-default-template "moderncv"))
        (resumel-init)
        (let ((content (buffer-string)))
          (should (string-match-p "RESUMEL_TEMPLATE: moderncv" content))
          (should (string-match-p "MODERNCV_COLOR" content))
          (should (string-match-p "MODERNCV_STYLE" content))
          (should (string-match-p "EXPORT_FILE_NAME: resumel-resume.pdf" content))
          (should (string-match-p "\\* Summary" content))
          (should (string-match-p "{{{entries-begin}}}" content))
          (should resumel-mode))))))

(ert-deftest resumel-test-init-inserts-jakes-scaffold ()
  "resumel-init inserts jakes boilerplate (minimal)."
  (resumel-test-with-org-buffer ""
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) t)))
      (let ((resumel-default-template "jakes"))
        (resumel-init)
        (let ((content (buffer-string)))
          (should (string-match-p "RESUMEL_TEMPLATE: jakes" content))
          (should-not (string-match-p "GEOMETRY" content))
          (should (string-match-p "EXPORT_FILE_NAME: resumel-resume.pdf" content)))))))

(ert-deftest resumel-test-init-fallback-for-unknown-template ()
  "resumel-init produces minimal scaffold for an unknown template."
  (resumel-test-with-org-buffer ""
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) t)))
      (let ((resumel-default-template "unknown-tmpl"))
        (resumel-init)
        (let ((content (buffer-string)))
          (should (string-match-p "RESUMEL_TEMPLATE: unknown-tmpl" content))
          (should (string-match-p "EXPORT_FILE_NAME: resumel-resume.pdf" content)))))))

;;; ---------------------------------------------------------------------------
;;; resumel-view-export
;;; ---------------------------------------------------------------------------

(ert-deftest resumel-test-view-export-requires-org ()
  "`resumel-view-export' is only valid in Org buffers."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (resumel-view-export))))

(ert-deftest resumel-test-view-export-missing-pdf ()
  "`resumel-view-export' signals when the PDF file is absent."
  (resumel-test-with-org-buffer
      "#+TITLE: T\n#+EXPORT_FILE_NAME: resumel-ert-view-export-missing.pdf\n"
    (let ((default-directory (temporary-file-directory)))
      (should-error (resumel-view-export) :type 'user-error))))

(ert-deftest resumel-test-mode-map-view-and-show-variables-keys ()
  "`C-c ,' submap binds v to view export and s to show all variables."
  (should (eq (lookup-key resumel-mode-command-map "v") #'resumel-view-export))
  (should (eq (lookup-key resumel-mode-command-map "s") #'resumel-show-all-variables)))

;;; ---------------------------------------------------------------------------
;;; resumel-install-templates
;;; ---------------------------------------------------------------------------

(ert-deftest resumel-test-install-jobs-dedupe-altacv ()
  "`altacv' and `modaltacv' map to a single AltaCV clone job."
  (let ((jobs (resumel-install-jobs-for-templates '("altacv" "modaltacv"))))
    (should (equal (length jobs) 1))
    (should (eq (nth 0 (car jobs)) 'altacv))))

(ert-deftest resumel-test-install-jobs-all-installable ()
  "All non-jakes templates yield three distinct clone jobs."
  (let ((jobs (resumel-install-jobs-for-templates
               '("moderncv" "altacv" "modaltacv" "awesomecv"))))
    (should (equal (length jobs) 3))
    (should (equal (mapcar #'car jobs) '(moderncv altacv awesomecv)))))

(ert-deftest resumel-test-install-job-target-dir ()
  "`resumel-install--job-target-dir' builds tex/latex paths under ROOT."
  (let ((root (expand-file-name "fake-texmf/" temporary-file-directory)))
    (should (string=
             (resumel-install--job-target-dir root "moderncv")
             (expand-file-name "tex/latex/moderncv" root)))))

(ert-deftest resumel-test-install-jakes-excluded ()
  "`jakes' never appears in installable templates."
  (should-not (member "jakes" (resumel-install-installable-templates))))

(ert-deftest resumel-test-install-jakes-maps-to-nil ()
  "`jakes' yields no install job."
  (let ((jobs (resumel-install-jobs-for-templates '("jakes"))))
    (should (null jobs))))

(ert-deftest resumel-test-install-templates-mocked-clone ()
  "`resumel-install-templates' runs git clone with mocked subprocess (no network)."
  (let (calls
        (resumel-install-skip-executable-checks t)
        (resumel-install--process-fn
         (lambda (program &optional _infile _destination _display &rest args)
           (push (cons (file-name-nondirectory program) args) calls)
           0)))
    (let ((dir (make-temp-file "resumel-install" t)))
      (unwind-protect
          (let ((res (resumel-install-templates '("moderncv") dir)))
            (should (assq 'moderncv res))
            (should (eq (cdr (assq 'moderncv res)) 'cloned))
            (should
             (cl-some (lambda (c)
                        (and (equal (car c) "git")
                             (equal (cadr c) "clone")))
                      calls)))
        (delete-directory dir t)))))

(ert-deftest resumel-test-install-templates-mocked-pull ()
  "Existing git dir triggers pull instead of clone."
  (let (calls
        (resumel-install-skip-executable-checks t)
        (resumel-install--process-fn
         (lambda (program &optional _infile _destination _display &rest args)
           (push (cons (file-name-nondirectory program) args) calls)
           0)))
    (let ((dir (make-temp-file "resumel-install" t)))
      (unwind-protect
          (let ((target (expand-file-name "tex/latex/moderncv" dir)))
            (make-directory target t)
            (make-directory (expand-file-name ".git" target))
            (let ((res (resumel-install-templates '("moderncv") dir)))
              (should (assq 'moderncv res))
              (should (eq (cdr (assq 'moderncv res)) 'pulled))
              (should
               (cl-some (lambda (c)
                          (and (equal (car c) "git")
                               (member "pull" c)))
                        calls))))
        (delete-directory dir t)))))

(ert-deftest resumel-test-install-templates-non-git-dir-errors ()
  "Non-git existing directory is caught as a failed job."
  (let ((resumel-install-skip-executable-checks t)
        (resumel-install--process-fn
         (lambda (_prog &optional _i _d _disp &rest _args) 0)))
    (let ((dir (make-temp-file "resumel-install" t)))
      (unwind-protect
          (let ((target (expand-file-name "tex/latex/moderncv" dir)))
            (make-directory target t)
            (let ((res (resumel-install-templates '("moderncv") dir)))
              (should (assq 'moderncv res))
              (should (consp (cdr (assq 'moderncv res))))
              (should (eq (car (cdr (assq 'moderncv res))) 'failed))))
        (delete-directory dir t)))))

(ert-deftest resumel-test-install-templates-mktexlsr-called ()
  "mktexlsr is invoked after a successful clone."
  (let (calls
        (resumel-install-skip-executable-checks t)
        (resumel-install--process-fn
         (lambda (program &optional _infile _destination _display &rest args)
           (push (cons (file-name-nondirectory program) args) calls)
           0)))
    (let ((dir (make-temp-file "resumel-install" t)))
      (unwind-protect
          (progn
            (resumel-install-templates '("moderncv") dir)
            (should
             (cl-some (lambda (c) (equal (car c) "mktexlsr")) calls)))
        (delete-directory dir t)))))

(ert-deftest resumel-test-install-mode-map-key ()
  "`C-c , u' is bound to `resumel-install-templates'."
  (should (eq (lookup-key resumel-mode-command-map "u")
              #'resumel-install-templates)))
