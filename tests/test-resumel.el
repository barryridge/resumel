(require 'ert)
(require 'resumel)

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
            ;; Call resumel-setup before exporting
            (resumel-setup)
            ;; Export to PDF
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

;; Function to compare two PDFs using diff-pdf
(defun resumel-files-equal-p (file1 file2)
  "Compare FILE1 and FILE2 using diff-pdf tool with specified tolerances."
  (let ((channel-tolerance (or (getenv "DIFF_PDF_CHANNEL_TOLERANCE") "0"))
        (per-page-pixel-tolerance (or (getenv "DIFF_PDF_PER_PAGE_PIXEL_TOLERANCE") "0")))
    (zerop (call-process "diff-pdf" nil nil nil
                         "--channel-tolerance" channel-tolerance
                         "--per-page-pixel-tolerance" per-page-pixel-tolerance
                         file1 file2))))

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
    ("awesomecv-complex.org" "awesomecv-complex.pdf"))
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

(ert-deftest resumel-test-show-all-variables ()
  "resumel-show-all-variables populates a variables buffer with both sections."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_MODERNCV_COLOR: orange\n#+TITLE: Test\n"
    (resumel-show-all-variables)
    (let ((buf (get-buffer "*resumel: template variables*")))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match "moderncv" content))
          (should (string-match "CORE VARIABLES" content))
          (should (string-match "TEMPLATE-SPECIFIC" content))
          (should (string-match "#\\+RESUMEL_COMPILER:" content))
          (should (string-match "#\\+RESUMEL_MODERNCV_COLOR: orange" content)))))))

(ert-deftest resumel-test-show-core-variables ()
  "resumel-show-core-variables shows only the core section."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+TITLE: Test\n"
    (resumel-show-core-variables)
    (let ((buf (get-buffer "*resumel: template variables*")))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match "CORE VARIABLES" content))
          (should (string-match "#\\+RESUMEL_COMPILER:" content))
          ;; Template-specific section should NOT appear
          (should-not (string-match "TEMPLATE-SPECIFIC" content))
          (should-not (string-match "MODERNCV_COLOR" content)))))))

(ert-deftest resumel-test-show-template-variables ()
  "resumel-show-template-variables shows only the template-specific section."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+TITLE: Test\n"
    (resumel-show-template-variables)
    (let ((buf (get-buffer "*resumel: template variables*")))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match "TEMPLATE-SPECIFIC" content))
          (should (string-match "MODERNCV_COLOR" content))
          ;; Core section should NOT appear
          (should-not (string-match "CORE VARIABLES" content))
          (should-not (string-match "#+RESUMEL_COMPILER:" content)))))))

(ert-deftest resumel-test-show-variables-is-alias ()
  "resumel-show-variables is an alias for resumel-show-all-variables."
  (should (fboundp 'resumel-show-variables))
  (should (eq (indirect-function 'resumel-show-variables)
              (indirect-function 'resumel-show-all-variables))))

;; ---- resumel-variables-mode live sync ---------------------------------------

(ert-deftest resumel-test-variables-mode-live-sync ()
  "Editing a #+RESUMEL_VAR line in the variables buffer syncs to the Org buffer."
  (resumel-test-with-org-buffer
      "#+RESUMEL_TEMPLATE: moderncv\n#+RESUMEL_MODERNCV_COLOR: blue\n#+TITLE: Test\n"
    (let ((org-buf (current-buffer)))
      (resumel-show-all-variables)
      (let ((vars-buf (get-buffer "*resumel: template variables*")))
        (should vars-buf)
        ;; Edit the MODERNCV_COLOR line in the variables buffer
        (with-current-buffer vars-buf
          (goto-char (point-min))
          (re-search-forward "^#\\+RESUMEL_MODERNCV_COLOR: ")
          (delete-region (point) (line-end-position))
          (insert "burgundy"))
        ;; Check that the Org buffer was updated
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
  (should (member "CVTAG_CORNER_DEFAULT" resumel-core-variable-names)))
