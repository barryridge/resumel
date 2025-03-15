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
    ("modaltacv-complex.org" "modaltacv-complex.pdf"))
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
