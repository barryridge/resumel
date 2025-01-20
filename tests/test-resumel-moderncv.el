(require 'ert)
(require 'resumel)

;; Define test directories
(defvar resumel-test-dir (file-name-directory (or load-file-name buffer-file-name))
  "Root directory for resumel tests.")

(defvar resumel-fixture-dir (expand-file-name "fixtures" resumel-test-dir)
  "Directory containing fixture Org files for testing.")

(defvar resumel-expected-dir (expand-file-name "expected" resumel-test-dir)
  "Directory containing expected PDF outputs for testing.")

(defvar resumel-results-dir (expand-file-name "fixtures" resumel-test-dir)
  "Directory where test-generated PDF results are stored.")

;; Function to export Org to PDF
(defun resumel-test-export-org-to-pdf (org-file)
  "Export ORG-FILE to PDF and return the PDF filename."
  (let* ((base-name (file-name-base org-file))
         (output-dir resumel-results-dir)
         (tex-file (expand-file-name (concat base-name ".tex") output-dir))
         (pdf-file (expand-file-name (concat base-name ".pdf") output-dir)))
    ;; Debug output
    (message "Exporting: %s" org-file)
    (message "Output TEX: %s" tex-file)
    (message "Output PDF: %s" pdf-file)
    (message "Output directory: %s" output-dir)

    ;; Ensure output directory exists
    (make-directory output-dir t)
    (with-current-buffer (find-file-noselect org-file)
      ;; Set LaTeX export settings
      (setq-local org-latex-pdf-process
                  (list (format "latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -output-directory=%s %%f" output-dir)))
      ;; Do NOT set org-latex-output-directory here, as it only affects auxiliary files.

      ;; Export to PDF and capture any error details
      (condition-case err
          (progn
            ;; Pass the output .tex file via the EXT-PLIST parameter
            (org-latex-export-to-pdf nil nil nil nil (list :output-file tex-file))
            (unless (file-exists-p pdf-file)
              (message "LaTeX Output:\n%s" (with-current-buffer "*Org PDF LaTeX Output*" (buffer-string)))
              (error "File \"%s\" wasn't produced. See \"*Org PDF LaTeX Output*\" for details" pdf-file))
            pdf-file)  ; Return pdf-file inside let*
        (error
         (message "Export error: %S" err)
         (when (get-buffer "*Org PDF LaTeX Output*")
           (with-current-buffer "*Org PDF LaTeX Output*"
             (message "LaTeX Output:\n%s" (buffer-string))))
         (signal (car err) (cdr err)))))))

;; Function to compare two PDFs using diff-pdf
(defun resumel-files-equal-p (file1 file2)
  "Compare FILE1 and FILE2 using diff-pdf tool."
  (zerop (call-process "diff-pdf" nil nil nil file1 file2)))

;; Setup function to load the moderncv template
(defun resumel-test-setup ()
  "Setup resumel with the moderncv template for testing."
  (resumel-select-template "moderncv")
  (resumel-setup))

;; Run setup before tests
(resumel-test-setup)

;; Test for basic export
(ert-deftest resumel-test-basic-export ()
  "Test basic resume export."
  (let* ((org-file (expand-file-name "moderncv-basic.org" resumel-fixture-dir))
         (generated-pdf (resumel-test-export-org-to-pdf org-file))
         (expected-pdf (expand-file-name "moderncv-basic.pdf" resumel-expected-dir)))
    ;; Debug output
    (message "Testing PDF paths:")
    (message "Generated: %s" generated-pdf)
    (message "Expected: %s" expected-pdf)
    ;; Verify files exist
    (should (file-exists-p generated-pdf))
    (should (file-exists-p expected-pdf))
    ;; Compare PDFs
    (should (resumel-files-equal-p generated-pdf expected-pdf))))

;; Test for complex export
(ert-deftest resumel-test-complex-export ()
  "Test complex resume export."
  (let* ((org-file (expand-file-name "moderncv-complex.org" resumel-fixture-dir))
         (generated-pdf (resumel-test-export-org-to-pdf org-file))
         (expected-pdf (expand-file-name "moderncv-complex.pdf" resumel-expected-dir)))
    ;; Debug output
    (message "Testing PDF paths:")
    (message "Generated: %s" generated-pdf)
    (message "Expected: %s" expected-pdf)
    ;; Verify files exist
    (should (file-exists-p generated-pdf))
    (should (file-exists-p expected-pdf))
    ;; Compare PDFs
    (should (resumel-files-equal-p generated-pdf expected-pdf))))
