(require 'ert)
(require 'resumel)

(defvar resumel-test-dir (file-name-directory (or load-file-name buffer-file-name)))
(defvar resumel-fixture-dir (expand-file-name "fixtures" resumel-test-dir))
(defvar resumel-expected-dir (expand-file-name "expected" resumel-test-dir))
(defvar resumel-results-dir (expand-file-name "fixtures" resumel-test-dir))

(defun resumel-test-export-org-to-pdf (org-file)
  "Export ORG-FILE to PDF and return the PDF filename."
  (let* ((base-name (file-name-base org-file))
         (pdf-file (expand-file-name (concat base-name ".pdf") resumel-results-dir))
         (output-dir resumel-results-dir))
    ;; Debug output
    ;; (message "Exporting: %s" org-file)
    ;; (message "Output PDF: %s" pdf-file)
    ;; (message "Output directory: %s" output-dir)

    ;; Ensure output directory exists
    (make-directory output-dir t)
    (with-current-buffer (find-file-noselect org-file)
      ;; Debug current buffer
      ;; (message "Current buffer: %s" (buffer-name))

      ;; Set LaTeX export settings
      (setq-local org-latex-pdf-process
                  '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -output-directory=%o %f"))
      (setq-local org-latex-output-directory output-dir)

      ;; Debug export settings
      ;; (message "LaTeX process: %s" org-latex-pdf-process)
      ;; (message "Output directory set to: %s" org-latex-output-directory)

      ;; Export to PDF and capture any error details
      (condition-case err
          (org-latex-export-to-pdf)
        (error
         (message "Export error: %S" err)
         (when (get-buffer "/Org PDF LaTeX Output/")
           (with-current-buffer "/Org PDF LaTeX Output/"
             (message "LaTeX Output:\n%s" (buffer-string))))
         (signal (car err) (cdr err)))))
    pdf-file))

(defun resumel-files-equal-p (file1 file2)
  "Compare FILE1 and FILE2 using diff-pdf tool."
  (zerop (call-process "diff-pdf" nil nil nil file1 file2)))

(ert-deftest resumel-test-basic-export ()
  "Test basic resume export."
  (message "\nStarting basic export test...")
  (let* ((org-file (expand-file-name "basic.org" resumel-fixture-dir))
         (generated-pdf (resumel-test-export-org-to-pdf org-file))
         (expected-pdf (expand-file-name "basic.pdf" resumel-expected-dir)))

    ;; Debug output
    ;; (message "Testing PDF paths:")
    ;; (message "Generated: %s" generated-pdf)
    ;; (message "Expected: %s" expected-pdf)

    ;; Verify files exist
    (should (file-exists-p generated-pdf))
    (should (file-exists-p expected-pdf))

    ;; Compare PDFs
    (should (resumel-files-equal-p generated-pdf expected-pdf))))

(ert-deftest resumel-test-complex-export ()
  "Test complex resume export."
  (message "\nStarting complex export test...")
  (let* ((org-file (expand-file-name "complex.org" resumel-fixture-dir))
         (generated-pdf (resumel-test-export-org-to-pdf org-file))
         (expected-pdf (expand-file-name "complex.pdf" resumel-expected-dir)))

    ;; Debug output
    ;; (message "Testing PDF paths:")
    ;; (message "Generated: %s" generated-pdf)
    ;; (message "Expected: %s" expected-pdf)

    ;; Verify files exist
    (should (file-exists-p generated-pdf))
    (should (file-exists-p expected-pdf))

    ;; Compare PDFs
    (should (resumel-files-equal-p generated-pdf expected-pdf))))
