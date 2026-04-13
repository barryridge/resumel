;;; resumel-install.el --- Install upstream LaTeX class repos into a texmf tree -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Barry Ridge
;;
;; Author: Barry Ridge <barry@barr.ai>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Clones moderncv, AltaCV, and Awesome-CV into `resumel-template-install-root'
;;  (default ~/texmf) under tex/latex/, matching the layout used in CI.
;;  The jakes template uses the standard article class only; no clone.
;;
;;; Code:

(require 'resumel)
(require 'cl-lib)

(defgroup resumel-install nil
  "Install upstream LaTeX CV classes for resumel."
  :group 'resumel
  :prefix "resumel-install-")

(defcustom resumel-template-install-root
  (expand-file-name "~/texmf")
  "Root directory for TeX user files (often TEXMFHOME).
Clones go under `tex/latex/<name>/' beneath this directory."
  :type 'directory
  :group 'resumel-install)

(defvar resumel-install--process-fn #'call-process
  "Function like `call-process' (PROGRAM nil nil nil &rest ARGS).
Bound in tests to record or stub subprocesses.")

(defvar resumel-install-skip-executable-checks nil
  "When non-nil, do not require git on `exec-path' (for tests).")

(defvar resumel-install--repo-specs
  '((moderncv . ("https://github.com/moderncv/moderncv.git" . "moderncv"))
    (altacv . ("https://github.com/liantze/AltaCV.git" . "altacv"))
    (awesomecv . ("https://github.com/posquit0/Awesome-CV.git" . "awesomecv")))
  "Alist mapping install job id to (GIT-URL . RELATIVE-DIR-UNDER-tex/latex).")

(defun resumel-install--template->job-id (template)
  "Return install job id for resumel template id TEMPLATE, or nil for jakes."
  (pcase template
    ("moderncv" 'moderncv)
    ("altacv" 'altacv)
    ("modaltacv" 'altacv)
    ("awesomecv" 'awesomecv)
    ("jakes" nil)
    (_ (error "Unknown resumel template: %S" template))))

(defun resumel-install-installable-templates ()
  "Return template ids that need an upstream LaTeX class clone."
  (seq-remove (lambda (id) (member id '("jakes"))) resumel-template-names))

(defun resumel-install-jobs-for-templates (templates)
  "Return unique install jobs for TEMPLATES (template id strings).
Each job is (JOB-ID URL REL-DIR); REL-DIR is the name under tex/latex/."
  (let ((seen (make-hash-table :test 'eq))
        jobs)
    (dolist (tmpl templates)
      (when-let ((jid (resumel-install--template->job-id tmpl)))
        (unless (gethash jid seen)
          (puthash jid t seen)
          (let* ((spec (cdr (assq jid resumel-install--repo-specs)))
                 (url (car spec))
                 (rel (cdr spec)))
            (unless spec
              (error "Missing repo spec for job %S" jid))
            (push (list jid url rel) jobs)))))
    (nreverse jobs)))

(defun resumel-install--job-target-dir (root rel)
  "Return expanded tex/latex/REL under ROOT."
  (expand-file-name (concat "tex/latex/" rel) (directory-file-name (expand-file-name root))))

(defun resumel-install--git-dir-p (path)
  "Non-nil if PATH is a git working tree (has .git file or directory)."
  (let ((git (expand-file-name ".git" path)))
    (or (file-directory-p git) (file-regular-p git))))

(defun resumel-install--run-mktexlsr (root)
  "Run mktexlsr on ROOT. Return exit code, or 0 if mktexlsr is missing."
  (if-let ((exe (executable-find "mktexlsr")))
      (funcall resumel-install--process-fn exe nil nil nil root)
    (message "resumel-install: mktexlsr not found; run it manually on %S" root)
    0))

(defun resumel-install--clone (url path)
  "Clone URL into PATH (must not exist). Return t on success."
  (zerop (apply resumel-install--process-fn
                (or (executable-find "git") "git")
                nil nil nil
                (list "clone" "--depth" "1" url path))))

(defun resumel-install--pull (path)
  "Run git pull in PATH. Return t on success."
  (zerop (apply resumel-install--process-fn
                (or (executable-find "git") "git")
                nil nil nil
                (list "-C" path "pull" "--ff-only"))))

(defun resumel-install--read-templates-interactive ()
  "Read one or more installable template names from the minibuffer."
  (let ((choices (resumel-install-installable-templates)))
    (cond
     ((fboundp 'completing-read-multiple)
      (let ((picked (completing-read-multiple
                     "Templates to install (comma-separated; empty = all): "
                     choices nil t)))
        (if picked
            (cl-delete-duplicates picked :test #'string=)
          (copy-sequence choices))))
     (t
      (let (acc done)
        (while (not done)
          (let ((one (completing-read
                      (if acc "Another template (empty to finish): " "Template (empty = all): ")
                      (cons "" choices)
                      nil t)))
            (cond
             ((string-empty-p one)
              (setq done t)
              (unless acc (setq acc (copy-sequence choices))))
             (t
              (push one acc)))))
        (cl-delete-duplicates acc :test #'string=))))))

;;;###autoload
(defun resumel-install-templates (&optional templates root)
  "Install upstream LaTeX class repos for selected resumel templates.

TEMPLATES is a list of template id strings (e.g. \"moderncv\") or nil
meaning all installable templates (everything except jakes).

ROOT is the texmf root directory; nil uses `resumel-template-install-root'.

When called interactively, prompts for ROOT and which templates to install.
Existing git directories: asks whether to pull or skip each.

Returns an alist of (JOB-ID . STATUS) where STATUS is one of:
`cloned', `pulled', `skipped', or (`failed' . ERROR-STRING)."
  (interactive)
  (unless resumel-install-skip-executable-checks
    (unless (executable-find "git")
      (user-error "git is not on `exec-path'; install git to use this command")))
  (let* ((interactive-p (called-interactively-p 'interactive))
         (root (directory-file-name
                (expand-file-name (or root resumel-template-install-root))))
         (tpls
          (if interactive-p
              (progn
                (setq root (directory-file-name
                            (expand-file-name
                             (read-directory-name "TeX user tree (texmf root): " root nil t))))
                (resumel-install--read-templates-interactive))
            (or templates (resumel-install-installable-templates))))
         results
         changedp)
    (unless tpls
      (when interactive-p (message "resumel-install-templates: no templates selected"))
      (user-error "No templates selected"))
    (dolist (tmpl tpls)
      (unless (member tmpl resumel-template-names)
        (user-error "Unknown template %S" tmpl)))
    (let ((jobs (resumel-install-jobs-for-templates tpls)))
      (make-directory (expand-file-name "tex/latex" root) t)
      (dolist (job jobs)
        (let* ((jid (nth 0 job))
               (url (nth 1 job))
               (rel (nth 2 job))
               (path (resumel-install--job-target-dir root rel)))
          (condition-case err
              (cond
               ((not (file-exists-p path))
                (if (resumel-install--clone url path)
                    (progn
                      (setq changedp t)
                      (push (cons jid 'cloned) results))
                  (push (cons jid `(failed . "git clone failed")) results)))
               ((resumel-install--git-dir-p path)
                (let ((do-pull
                       (if interactive-p
                           (yes-or-no-p (format "Update existing clone at %s? " path))
                         t)))
                  (if do-pull
                      (if (resumel-install--pull path)
                          (progn
                            (setq changedp t)
                            (push (cons jid 'pulled) results))
                        (push (cons jid `(failed . "git pull failed")) results))
                    (push (cons jid 'skipped) results))))
               (t
                (user-error "Path exists and is not a git clone: %s" path)))
            (error
             (push (cons jid `(failed . ,(error-message-string err))) results))))))
    (when changedp
      (unless (zerop (resumel-install--run-mktexlsr root))
        (message "resumel-install: mktexlsr returned non-zero for %S" root)))
    (setq results (nreverse results))
    (when interactive-p
      (message "resumel-install-templates: %S" results))
    results))

(provide 'resumel-install)

;;; resumel-install.el ends here
