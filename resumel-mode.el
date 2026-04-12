;;; resumel-mode.el --- Minor mode for resumel Org resume buffers -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Barry Ridge
;;
;; Author: Barry Ridge <barry@barr.ai>
;; Maintainer: Barry Ridge <barry@barr.ai>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides completion-at-point for Org macro calls {{{...}}}, optional
;;  Eldoc, completion for #+RESUMEL_* keyword names, and a scaffold
;;  command (`resumel-init') for new resume files.  The mode auto-enables
;;  in Org buffers that contain a #+RESUMEL_TEMPLATE: keyword.
;;
;;; Code:

(require 'org)
(require 'subr-x)
(require 'resumel)

;; --- Customization ---------------------------------------------------------

(defcustom resumel-mode-auto-enable nil
  "When non-nil, turn on `resumel-mode' in every new `org-mode' buffer.
When nil the mode still auto-enables in buffers that contain a
#+RESUMEL_TEMPLATE: keyword.  Set this to t to enable in *all* Org
buffers regardless."
  :type 'boolean
  :group 'resumel)

(defcustom resumel-mode-macro-annotation-max-length 56
  "Maximum length of macro expansion text shown in completion annotations."
  :type 'integer
  :group 'resumel)

(defcustom resumel-mode-eldoc t
  "When non-nil, show macro / RESUMEL keyword hints in the echo area via Eldoc."
  :type 'boolean
  :group 'resumel)

;; --- Variables / constants -------------------------------------------------

(defvar resumel-mode--macro-descriptions
  '(;; -- shared formatting (resumel.org) --
    ("bf"           . "bold text: {{{bf(text)}}}")
    ("it"           . "italic/emphasis: {{{it(text)}}}")
    ("href"         . "hyperlink: {{{href(URL,label)}}}")
    ("amp"          . "literal & (LaTeX table column separator)")
    ("endl"         . "literal \\\\ (LaTeX line break in tables)")
    ("divider"      . "horizontal divider with surrounding paragraphs")
    ("par-div"      . "paragraph + divider (no trailing \\par)")
    ("tightdiv"     . "divider with symmetric vspace: {{{tightdiv(-1ex)}}}")
    ("tighterdiv"   . "divider with asymmetric vspace: {{{tighterdiv(-2ex,1ex)}}}")
    ("new-page"     . "force a new page")
    ("vspace"       . "vertical space: {{{vspace(1cm)}}}")
    ("hspace"       . "horizontal space: {{{hspace(1cm)}}}")
    ("linebreak"    . "suggest a line break")
    ("pagebreak"    . "suggest a page break")
    ("cal"          . "calendar symbol (\\calendarsymbol)")
    ("pin"          . "address/pin symbol (\\addresssymbol)")
    ("fa"           . "Font Awesome icon: {{{fa(IconName,\\size)}}}")
    ("defcolor"     . "define color: {{{defcolor(name,model,spec)}}}")
    ("colorlet"     . "alias a color: {{{colorlet(name,color)}}}")
    ("resumel-color-samples" . "show Accent/Body/Muted color samples")
    ;; -- shared CV macros (resumel.org) --
    ("tag"          . "single skill tag: {{{tag(Python)}}}")
    ("ltag"         . "skill tag with level: {{{ltag(Python,5)}}}")
    ("tags"         . "variadic skill tags: {{{tags(Python,Emacs,LaTeX)}}}")
    ("ltags"        . "skill+level pairs: {{{ltags(Python,5,Emacs,4)}}}")
    ("wheelchart"   . "skill wheel: {{{wheelchart(outer,inner, val,w,color,label, ...)}}}")
    ;; -- unified portable macros (all templates) --
    ("item"         . "key-value item: {{{item(key,value)}}}")
    ("double"       . "two-column item: {{{double(k1,v1,k2,v2)}}}")
    ("itemc"        . "item with comment: {{{itemc(key,value,comment)}}}")
    ("listdouble"   . "two-column list row: {{{listdouble(left,right)}}}")
    ("entries-begin" . "open an entries block")
    ("entries-end"  . "close an entries block")
    ("entry"        . "experience entry: {{{entry(title,org,loc,dates,desc)}}}")
    ("achievement"  . "achievement: {{{achievement(icon,title,details)}}}")
    ("reference"    . "reference: {{{reference(name,org,email)}}}")
    ("skills-begin" . "open a skills block")
    ("skills-end"   . "close a skills block")
    ("skill"        . "skill rating: {{{skill(name,level)}}}")
    ;; -- common template-specific macros --
    ("cvevent"      . "CV event (template-specific entry variant)")
    ("cventry"      . "CV entry (template-specific)")
    ("cvitem"       . "CV item (template-specific)")
    ("cvskill"      . "CV skill (template-specific)")
    ("cvachievement" . "CV achievement (template-specific)")
    ("cvproject"    . "CV project (template-specific)")
    ("cvthesis"     . "CV thesis (template-specific)")
    ("cvref"        . "CV reference (template-specific)")
    ("cvaward"      . "CV award (template-specific)")
    ("cvhonor"      . "CV honor entry (awesomecv)")
    ("cvhonors-begin" . "open an honors block")
    ("cvhonors-end" . "close an honors block")
    ("cvsection"    . "CV section heading (template-specific)")
    ("cvsubsection" . "CV subsection heading (template-specific)")
    ("beginparacol" . "begin two-column layout")
    ("endparacol"   . "end two-column layout")
    ("switchcol"    . "switch to next column in paracol")
    ("fa-basic-icons" . "Font Awesome icon sampler")
    ("fa-briefcase-colors" . "briefcase icon in various colors")
    ("fa-briefcase-sizes-colors" . "briefcase icon in various sizes and colors"))
  "Human-readable descriptions for known resumel macros.
Used by completion annotations and Eldoc.")

(defvar resumel-mode--eval-macro-help
  '(("tags" . "variadic: skill strings -> \\cvtag{...}")
    ("ltags" . "variadic: skill level pairs -> \\cvtag{...}[level]")
    ("wheelchart" . "variadic: outer inner, then value/width/color/detail …"))
  "Brief help strings for Org macros whose body is `(eval …)'.")

(defvar resumel-mode--merged-file-macros-cache nil
  "A cons (CACHE-KEY . ALIST) for `resumel-mode--merged-file-macros'.")

;; --- Index builder ---------------------------------------------------------

(defun resumel-mode--file-mtime (file)
  "Return modification time for FILE, or nil if unreadable."
  (when (file-readable-p file)
    (let ((attrs (file-attributes file)))
      (if (fboundp 'file-attribute-modification-time)
          (file-attribute-modification-time attrs)
        (nth 5 attrs)))))

(defun resumel-mode--parse-macros-from-buffer ()
  "Parse #+MACRO lines in the current buffer; return alist (NAME . EXPANSION)."
  (let (result)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward
                "^[ \t]*#\\+MACRO:[ \t]+\\([^ \t\n]+\\)[ \t]+\\(.*\\)[ \t]*$"
                nil t)
          (push (cons (match-string-no-properties 1)
                      (string-trim (match-string-no-properties 2)))
                result))))
    (nreverse result)))

(defun resumel-mode--read-macros-from-file (file)
  "Read FILE and return its #+MACRO alist, or nil if FILE is missing."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (resumel-mode--parse-macros-from-buffer))))

(defun resumel-mode--merge-macro-alists (low high)
  "Merge two (NAME . EXPANSION) alists; entries in HIGH shadow LOW.
Returns a new alist sorted by NAME."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair low)
      (unless (gethash (car pair) table)
        (puthash (car pair) (cdr pair) table)))
    (dolist (pair high)
      (puthash (car pair) (cdr pair) table))
    (let (keys)
      (maphash (lambda (k _v) (push k keys)) table)
      (mapcar (lambda (k) (cons k (gethash k table)))
              (sort keys #'string<)))))

(defun resumel-mode--file-macro-cache-key (template)
  "Cache key for merged file macros for TEMPLATE."
  (list template
        (resumel-mode--file-mtime
         (expand-file-name "resumel.org" resumel-base-dir))
        (resumel-mode--file-mtime
         (expand-file-name (format "%s/%s.org" template template)
                           resumel-templates-dir))))

(defun resumel-mode--merged-file-macros (template)
  "Merge `resumel.org' and templates/TEMPLATE/TEMPLATE.org; template wins."
  (let* ((key (resumel-mode--file-macro-cache-key template))
         (hit (and resumel-mode--merged-file-macros-cache
                   (equal (car resumel-mode--merged-file-macros-cache) key))))
    (if hit
        (cdr resumel-mode--merged-file-macros-cache)
      (let* ((shared (resumel-mode--read-macros-from-file
                      (expand-file-name "resumel.org" resumel-base-dir)))
             (tmpl (resumel-mode--read-macros-from-file
                    (expand-file-name (format "%s/%s.org" template template)
                                     resumel-templates-dir)))
             (merged (resumel-mode--merge-macro-alists
                      (or shared nil) (or tmpl nil))))
        (setq resumel-mode--merged-file-macros-cache (cons key merged))
        merged))))

(defun resumel-mode--macro-index (&optional buffer)
  "Return merged macro alist for BUFFER (merged files + buffer #+MACRO).
Buffer-local definitions override file-based ones."
  (let* ((buf (or buffer (current-buffer)))
         (template (with-current-buffer buf (resumel--get-buffer-template)))
         (files (resumel-mode--merged-file-macros template))
         (local (with-current-buffer buf
                  (resumel-mode--parse-macros-from-buffer))))
    (resumel-mode--merge-macro-alists files local)))

;; --- Helpers ---------------------------------------------------------------

(defun resumel-mode--truncate-string (s max)
  "Truncate S to MAX characters with ellipsis."
  (if (<= (length s) max)
      s
    (concat (substring s 0 (max 0 (- max 3))) "...")))

(defun resumel-mode--macro-help (name expansion)
  "Return a help string for macro NAME with EXPANSION.
Prefers the hand-written description from
`resumel-mode--macro-descriptions', then eval-macro help,
then infers arity from $N placeholders."
  (or (cdr (assoc name resumel-mode--macro-descriptions))
      (cdr (assoc name resumel-mode--eval-macro-help))
      (when (string-match-p "\\`(\\s-*eval\\>" expansion)
        "variadic (eval macro)")
      (let ((max-n 0)
            (pos 0))
        (while (string-match "\\$\\([0-9]+\\)" expansion pos)
          (setq max-n (max max-n (string-to-number (match-string 1 expansion)))
                pos (match-end 0)))
        (cond
         ((zerop max-n) "no arguments")
         ((= max-n 1) "1 argument")
         (t (format "%d arguments (comma-separated)" max-n))))))

;; --- Bounds detection ------------------------------------------------------

(defun resumel-mode--macro-name-bounds-at-point ()
  "If point is on a macro name inside {{{ … }}}, return (BEG . PT).
Return nil otherwise."
  (let ((pt (point))
        after-open beg arg-beg id-end ne)
    (save-excursion
      (save-match-data
        (when (re-search-backward "{{{" nil t)
          (setq after-open (match-end 0))
          (unless (save-excursion
                    (goto-char after-open)
                    (re-search-forward "}}}" pt t))
            (goto-char after-open)
            (skip-chars-forward " \t")
            (setq beg (point))
            (setq arg-beg
                  (save-excursion
                    (goto-char beg)
                    (when (re-search-forward "(" pt t)
                      (match-beginning 0))))
            (when (and (<= beg pt)
                       (or (null arg-beg) (< pt arg-beg)))
              (goto-char beg)
              (when (looking-at "[a-zA-Z0-9_-]*")
                (setq id-end (match-end 0))
                (setq ne (if arg-beg (min id-end arg-beg) id-end))
                (when (and (<= beg pt) (<= pt ne))
                  (cons beg pt))))))))))

(defun resumel-mode--resumel-keyword-name-bounds ()
  "If on a #+RESUMEL keyword line, return (BEG . PT) for the name portion.
Activates as soon as the user has typed #+RESUMEL (underscore not
required).  BEG is the position right after #+RESUMEL_ and PT is point."
  (let ((pt (point)))
    (save-excursion
      (save-match-data
        (beginning-of-line)
        (when (looking-at "^[ \t]*#\\+RESUMEL_?\\([A-Za-z0-9_]*\\)")
          (let ((beg (match-beginning 1))
                (end (match-end 1)))
            (when (and (<= beg pt) (<= pt end))
              (cons beg pt))))))))

;; --- Completion candidates -------------------------------------------------

(defun resumel-mode--resumel-keyword-candidates ()
  "Variable name strings (without RESUMEL_ prefix) for completion."
  (let* ((template (resumel--get-buffer-template))
         (defaults (resumel--get-template-defaults template))
         (buf-vars (resumel--get-buffer-vars))
         (names (delete-dups
                 (append (mapcar #'car defaults)
                         (mapcar #'car buf-vars)))))
    (delete-dups (cons "TEMPLATE" names))))

;; --- CAPF entries ----------------------------------------------------------

(defun resumel-mode-macro-completion-at-point ()
  "`completion-at-point-functions' entry for {{{macro}}} names."
  (when (and resumel-mode (derived-mode-p 'org-mode))
    (when-let ((bounds (resumel-mode--macro-name-bounds-at-point)))
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (idx (resumel-mode--macro-index))
             (names (mapcar #'car idx)))
        (list beg end names
              :exclusive 'no
              :annotation-function
              (lambda (name)
                (let ((exp (cdr (assoc name idx))))
                  (when exp
                    (format " — %s"
                            (resumel-mode--macro-help name exp)))))
              :company-docsig
              (lambda (name)
                (let ((exp (cdr (assoc name idx))))
                  (when exp
                    (resumel-mode--macro-help name exp)))))))))

(defun resumel-mode-resumel-keyword-completion-at-point ()
  "`completion-at-point-functions' entry for #+RESUMEL_VAR: keywords."
  (when (and resumel-mode (derived-mode-p 'org-mode))
    (when-let ((bounds (resumel-mode--resumel-keyword-name-bounds)))
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (cands (resumel-mode--resumel-keyword-candidates))
             (table (lambda (string pred action)
                      (let ((completion-ignore-case t))
                        (complete-with-action action cands string pred)))))
        (list beg end table
              :exclusive 'no
              :annotation-function
              (lambda (_name) " RESUMEL keyword")
              :exit-function
              (lambda (_candidate status)
                (when (memq status '(finished exact))
                  (unless (looking-at "[ \t]*:")
                    (insert ": ")))))))))

(defun resumel-mode--capf ()
  "Try resumel completions; first macro, then RESUMEL keyword."
  (or (resumel-mode-macro-completion-at-point)
      (resumel-mode-resumel-keyword-completion-at-point)))

;; --- Eldoc -----------------------------------------------------------------

(defun resumel-mode--eldoc (&optional callback &rest _)
  "Eldoc function for macro and RESUMEL keyword lines.
When called via `eldoc-documentation-functions' (Emacs 28+),
CALLBACK receives the documentation string.  The legacy
`eldoc-documentation-function' path ignores CALLBACK."
  (when resumel-mode-eldoc
    (let ((doc
           (or (when-let ((bounds (resumel-mode--macro-name-bounds-at-point)))
                 (let* ((name (buffer-substring-no-properties
                               (car bounds) (cdr bounds)))
                        (idx (resumel-mode--macro-index))
                        (exp (cdr (assoc name idx))))
                   (when (and name (not (string-empty-p name)) exp)
                     (format "%s: %s"
                             name
                             (resumel-mode--macro-help name exp)))))
               (when-let ((bounds (resumel-mode--resumel-keyword-name-bounds)))
                 (let ((name (buffer-substring-no-properties
                              (car bounds) (cdr bounds))))
                   (when (and name (not (string-empty-p name)))
                     (format "#+RESUMEL_%s: — template / profile keyword"
                             name)))))))
      (if callback
          (when doc (funcall callback doc))
        doc))))

;; --- Keymap ----------------------------------------------------------------

(defvar resumel-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'resumel-export)
    (define-key map (kbd "i") #'resumel-init)
    (define-key map (kbd "t") #'resumel-select-template)
    (define-key map (kbd "v") #'resumel-view-export)
    (define-key map (kbd "s") #'resumel-show-all-variables)
    map)
  "Prefix map on \\`C-c ,' for resumel commands.")

(defvar resumel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ,") resumel-mode-command-map)
    map)
  "Keymap for `resumel-mode'.")

;; --- Minor mode ------------------------------------------------------------

(define-minor-mode resumel-mode
  "Minor mode for resumel resume Org buffers.

When enabled, offers:
- completion-at-point for Org macro names after \"{{{\"
- completion for #+RESUMEL_ keyword names
- optional Eldoc hints (`resumel-mode-eldoc')
- `resumel-init' to scaffold a new resume
- \\`C-c ,' prefix: export, view PDF, template, variables, etc.

\\{resumel-mode-map}"
  :lighter " Resumel"
  :keymap resumel-mode-map
  :group 'resumel)

(defun resumel-mode--toggle-hooks ()
  "Run from `resumel-mode-hook' to install or remove CAPF and Eldoc."
  (if resumel-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (setq resumel-mode nil)
          (user-error "resumel-mode only applies in Org buffers"))
        (add-hook 'completion-at-point-functions
                  #'resumel-mode--capf nil t)
        (if (boundp 'eldoc-documentation-functions)
            (add-hook 'eldoc-documentation-functions
                      #'resumel-mode--eldoc nil t)
          (setq-local eldoc-documentation-function #'resumel-mode--eldoc)))
    (remove-hook 'completion-at-point-functions #'resumel-mode--capf t)
    (if (boundp 'eldoc-documentation-functions)
        (remove-hook 'eldoc-documentation-functions #'resumel-mode--eldoc t)
      (kill-local-variable 'eldoc-documentation-function))))

(add-hook 'resumel-mode-hook #'resumel-mode--toggle-hooks)

;; --- Auto-enable -----------------------------------------------------------

(defun resumel-mode--buffer-has-resumel-template-p ()
  "Return non-nil if the current buffer contains a #+RESUMEL_TEMPLATE: keyword."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (re-search-forward "^[ \t]*#\\+RESUMEL_TEMPLATE:" nil t))))

;;;###autoload
(defun resumel-mode--maybe-turn-on ()
  "Turn on `resumel-mode' when appropriate.
Activates if `resumel-mode-auto-enable' is non-nil, or if the
buffer contains a #+RESUMEL_TEMPLATE: keyword."
  (when (and (derived-mode-p 'org-mode)
             (or resumel-mode-auto-enable
                 (resumel-mode--buffer-has-resumel-template-p)))
    (resumel-mode 1)))

;;;###autoload
(add-hook 'org-mode-hook #'resumel-mode--maybe-turn-on)

;; --- resumel-init scaffold -------------------------------------------------

(defvar resumel-init--scaffolds
  '(("moderncv" .
     "* Config :noexport:
#+RESUMEL_TEMPLATE: moderncv
#+RESUMEL_GEOMETRY: scale=0.75, top=2cm, bottom=2cm, left=2.05cm, right=2.05cm
#+RESUMEL_MAIN_FONT_XELATEX: Latin Modern Roman
#+RESUMEL_SANS_FONT_XELATEX: Latin Modern Sans
#+RESUMEL_MAIN_FONT_PDFLATEX: lmodern
# MODERNCV_COLOR options: black, blue (default), burgundy, green, grey, orange, purple, red, cerulean
#+RESUMEL_MODERNCV_COLOR: blue
# MODERNCV_STYLE options: casual, classic (default), banking, oldstyle, fancy, contemporary
#+RESUMEL_MODERNCV_STYLE: classic
#+TITLE: Your Name
#+AUTHOR: Your Name
#+EXPORT_FILE_NAME: resumel-resume.pdf
#+CITE_EXPORT: bibtex
#+BIBLIOGRAPHY: nil
#+OPTIONS: toc:nil title:nil H:2

* Summary

Hello World! This is a starter line. Edit the sections below and run =M-x resumel-export= to build your PDF.

* Color Customization :ignore:
# colorlet: {{{colorlet(var,color)}}}
#   vars: color0, color1, color2, lastnamecolor, namecolor, headrulecolor,
#         firstnamecolor, titlecolor, addresscolor, quotecolor,
#         bodyrulecolor, sectioncolor, subsectioncolor, hintstylecolor
#   colors: black, red, darkgrey, orange, burgundy, purple, lightblue, green

* Experience
{{{entries-begin}}}
{{{entry(Job Title,Organization,Location,2020 -- Present,Description of role and accomplishments.)}}}
{{{entries-end}}}

* Education
{{{entries-begin}}}
{{{entry(Degree,University,Location,2016 -- 2020,Details.)}}}
{{{entries-end}}}

* Skills
{{{tags(Skill1,Skill2,Skill3)}}}
")
    ("altacv" .
     "* Config :noexport:
#+RESUMEL_TEMPLATE: altacv
#+RESUMEL_GEOMETRY: left=1.25cm,right=1.25cm,top=1.5cm,bottom=1.5cm,columnsep=1.2cm
#+RESUMEL_MAIN_FONT_XELATEX: Roboto Slab
#+RESUMEL_SANS_FONT_XELATEX: Lato
#+RESUMEL_MAIN_FONT_PDFLATEX: roboto
#+RESUMEL_SANS_FONT_PDFLATEX: lato
#+TITLE: Your Name
#+AUTHOR: Your Name
#+EXPORT_FILE_NAME: resumel-resume.pdf
#+OPTIONS: toc:nil title:nil H:2

* Summary

Hello World! This is a starter line. Edit the sections below and run =M-x resumel-export= to build your PDF.

* Color Customization :ignore:
# colorlet: {{{colorlet(var,color)}}}
#   vars: name, tagline, heading, headingrule, subheading, accent, emphasis, body
#   colors: Black, SlateGrey, LightGrey, DarkPastelRed, PastelRed, Blue, DarkBlue, GoldenEarth

* Experience
{{{entries-begin}}}
{{{entry(Job Title,Organization,Location,2020 -- Present,Description of role and accomplishments.)}}}
{{{entries-end}}}

* Education
{{{entries-begin}}}
{{{entry(Degree,University,Location,2016 -- 2020,Details.)}}}
{{{entries-end}}}

* Skills
{{{tags(Skill1,Skill2,Skill3)}}}
")
    ("modaltacv" .
     "* Config :noexport:
#+RESUMEL_TEMPLATE: modaltacv
#+RESUMEL_GEOMETRY: left=1.25cm,right=1.25cm,top=1.5cm,bottom=1.5cm,columnsep=1.2cm
#+RESUMEL_MAIN_FONT_XELATEX: Latin Modern Roman
#+RESUMEL_SANS_FONT_XELATEX: Latin Modern Sans
#+RESUMEL_MAIN_FONT_PDFLATEX: lmodern
#+RESUMEL_SANS_FONT_PDFLATEX: lmodern
#+TITLE: Your Name
#+AUTHOR: Your Name
#+EXPORT_FILE_NAME: resumel-resume.pdf
#+OPTIONS: toc:nil title:nil H:2

* Summary

Hello World! This is a starter line. Edit the sections below and run =M-x resumel-export= to build your PDF.

* Color Customization :ignore:
# colorlet: {{{colorlet(var,color)}}}
#   altacv vars: name, tagline, heading, headingrule, subheading, accent, emphasis, body
#   moderncv vars: color0, color1, color2
#   altacv colors: Black, SlateGrey, LightGrey, DarkPastelRed, PastelRed, Blue, DarkBlue
#   moderncv colors: black, red, darkgrey, orange, burgundy, purple, lightblue, green

* Experience
{{{entries-begin}}}
{{{entry(Job Title,Organization,Location,2020 -- Present,Description of role and accomplishments.)}}}
{{{entries-end}}}

* Education
{{{entries-begin}}}
{{{entry(Degree,University,Location,2016 -- 2020,Details.)}}}
{{{entries-end}}}

* Skills
{{{tags(Skill1,Skill2,Skill3)}}}
")
    ("awesomecv" .
     "* Config :noexport:
#+RESUMEL_TEMPLATE: awesomecv
# AWESOMECV_COLOR options: awesome-emerald, awesome-skyblue, awesome-red (default), awesome-pink, awesome-orange, awesome-nephritis, awesome-concrete, awesome-darknight
#+RESUMEL_AWESOMECV_COLOR: awesome-red
#+TITLE: Your Name
#+AUTHOR: Your Name
#+EXPORT_FILE_NAME: resumel-resume.pdf
#+OPTIONS: toc:nil title:nil H:2

* Summary

Hello World! This is a starter line. Edit the sections below and run =M-x resumel-export= to build your PDF.

* Color Customization :ignore:
# colorlet: {{{colorlet(var,color)}}}
#   vars: color0, color1, color2, darktext, text, graytext, lighttext, sectiondivider
#   colors: white, black, darkgray, gray, lightgray, green, orange, purple, red, blue,
#           awesome-emerald, awesome-skyblue, awesome-red, awesome-pink, awesome-orange,
#           awesome-nephritis, awesome-concrete, awesome-darknight

* Experience
{{{entries-begin}}}
{{{entry(Job Title,Organization,Location,2020 -- Present,Description of role and accomplishments.)}}}
{{{entries-end}}}

* Education
{{{entries-begin}}}
{{{entry(Degree,University,Location,2016 -- 2020,Details.)}}}
{{{entries-end}}}

* Skills
{{{tags(Skill1,Skill2,Skill3)}}}
")
    ("jakes" .
     "* Config :noexport:
#+RESUMEL_TEMPLATE: jakes
#+TITLE: Your Name
#+AUTHOR: Your Name
#+EXPORT_FILE_NAME: resumel-resume.pdf
#+OPTIONS: toc:nil H:2

* Summary

Hello World! This is a starter line. Edit the sections below and run =M-x resumel-export= to build your PDF.

* Experience
{{{entries-begin}}}
{{{entry(Job Title,Organization,Location,2020 -- Present,Description of role and accomplishments.)}}}
{{{entries-end}}}

* Education
{{{entries-begin}}}
{{{entry(Degree,University,Location,2016 -- 2020,Details.)}}}
{{{entries-end}}}

* Skills
{{{tags(Skill1,Skill2,Skill3)}}}
"))
  "Per-template scaffold strings for `resumel-init'.")

;;;###autoload
(defun resumel-init ()
  "Initialize the current buffer as a resumel resume.
Prompts for a template and inserts boilerplate configuration,
section headings, and example macros.  If the buffer already has
content, asks for confirmation before erasing it."
  (interactive)
  (when (and (> (buffer-size) 0)
             (not (yes-or-no-p "Buffer has content.  Erase and start fresh? ")))
    (user-error "Aborted"))
  (let ((template (if (called-interactively-p 'interactive)
                      (resumel-read-template-name "Template for new resume: ")
                    resumel-default-template)))
    (erase-buffer)
    (setq resumel-default-template template)
    (let ((scaffold (cdr (assoc template resumel-init--scaffolds))))
      (insert (or scaffold
                  (format
                   (concat
                    "* Config :noexport:\n"
                    "#+RESUMEL_TEMPLATE: %s\n"
                    "#+TITLE: Your Name\n"
                    "#+AUTHOR: Your Name\n"
                    "#+EXPORT_FILE_NAME: resumel-resume.pdf\n"
                    "#+OPTIONS: toc:nil title:nil H:2\n"
                    "\n"
                    "* Summary\n"
                    "\n"
                    "Hello World! This is a starter line. Edit the sections below and run =M-x resumel-export= to build your PDF.\n")
                   template))))
    (goto-char (point-min))
    (unless resumel-mode
      (resumel-mode 1))
    (message "Initialized resumel buffer with template: %s" template)))

(provide 'resumel-mode)
;;; resumel-mode.el ends here
