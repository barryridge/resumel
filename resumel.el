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

;; Disable Org's hyperref template - let moderncv handle it
(setq org-latex-hyperref-template nil)
(setq org-latex-default-packages-alist nil)
(setq org-latex-packages-alist nil)

(require 'subr-x)  ;; For string-trim if on older Emacs (24.x). Emacs 25+ has it built-in.

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

(setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf")))
(add-to-list 'org-latex-classes
             '("moderncv"
               "\\documentclass[11pt,letterpaper,sans]{moderncv}

% ModernCV themes
\\moderncvstyle{classic}

% To make cover letter text justified
\\usepackage{etoolbox}% http://ctan.org/pkg/etoolbox
\\makeatletter
\\patchcmd{\\makeletterhead}% <cmd>
  {\\raggedright \\@opening}% <search>
  {\\@opening}% <replace>
  {}{}% <success><failure>
\\makeatother

\\moderncvcolor{blue}

% Set up fonts
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}

% Page layout adjustments
\\usepackage[scale=0.75]{geometry}

% Math and symbol support
\\usepackage{amsmath}
\\usepackage{amsfonts}

% Custom commands
\\newcommand*{\\Cplusplus}{C{}\\texttt{++}}
\\renewcommand*{\\cventry}[7][.25em]{%
  \\cvitem[#1]{#2}{%
    {\\bfseries#3}%
    \\ifthenelse{\\equal{#4}{}}{}{{\\slshape#4}}%
    \\ifthenelse{\\equal{#5}{}}{}{#5}%
    \\ifthenelse{\\equal{#6}{}}{}{#6}%
    \\strut%
    \\ifx&#7&%
    \\else{\\newline{}\\begin{minipage}[t]{\\linewidth}\\small#7\\end{minipage}}
    \\fi
  }
}

% % CV Tags
% \\usepackage{tikz}
% \\usepackage{xcolor}
% \\newcommand{\\cvtag}[1]{%
%   \\tikz[baseline]{%
%     \\node[anchor=base,
%            draw=color2!30,
%            rounded corners,
%            inner xsep=0.5ex,
%            inner ysep=0.5ex,
%            text height=1.25ex,
%            text depth=.25ex,
%            font=\\scriptsize]{#1};
%   }
% }

\\usepackage{xparse}
\\usepackage{tikz}
\\usepackage{xcolor}

\\NewDocumentCommand{\\cvtag}{m O{5}}{%
  \\pgfmathsetmacro{\\skillIntensity}{20 + (#2 * 16)}%
  \\ifdim \\skillIntensity pt > 100pt \\def\\skillIntensity{100}\\fi
  \\ifdim \\skillIntensity pt < 0pt   \\def\\skillIntensity{0}\\fi
  \\tikz[baseline=-0.5ex]{%
    \\node[draw=black!\\skillIntensity!white,
           fill=white,
           rounded corners,
           inner xsep=0.5ex,
           inner ysep=0.5ex,
           text height=1.25ex,
           text depth=.25ex,
           font=\\scriptsize,
           text=black!\\skillIntensity!white]{#1};%
  }
}

% % CV Skills
% \\renewcommand{\\cvskill}[2]{%
% \\textcolor{emphasis}{\\textbf{#1}}\\hfill
% \\foreach \\x in {1,...,5}{%
%   \\space{\\ifnumgreater{\\x}{#2}{\\color{color2!30}}{\\color{color1}}\\ratingmarker}}\\par%
% }

% Highlight macro used to boldface name in .bib file
\\newcommand{\\highlight}[1]{\\textbf{#1}} % Replace \textbf with any desired formatting

% Add map marker symbol to address
\\renewcommand*{\\addresssymbol}{{\\color{color2}\\small\\faMapMarker}~}

% Add calendar symbol command for dates
\\newcommand{\\calendarsymbol}{{\\color{color2}\\small\\faCalendar}~}

% For pdf attachments
\\usepackage{pdfpages}

% Adjust the page margins
\\geometry{top=2cm, bottom=2cm, left=2.05cm, right=2.05cm}

"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")))

(setq org-latex-packages-alist 'nil)

(provide 'resumel)
;;; resumel.el ends here
