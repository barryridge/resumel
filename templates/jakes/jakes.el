;; Disable Org's default hyperref template and package lists -
;; Jake's template manages its own package loading.
(setq org-latex-hyperref-template nil)
(setq org-latex-default-packages-alist nil)
(setq org-latex-packages-alist nil)

(let* ((compiler (or (cdr (assoc "COMPILER" resumel-template-vars)) "pdflatex"))
       (documentclass-options (or (cdr (assoc "DOCUMENTCLASS_OPTIONS" resumel-template-vars)) "letterpaper,11pt"))
       (font (or (cdr (assoc "JAKES_FONT" resumel-template-vars)) "default"))
       (phone (or (cdr (assoc "JAKES_PHONE" resumel-template-vars)) ""))
       (email (or (cdr (assoc "JAKES_EMAIL" resumel-template-vars)) ""))
       (linkedin (or (cdr (assoc "JAKES_LINKEDIN" resumel-template-vars)) ""))
       (linkedin-label (let ((l (or (cdr (assoc "JAKES_LINKEDIN_LABEL" resumel-template-vars)) "")))
                         (if (string-empty-p l) linkedin l)))
       (github (or (cdr (assoc "JAKES_GITHUB" resumel-template-vars)) ""))
       (github-label (let ((l (or (cdr (assoc "JAKES_GITHUB_LABEL" resumel-template-vars)) "")))
                       (if (string-empty-p l) github l)))
       ;; cvtag defaults
       (cvtag-intensity-default (or (cdr (assoc "CVTAG_INTENSITY_DEFAULT" resumel-template-vars)) "5"))
       (cvtag-font-default (or (cdr (assoc "CVTAG_FONT_DEFAULT" resumel-template-vars)) "\\scriptsize"))
       (cvtag-baseline-default (or (cdr (assoc "CVTAG_BASELINE_DEFAULT" resumel-template-vars)) "-0.5ex"))
       (cvtag-inner-x-sep-default (or (cdr (assoc "CVTAG_INNER_X_SEP_DEFAULT" resumel-template-vars)) "0.5ex"))
       (cvtag-inner-y-sep-default (or (cdr (assoc "CVTAG_INNER_Y_SEP_DEFAULT" resumel-template-vars)) "0.5ex"))
       (cvtag-text-height-default (or (cdr (assoc "CVTAG_TEXT_HEIGHT_DEFAULT" resumel-template-vars)) "1.25ex"))
       (cvtag-text-depth-default (or (cdr (assoc "CVTAG_TEXT_DEPTH_DEFAULT" resumel-template-vars)) "0.25ex"))
       (cvtag-corner-default (or (cdr (assoc "CVTAG_CORNER_DEFAULT" resumel-template-vars)) "rounded corners"))
       ;; Build contact info line from non-empty fields
       (contact-parts '()))

  ;; Accumulate contact items in order: phone, email, linkedin, github
  (unless (string-empty-p phone)
    (push phone contact-parts))
  (unless (string-empty-p email)
    (push (format "\\href{mailto:%s}{\\underline{%s}}" email email) contact-parts))
  (unless (string-empty-p linkedin)
    (push (format "\\href{https://linkedin.com/in/%s}{\\underline{%s}}" linkedin linkedin-label) contact-parts))
  (unless (string-empty-p github)
    (push (format "\\href{https://github.com/%s}{\\underline{%s}}" github github-label) contact-parts))
  (setq contact-parts (nreverse contact-parts))

  (setq org-latex-compiler compiler)

  (add-to-list 'org-latex-classes
               `("resumel-jakes"
                 ,(concat "\\documentclass[" documentclass-options "]{article}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
"
                          ;; Optional font package
                          (cond
                           ((string= font "FiraSans")          "\\usepackage[sfdefault]{FiraSans}\n")
                           ((string= font "roboto")            "\\usepackage[sfdefault]{roboto}\n")
                           ((string= font "noto-sans")         "\\usepackage[sfdefault]{noto-sans}\n")
                           ((string= font "sourcesanspro")     "\\usepackage[default]{sourcesanspro}\n")
                           ((string= font "CormorantGaramond") "\\usepackage{CormorantGaramond}\n")
                           ((string= font "charter")           "\\usepackage{charter}\n")
                           (t ""))
                          "\\usepackage{latexsym}
\\usepackage[empty]{fullpage}
\\usepackage{titlesec}
\\usepackage{marvosym}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage{verbatim}
\\usepackage{enumitem}
\\usepackage[hidelinks]{hyperref}
\\usepackage{fancyhdr}
\\usepackage[english]{babel}
\\usepackage{tabularx}
\\usepackage{fontawesome5}
\\usepackage{tikz}
\\usepackage{dashrule}

% ATS-parsable PDF (pdflatex only)
\\ifdefined\\pdfgentounicode
  \\input{glyphtounicode}
  \\pdfgentounicode=1
\\fi

% Color aliases for resumel common interface
\\colorlet{color0}{black}
\\colorlet{color1}{black}
\\colorlet{color2}{darkgray}

\\pagestyle{fancy}
\\fancyhf{}
\\fancyfoot{}
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}

% Adjust margins
\\addtolength{\\oddsidemargin}{-0.5in}
\\addtolength{\\evensidemargin}{-0.5in}
\\addtolength{\\textwidth}{1in}
\\addtolength{\\topmargin}{-.5in}
\\addtolength{\\textheight}{1.0in}

\\urlstyle{same}

\\raggedbottom
\\raggedright
\\setlength{\\tabcolsep}{0in}

% Section heading formatting
\\titleformat{\\section}{
  \\vspace{-4pt}\\scshape\\raggedright\\large
}{}{0em}{}[\\color{black}\\titlerule \\vspace{-5pt}]

%-------------------------
% Standard list environments styled to match Jake's resume conventions.
%
% First-level itemize acts as the entry container (\\resumeSubHeadingListStart):
%   no bullet, 0.15in left margin, tight spacing.
% Second-level itemize acts as the responsibility list (\\resumeItemListStart):
%   small bullet, small font, tight spacing.
% Description lists act as the skills list: bold label with colon, inline value.

\\setlist[itemize,1]{
  leftmargin=0.15in,
  label={},
  topsep=0pt,
  itemsep=0pt,
  parsep=0pt
}

\\setlist[itemize,2]{
  label=\\footnotesize$\\vcenter{\\hbox{\\tiny$\\bullet$}}$,
  before=\\small
}

\\setlist[description]{
  leftmargin=0.15in,
  labelindent=0.15in,
  style=sameline,
  labelsep=0pt,
  topsep=2pt,
  parsep=0pt,
  itemsep=2pt,
  before=\\small
}

% Override description label to match Jake's original: bold label followed by colon + space
\\renewcommand*{\\descriptionlabel}[1]{\\hspace{\\labelsep}\\textbf{#1}{: }}

%-------------------------
% Utility commands

\\newcommand*{\\Cplusplus}{C\\nolinebreak\\hspace{-.05em}\\raisebox{.4ex}{\\tiny\\textbf{++}}}

%-------------------------
% Symbols

\\newcommand{\\addresssymbol}{{\\color{color2}\\small\\faMapMarker}~}
\\newcommand{\\calendarsymbol}{{\\color{color2}\\small\\faCalendar}~}

%-------------------------
% Divider

\\newcommand{\\divider}{\\textcolor{color2!30}{\\hdashrule{\\linewidth}{0.6pt}{0.5ex}}\\medskip}

%-------------------------
% CV Tags

\\newcommand{\\cvtagIntensityDefault}{" cvtag-intensity-default "}
\\newcommand{\\cvtagFontDefault}{" cvtag-font-default "}
\\newcommand{\\cvtagBaselineDefault}{" cvtag-baseline-default "}
\\newcommand{\\cvtagInnerXSepDefault}{" cvtag-inner-x-sep-default "}
\\newcommand{\\cvtagInnerYSepDefault}{" cvtag-inner-y-sep-default "}
\\newcommand{\\cvtagTextHeightDefault}{" cvtag-text-height-default "}
\\newcommand{\\cvtagTextDepthDefault}{" cvtag-text-depth-default "}
\\newcommand{\\cvtagCornerDefault}{" cvtag-corner-default "}

\\makeatletter
\\NewDocumentCommand{\\cvtag}{m
  O{\\cvtagIntensityDefault}
  O{\\cvtagFontDefault}
  O{\\cvtagBaselineDefault}
  O{\\cvtagInnerXSepDefault}
  O{\\cvtagInnerYSepDefault}
  O{\\cvtagTextHeightDefault}
  O{\\cvtagTextDepthDefault}
  O{\\cvtagCornerDefault}}{%
  \\pgfmathsetmacro{\\skillIntensity}{20 + (#2 * 16)}%
  \\ifdim \\skillIntensity pt > 100pt \\def\\skillIntensity{100}\\fi
  \\ifdim \\skillIntensity pt < 0pt   \\def\\skillIntensity{0}\\fi
  \\tikz[baseline=#4]{%
    \\node[draw=black!\\skillIntensity!white,
          fill=white,
          #9,
          inner xsep=#5,
          inner ysep=#6,
          text height=#7,
          text depth=#8,
          font=#3,
          text=black!\\skillIntensity!white]
          {#1};%
  }%
}
\\makeatother

%-------------------------
% Custom commands (used by the complex fixture and available for manual use)

\\newcommand{\\resumeItem}[1]{
  \\item\\small{
    {#1 \\vspace{-2pt}}
  }
}

\\newcommand{\\resumeSubheading}[4]{
  \\vspace{-2pt}\\item
    \\begin{tabular*}{0.97\\textwidth}[t]{l@{\\extracolsep{\\fill}}r}
      \\textbf{#1} & #2 \\\\
      \\textit{\\small#3} & \\textit{\\small #4} \\\\
    \\end{tabular*}\\vspace{-7pt}
}

\\newcommand{\\resumeSubSubheading}[2]{
    \\item
    \\begin{tabular*}{0.97\\textwidth}{l@{\\extracolsep{\\fill}}r}
      \\textit{\\small#1} & \\textit{\\small #2} \\\\
    \\end{tabular*}\\vspace{-7pt}
}

\\newcommand{\\resumeProjectHeading}[2]{
    \\item
    \\begin{tabular*}{0.97\\textwidth}{l@{\\extracolsep{\\fill}}r}
      \\small#1 & #2 \\\\
    \\end{tabular*}\\vspace{-7pt}
}

\\newcommand{\\resumeSubItem}[1]{\\resumeItem{#1}\\vspace{-4pt}}

\\renewcommand\\labelitemii{$\\vcenter{\\hbox{\\tiny$\\bullet$}}$}

\\newcommand{\\resumeSubHeadingListStart}{\\begin{itemize}[leftmargin=0.15in, label={}, topsep=9pt, itemsep=4.5pt, parsep=4.5pt]}
\\newcommand{\\resumeSubHeadingListEnd}{\\end{itemize}}
\\newcommand{\\resumeItemListStart}{\\begin{itemize}}
\\newcommand{\\resumeItemListEnd}{\\end{itemize}\\vspace{-5pt}}

%-------------------------
% Common CV macros (resumel common interface)
%
% These provide a template-agnostic interface matching the other resumel templates.

% cvevent: {title}{organization}{dates}{location}
% Maps to Jake's 2-row tabular layout (bold title + dates, italic org + location)
\\newcommand{\\cvevent}[4]{%
  \\vspace{-2pt}%
  \\begin{tabular*}{0.97\\textwidth}[t]{l@{\\extracolsep{\\fill}}r}
    \\textbf{#1} & #3 \\\\
    \\textit{\\small#2} & \\textit{\\small #4} \\\\
  \\end{tabular*}\\vspace{-7pt}
}

% cvachievement: {icon}{title}{description}
\\newcommand{\\cvachievement}[3]{%
  \\small{#1~\\textbf{#2} -- #3}\\par\\vspace{2pt}
}

% cvref: {name}{institution}{email}
\\newcommand{\\cvref}[3]{%
  \\begin{tabular*}{0.97\\textwidth}[t]{l@{\\extracolsep{\\fill}}r}
    \\textbf{#1} & \\href{mailto:#3}{\\underline{#3}} \\\\
    \\textit{\\small#2} \\\\
  \\end{tabular*}\\vspace{-7pt}
}

% cvskill: {name}{level} where level is 1-5
\\newcommand{\\cvskillDot}[2]{%
  \\ifnum#1>#2\\relax\\textcolor{black!20}{\\footnotesize\\faCircle}\\else\\textcolor{black}{\\footnotesize\\faCircle}\\fi
}
\\newcommand{\\cvskill}[2]{%
  \\small\\textbf{#1}\\hfill
  \\cvskillDot{1}{#2}\\,\\cvskillDot{2}{#2}\\,\\cvskillDot{3}{#2}\\,\\cvskillDot{4}{#2}\\,\\cvskillDot{5}{#2}%
  \\par\\vspace{2pt}
}

% Jake's resume-style centered header via \\maketitle
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\begin{center}
    \\textbf{\\Huge \\scshape \\@author}"
                          (if contact-parts
                              (concat " \\\\ \\vspace{1pt}\n    \\small "
                                      (string-join contact-parts " $|$ "))
                            "")
                          "
  \\end{center}
  \\vspace{-8pt}
}
\\makeatother
")
                 ("\n\\section{%s}" . "\n\\section*{%s}")
                 ("\n\\subsection{%s}" . "\n\\subsection*{%s}"))))

(defconst resumel-jakes-variable-defaults
  '(("COMPILER"              . "pdflatex")
    ("DOCUMENTCLASS_OPTIONS" . "letterpaper,11pt")
    ("JAKES_FONT"            . "default")
    ("JAKES_PHONE"           . "")
    ("JAKES_EMAIL"           . "")
    ("JAKES_LINKEDIN"        . "")
    ("JAKES_LINKEDIN_LABEL"  . "")
    ("JAKES_GITHUB"          . "")
    ("JAKES_GITHUB_LABEL"    . "")
    ("CVTAG_INTENSITY_DEFAULT"   . "5")
    ("CVTAG_FONT_DEFAULT"        . "\\scriptsize")
    ("CVTAG_BASELINE_DEFAULT"    . "-0.5ex")
    ("CVTAG_INNER_X_SEP_DEFAULT" . "0.5ex")
    ("CVTAG_INNER_Y_SEP_DEFAULT" . "0.5ex")
    ("CVTAG_TEXT_HEIGHT_DEFAULT" . "1.25ex")
    ("CVTAG_TEXT_DEPTH_DEFAULT"  . "0.25ex")
    ("CVTAG_CORNER_DEFAULT"      . "rounded corners"))
  "Default variable values for the resumel jakes template.")

(provide 'resumel-jakes)
