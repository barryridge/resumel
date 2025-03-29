(let* ((compiler (or (cdr (assoc "COMPILER" resumel-template-vars)) "xelatex"))
       (geometry (or (cdr (assoc "GEOMETRY" resumel-template-vars)) "left=1.4cm, top=.8cm, right=1.4cm, bottom=1.8cm, footskip=.5cm"))
       (documentclass-options (or (cdr (assoc "DOCUMENTCLASS_OPTIONS" resumel-template-vars)) "10pt,letterpaper,ragged2e,withhyper"))
       (main-font-xelatex (or (cdr (assoc "MAIN_FONT_XELATEX" resumel-template-vars)) "Roboto Slab"))
       (sans-font-xelatex (or (cdr (assoc "SANS_FONT_XELATEX" resumel-template-vars)) "Lato"))
       (mono-font-xelatex (or (cdr (assoc "MONO_FONT_XELATEX" resumel-template-vars)) "Fira Code"))
       (math-font-xelatex (or (cdr (assoc "MATH_FONT_XELATEX" resumel-template-vars)) "TeX Gyre Termes Math"))
       (main-font-pdflatex (or (cdr (assoc "MAIN_FONT_PDFLATEX" resumel-template-vars)) "roboto"))
       (sans-font-pdflatex (or (cdr (assoc "SANS_FONT_PDFLATEX" resumel-template-vars)) "lato"))
       (mono-font-pdflatex (or (cdr (assoc "MONO_FONT_PDFLATEX" resumel-template-vars)) "sourcecodepro"))
       (math-font-pdflatex (or (cdr (assoc "MATH_FONT_PDFLATEX" resumel-template-vars)) "newtxmath"))
       (awesomecv-color (or (cdr (assoc "AWESOMECV_COLOR" resumel-template-vars)) "awesome-red"))
       (awesomecv-section-color-highlight (or (cdr (assoc "AWESOMECV_SECTION_COLOR_HIGHLIGHT" resumel-template-vars)) "true"))
       (awesomecv-header-social-sep (or (cdr (assoc "AWESOMECV_HEADER_SOCIAL_SEP" resumel-template-vars)) "\\quad\\textbar\\quad"))
       (awesomecv-section-top-skip (or (cdr (assoc "AWESOMECV_SECTION_TOP_SKIP" resumel-template-vars)) "3mm"))
       (awesomecv-section-content-top-skip (or (cdr (assoc "AWESOMECV_SECTION_CONTENT_TOP_SKIP" resumel-template-vars)) "2.5mm")))

  (setq org-latex-compiler compiler)

  (message "[resumel - DEBUG]: org-latex-default-packages-alist BEFORE: %S" org-latex-default-packages-alist)

  ;; Remove amssymb from org latex packages, as it seems to cause a conflict
  (require 'cl-lib)  ;; for cl-remove-if
  (setq org-latex-default-packages-alist
        (cl-remove-if (lambda (pkg)
                        (member (cadr pkg) '("amsmath" "amssymb")))
                      org-latex-default-packages-alist))

  (message "[resumel - DEBUG]: org-latex-default-packages-alist AFTER: %S" org-latex-default-packages-alist)

  (add-to-list 'org-latex-classes
               `("resumel-awesomecv"
                 ,(concat "\\documentclass[" documentclass-options "]{awesome-cv}

% Layout
\\geometry{" geometry "}

% Fonts
% \\iftutex
%   % If using xelatex or lualatex:
%   \\setmainfont{" main-font-xelatex "} % Main (serif/roman) font
%   \\setsansfont{" sans-font-xelatex "} % Sans-serif font
%   % \\setmonofont{" mono-font-xelatex "} % Monospace font
%   % \\setmathfont{" math-font-xelatex "} % Math font (compatible with Roboto Slab)
%   \\renewcommand{\\familydefault}{\\sfdefault}
% \\else
%   % If using pdflatex:
%   \\usepackage[rm]{" main-font-pdflatex "} % Main (serif/roman) font
%   \\usepackage[defaultsans]{" sans-font-pdflatex "} % Sans-serif font
%   % \\usepackage[ttdefault]{" mono-font-pdflatex "} % Monospace font
%   % \\usepackage{" math-font-pdflatex "} % Math font
%   \\renewcommand{\\familydefault}{\\sfdefault}
% \\fi

% Colors
%
% Color for highlights
% Awesome Colors: awesome-emerald, awesome-skyblue, awesome-red, awesome-pink, awesome-orange
%                 awesome-nephritis, awesome-concrete, awesome-darknight
\\colorlet{awesome}{" awesomecv-color "}
% Uncomment if you would like to specify your own color
% \\definecolor{awesome}{HTML}{3E6D9C}

% Colors for text
% Uncomment if you would like to specify your own color
% \\definecolor{darktext}{HTML}{414141}
% \\definecolor{text}{HTML}{333333}
% \\definecolor{graytext}{HTML}{5D5D5D}
% \\definecolor{lighttext}{HTML}{999999}
% \\definecolor{sectiondivider}{HTML}{5D5D5D}
\\colorlet{color0}{black}
\\colorlet{color1}{" awesomecv-color "}
\\colorlet{color2}{gray}

% Set false if you don't want to highlight section with awesome color
\\setbool{acvSectionColorHighlight}{" awesomecv-section-color-highlight "}

% If you would like to change the social information separator from a pipe (|) to something else
\\renewcommand{\\acvHeaderSocialSep}{" awesomecv-header-social-sep "}

% Other
\\renewcommand{\\acvSectionTopSkip}{" awesomecv-section-top-skip "}
\\renewcommand{\\acvSectionContentTopSkip}{" awesomecv-section-content-top-skip "}

% Define divider (replicated from altacv)
\\usepackage{dashrule}
\\newcommand{\\divider}{\\textcolor{color2!30}{\\hdashrule{\\linewidth}{0.6pt}{0.5ex}}\\medskip}

% CV Tags
\\usepackage{tikz}

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

% C++ logo
\\def\\Cplusplus{C{}\\texttt{++}}

% Add map marker symbol for addresses
\\newcommand*{\\addresssymbol}{{\\color{color2}\\small\\faMapMarker}~}

% Add calendar symbol command for dates
\\newcommand{\\calendarsymbol}{{\\color{color2}\\small\\faCalendar}~}

% Math stuff...
\\usepackage{amsmath}
\\usepackage{amsfonts}

")

                 ("\n\\cvsection{%s}" . "\n\\cvsection*{%s}")
                 ("\n\\cvsubsection{%s}" . "\n\\cvsubsection*{%s}"))))

(provide 'resumel-awesomecv)
