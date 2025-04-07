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
       (section-font (or (cdr (assoc "SECTION_FONT" resumel-template-vars)) "\\fontsize{16pt}{1em}\\bodyfont\\bfseries"))
       (subsection-font (or (cdr (assoc "SUBSECTION_FONT" resumel-template-vars)) "\\fontsize{12pt}{1em}\\bodyfont\\scshape"))
       (cvtag-intensity-default (or (cdr (assoc "CVTAG_INTENSITY_DEFAULT" resumel-template-vars)) "5"))
       (cvtag-font-default (or (cdr (assoc "CVTAG_FONT_DEFAULT" resumel-template-vars)) "\\scriptsize"))
       (cvtag-baseline-default (or (cdr (assoc "CVTAG_BASELINE_DEFAULT" resumel-template-vars)) "-0.5ex"))
       (cvtag-inner-x-sep-default (or (cdr (assoc "CVTAG_INNER_X_SEP_DEFAULT" resumel-template-vars)) "0.5ex"))
       (cvtag-inner-y-sep-default (or (cdr (assoc "CVTAG_INNER_Y_SEP_DEFAULT" resumel-template-vars)) "0.5ex"))
       (cvtag-text-height-default (or (cdr (assoc "CVTAG_TEXT_HEIGHT_DEFAULT" resumel-template-vars)) "1.25ex"))
       (cvtag-text-depth-default (or (cdr (assoc "CVTAG_TEXT_DEPTH_DEFAULT" resumel-template-vars)) "0.25ex"))
       (cvtag-corner-default (or (cdr (assoc "CVTAG_CORNER_DEFAULT" resumel-template-vars)) "rounded corners"))
       (awesomecv-color (or (cdr (assoc "AWESOMECV_COLOR" resumel-template-vars)) "awesome-red"))
       (awesomecv-section-color-highlight (or (cdr (assoc "AWESOMECV_SECTION_COLOR_HIGHLIGHT" resumel-template-vars)) "true"))
       (awesomecv-header-social-sep (or (cdr (assoc "AWESOMECV_HEADER_SOCIAL_SEP" resumel-template-vars)) "\\quad\\textbar\\quad"))
       (awesomecv-section-top-skip (or (cdr (assoc "AWESOMECV_SECTION_TOP_SKIP" resumel-template-vars)) "3mm"))
       (awesomecv-section-content-top-skip (or (cdr (assoc "AWESOMECV_SECTION_CONTENT_TOP_SKIP" resumel-template-vars)) "2.5mm")))

  (setq org-latex-compiler compiler)

  ;; Remove amssymb from org latex packages, as it seems to cause a conflict
  (require 'cl-lib)  ;; for cl-remove-if
  (setq org-latex-default-packages-alist
        (cl-remove-if (lambda (pkg)
                        (member (cadr pkg) '("amsmath" "amssymb")))
                      org-latex-default-packages-alist))

  (add-to-list 'org-latex-classes
               `("resumel-awesomecv"
                 ,(concat "\\documentclass[" documentclass-options "]{awesome-cv}

% Layout
%
\\geometry{" geometry "}

% Fonts
%
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

% Redefine Awesome section color to enable below section style renewal
\\def\\@sectioncolor#1#2#3{%
  \\ifbool{acvSectionColorHighlight}{{\\color{awesome}#1#2#3}}{#1#2#3}%
}

\\renewcommand*{\\sectionstyle}[1]{{" section-font "\\color{text}\\@sectioncolor #1}}
\\renewcommand*{\\subsectionstyle}[1]{{" subsection-font "\\textcolor{text}{#1}}}

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

% Math and symbol support
%
\\usepackage{amsmath}
\\usepackage{amsfonts}

% If you would like to change the social information separator from a pipe (|) to something else
\\renewcommand{\\acvHeaderSocialSep}{" awesomecv-header-social-sep "}

% Other
\\renewcommand{\\acvSectionTopSkip}{" awesomecv-section-top-skip "}
\\renewcommand{\\acvSectionContentTopSkip}{" awesomecv-section-content-top-skip "}

% Redfine itemize to match cvitems from awesomecv
% This will cause Org lists to use this format instead.
%
\\usepackage{enumitem}
\\let\\olditemize\\itemize
\\let\\endolditemize\\enditemize
\\renewenvironment{itemize}{
  \\begin{justify}
  \\begin{olditemize}[leftmargin=2ex, nosep, noitemsep]
    \\setlength{\\parskip}{0pt}
    \\renewcommand{\\labelitemi}{\\bullet}%
}{
  \\end{olditemize}
  \\end{justify}
}

% Define divider (replicated from altacv)
\\usepackage{dashrule}
\\newcommand{\\divider}{\\textcolor{color2!30}{\\hdashrule{\\linewidth}{0.6pt}{0.5ex}}\\medskip}

% CV Tags
%
% Set global cvtag defaults
\\newcommand{\\cvtagIntensityDefault}{" cvtag-intensity-default "}
\\newcommand{\\cvtagFontDefault}{" cvtag-font-default "}
\\newcommand{\\cvtagBaselineDefault}{" cvtag-baseline-default "}
\\newcommand{\\cvtagInnerXSepDefault}{" cvtag-inner-x-sep-default "}
\\newcommand{\\cvtagInnerYSepDefault}{" cvtag-inner-y-sep-default "}
\\newcommand{\\cvtagTextHeightDefault}{" cvtag-text-height-default "}
\\newcommand{\\cvtagTextDepthDefault}{" cvtag-text-depth-default "}
\\newcommand{\\cvtagCornerDefault}{" cvtag-corner-default "}

\\usepackage{tikz}
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
  % Calculate the intensity based on the provided level (0–5)
  \\pgfmathsetmacro{\\skillIntensity}{20 + (#2 * 16)}%
  \\ifdim \\skillIntensity pt > 100pt \\def\\skillIntensity{100}\\fi
  \\ifdim \\skillIntensity pt < 0pt   \\def\\skillIntensity{0}\\fi
  %
  % Draw the tag using separate spacing parameters.
  \\tikz[baseline=#4]{%
    \\node[draw=black!\\skillIntensity!white,
          fill=white,
          % Use the passed corner style
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

% C++ logo
\\def\\Cplusplus{C{}\\texttt{++}}

% Add map marker symbol for addresses
\\newcommand*{\\addresssymbol}{{\\color{color2}\\small\\faMapMarker}~}

% Add calendar symbol command for dates
\\newcommand{\\calendarsymbol}{{\\color{color2}\\small\\faCalendar}~}

")

                 ("\n\\cvsection{%s}" . "\n\\cvsection*{%s}")
                 ("\n\\cvsubsection{%s}" . "\n\\cvsubsection*{%s}"))))

(provide 'resumel-awesomecv)
