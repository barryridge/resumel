;; Disable Org's hyperref template - let moderncv handle it
(setq org-latex-hyperref-template nil)
(setq org-latex-default-packages-alist nil)
(setq org-latex-packages-alist nil)

(let* ((compiler (or (cdr (assoc "COMPILER" resumel-template-vars)) "pdflatex"))
       (geometry (or (cdr (assoc "GEOMETRY" resumel-template-vars)) "scale=0.75, top=2cm, bottom=2cm, left=2.05cm, right=2.05cm"))
       (documentclass-options (or (cdr (assoc "DOCUMENTCLASS_OPTIONS" resumel-template-vars)) "11pt,letterpaper,sans"))
       (main-font-xelatex (or (cdr (assoc "MAIN_FONT_XELATEX" resumel-template-vars)) "Latin Modern Roman"))
       (sans-font-xelatex (or (cdr (assoc "SANS_FONT_XELATEX" resumel-template-vars)) "Latin Modern Sans"))
       (mono-font-xelatex (or (cdr (assoc "MONO_FONT_XELATEX" resumel-template-vars)) "Latin Modern Mono"))
       (math-font-xelatex (or (cdr (assoc "MATH_FONT_XELATEX" resumel-template-vars)) "Latin Modern Math"))
       (main-font-pdflatex (or (cdr (assoc "MAIN_FONT_PDFLATEX" resumel-template-vars)) "lmodern"))
       (sans-font-pdflatex (or (cdr (assoc "SANS_FONT_PDFLATEX" resumel-template-vars)) "lmodern"))
       (mono-font-pdflatex (or (cdr (assoc "MONO_FONT_PDFLATEX" resumel-template-vars)) "lmodern"))
       (math-font-pdflatex (or (cdr (assoc "MATH_FONT_PDFLATEX" resumel-template-vars)) "newtxmath"))
       (cvtag-intensity-default (or (cdr (assoc "CVTAG_INTENSITY_DEFAULT" resumel-template-vars)) "5"))
       (cvtag-font-default (or (cdr (assoc "CVTAG_FONT_DEFAULT" resumel-template-vars)) "\\scriptsize"))
       (cvtag-inner-x-sep-default (or (cdr (assoc "CVTAG_INNER_X_SEP_DEFAULT" resumel-template-vars)) "0.5ex"))
       (cvtag-inner-y-sep-default (or (cdr (assoc "CVTAG_INNER_Y_SEP_DEFAULT" resumel-template-vars)) "0.5ex"))
       (cvtag-text-height-default (or (cdr (assoc "CVTAG_TEXT_HEIGHT_DEFAULT" resumel-template-vars)) "1.25ex"))
       (cvtag-text-depth-default (or (cdr (assoc "CVTAG_TEXT_DEPTH_DEFAULT" resumel-template-vars)) "0.25ex"))
       (cvtag-corner-default (or (cdr (assoc "CVTAG_CORNER_DEFAULT" resumel-template-vars)) "rounded corners"))
       (moderncv-color (or (cdr (assoc "MODERNCV_COLOR" resumel-template-vars)) "blue"))
       (moderncv-style (or (cdr (assoc "MODERNCV_STYLE" resumel-template-vars)) "classic"))
       (moderncv-firstname (or (cdr (assoc "MODERNCV_FIRSTNAME" resumel-template-vars)) ""))
       (moderncv-lastname (or (cdr (assoc "MODERNCV_LASTNAME" resumel-template-vars)) "")))

  (setq org-latex-compiler compiler)

  (add-to-list 'org-latex-classes
               `("resumel-moderncv"
                 ,(concat "\\documentclass[" documentclass-options "]{moderncv}

% Set default ModernCV color
\\moderncvcolor{" moderncv-color "}

% Redefine moderncv colors (they seem to not propagate from the package and cause xcolor 'Undefined color' errors)
\\definecolor{black}{RGB}{0, 0, 0}
\\definecolor{red}{rgb}{0.95, 0.20, 0.20}
\\definecolor{darkgrey}{rgb}{0.45, 0.45, 0.45}
\\definecolor{orange}{rgb}{0.95, 0.55, 0.15}
\\definecolor{burgundy}{rgb}{0.596078, 0, 0}% 139/255 (0.545098) or 152/255 (0.596078)
\\definecolor{purple}{rgb}{0.50, 0.33, 0.80}
\\definecolor{lightblue}{rgb}{0.22, 0.45, 0.70}
\\definecolor{green}{rgb}{0.35, 0.70, 0.30}

% Set default ModernCV theme
\\moderncvstyle{" moderncv-style "}

% Layout
\\usepackage[" geometry "]{geometry}

% To make cover letter text justified
\\usepackage{etoolbox}% http://ctan.org/pkg/etoolbox
\\makeatletter
\\patchcmd{\\makeletterhead}% <cmd>
  {\\raggedright \\@opening}% <search>
  {\\@opening}% <replace>
  {}{}% <success><failure>
\\makeatother

% Fonts
\\iftutex
  \\usepackage{fontspec}
  \\usepackage{unicode-math}
  \\defaultfontfeatures{Ligatures=TeX}
  \\setmainfont{" main-font-xelatex "}
  \\setsansfont{" sans-font-xelatex "}
  \\setmonofont{" mono-font-xelatex "}
  \\setmathfont{" math-font-xelatex "}

  % you may also consider Fira Sans Light for a extra modern look
  %\\setsansfont[ItalicFont={Fira Sans Light Italic},%
  %           BoldFont={Fira Sans},%
  %           BoldItalicFont={Fira Sans Italic}]%
  %           {Fira Sans Light}%
\\else
  \\usepackage[utf8]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage{" main-font-pdflatex "}
\\fi


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

% Define divider (replicated from altacv)
\\usepackage{dashrule}
\\newcommand{\\divider}{\\textcolor{color2!30}{\\hdashrule{\\linewidth}{0.6pt}{0.5ex}}\\medskip}

% CV Tags
%
% Set global cvtag defaults
\\newcommand{\\cvtagIntensityDefault}{" cvtag-intensity-default "}
\\newcommand{\\cvtagFontDefault}{" cvtag-font-default "}
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
  \\tikz[baseline=-0.5ex]{%
    \\node[draw=black!\\skillIntensity!white,
          fill=white,
          % Use the passed corner style
          #8,
          inner xsep=#4,
          inner ysep=#5,
          text height=#6,
          text depth=#7,
          font=#3,
          text=black!\\skillIntensity!white]
          {#1};%
  }%
}
\\makeatother

% Highlight macro used to boldface name in .bib file
\\newcommand{\\highlight}[1]{\\textbf{#1}} % Replace \\textbf with any desired formatting

% Add map marker symbol to address
\\renewcommand*{\\addresssymbol}{{\\color{color2}\\small\\faMapMarker}~}

% Add calendar symbol command for dates
\\newcommand{\\calendarsymbol}{{\\color{color2}\\small\\faCalendar}~}

% For pdf attachments
\\usepackage{pdfpages}

% Set name (moderncv will throw errors if this is not set in the header)
\\name{" moderncv-firstname "}{" moderncv-lastname "}

")
                 ("\n\\section{%s}" . "\n\\section*{%s}")
                 ("\n\\subsection{%s}" . "\n\\subsection*{%s}"))))

(provide 'resumel-moderncv)
