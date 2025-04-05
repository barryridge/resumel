(let* ((compiler (or (cdr (assoc "COMPILER" resumel-template-vars)) "pdflatex"))
       (geometry (or (cdr (assoc "GEOMETRY" resumel-template-vars)) "left=1.25cm,right=1.25cm,top=1.5cm,bottom=1.5cm,columnsep=1.2cm"))
       (documentclass-options (or (cdr (assoc "DOCUMENTCLASS_OPTIONS" resumel-template-vars)) "10pt,letterpaper,ragged2e,withhyper"))
       (main-font-xelatex (or (cdr (assoc "MAIN_FONT_XELATEX" resumel-template-vars)) "Roboto Slab"))
       (sans-font-xelatex (or (cdr (assoc "SANS_FONT_XELATEX" resumel-template-vars)) "Lato"))
       (mono-font-xelatex (or (cdr (assoc "MONO_FONT_XELATEX" resumel-template-vars)) "Fira Code"))
       (math-font-xelatex (or (cdr (assoc "MATH_FONT_XELATEX" resumel-template-vars)) "TeX Gyre Termes Math"))
       (main-font-pdflatex (or (cdr (assoc "MAIN_FONT_PDFLATEX" resumel-template-vars)) "roboto"))
       (sans-font-pdflatex (or (cdr (assoc "SANS_FONT_PDFLATEX" resumel-template-vars)) "lato"))
       (mono-font-pdflatex (or (cdr (assoc "MONO_FONT_PDFLATEX" resumel-template-vars)) "sourcecodepro"))
       (math-font-pdflatex (or (cdr (assoc "MATH_FONT_PDFLATEX" resumel-template-vars)) "newtxmath"))
       (cvtag-intensity-default (or (cdr (assoc "CVTAG_INTENSITY_DEFAULT" resumel-template-vars)) "5"))
       (cvtag-font-default (or (cdr (assoc "CVTAG_FONT_DEFAULT" resumel-template-vars)) "\\scriptsize"))
       (cvtag-baseline-default (or (cdr (assoc "CVTAG_BASELINE_DEFAULT" resumel-template-vars)) "-0.5ex"))
       (cvtag-inner-x-sep-default (or (cdr (assoc "CVTAG_INNER_X_SEP_DEFAULT" resumel-template-vars)) "0.5ex"))
       (cvtag-inner-y-sep-default (or (cdr (assoc "CVTAG_INNER_Y_SEP_DEFAULT" resumel-template-vars)) "0.5ex"))
       (cvtag-text-height-default (or (cdr (assoc "CVTAG_TEXT_HEIGHT_DEFAULT" resumel-template-vars)) "1.25ex"))
       (cvtag-text-depth-default (or (cdr (assoc "CVTAG_TEXT_DEPTH_DEFAULT" resumel-template-vars)) "0.25ex"))
       (cvtag-corner-default (or (cdr (assoc "CVTAG_CORNER_DEFAULT" resumel-template-vars)) "rounded corners"))
       (altacv-columnratio (or (cdr (assoc "ALTACV_COLUMNRATIO" resumel-template-vars)) "0.6")))

  (setq org-latex-compiler compiler)

  (add-to-list 'org-latex-classes
               `("resumel-altacv"
                 ,(concat "\\documentclass[" documentclass-options "]{altacv}

% Layout
\\geometry{" geometry "}

% Use paracol for column layout
\\usepackage{paracol}

% Set the left/right column width ratio
\\columnratio{" altacv-columnratio "}

% Fonts
\\iftutex
  % If using xelatex or lualatex:
  \\setmainfont{" main-font-xelatex "} % Main (serif/roman) font
  \\setsansfont{" sans-font-xelatex "} % Sans-serif font
  % \\setmonofont{" mono-font-xelatex "} % Monospace font
  % \\setmathfont{" math-font-xelatex "} % Math font (compatible with Roboto Slab)
  \\renewcommand{\\familydefault}{\\sfdefault}
\\else
  % If using pdflatex:
  \\usepackage[rm]{" main-font-pdflatex "} % Main (serif/roman) font
  \\usepackage[defaultsans]{" sans-font-pdflatex "} % Sans-serif font
  % \\usepackage[ttdefault]{" mono-font-pdflatex "} % Monospace font
  % \\usepackage{" math-font-pdflatex "} % Math font
  \\renewcommand{\\familydefault}{\\sfdefault}
\\fi

% Colors
\\definecolor{Black}{HTML}{000000}
\\definecolor{SlateGrey}{HTML}{2E2E2E}
\\definecolor{LightGrey}{HTML}{666666}
\\definecolor{DarkPastelRed}{HTML}{450808}
\\definecolor{PastelRed}{HTML}{8F0D0D}
\\definecolor{Blue}{HTML}{3872B2}
\\definecolor{DarkBlue}{HTML}{1F4064}
\\definecolor{GoldenEarth}{HTML}{E7D192}
\\definecolor{CoolSky}{HTML}{92CDE7}
\\definecolor{SoftSkyBlue}{HTML}{97D5EF}
\\colorlet{name}{Black}
\\colorlet{tagline}{PastelRed}
\\colorlet{heading}{DarkPastelRed}
\\colorlet{headingrule}{GoldenEarth}
\\colorlet{subheading}{PastelRed}
\\colorlet{accent}{PastelRed}
\\colorlet{emphasis}{SlateGrey}
\\colorlet{body}{LightGrey}
\\colorlet{color0}{Black}
\\colorlet{color1}{DarkPastelRed}
\\colorlet{color2}{SlateGrey}

% Update fonts
\\renewcommand{\\familydefault}{\\sfdefault}
\\renewcommand{\\namefont}{\\Huge\\rmfamily\\bfseries}
\\renewcommand{\\personalinfofont}{\\footnotesize}
\\renewcommand{\\cvsectionfont}{\\LARGE\\rmfamily\\bfseries}
\\renewcommand{\\cvsubsectionfont}{\\large\\bfseries}

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
\\RenewDocumentCommand{\\cvtag}{m
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

% Math stuff...
\\usepackage{amsmath}
\\usepackage{amsfonts}

")

                 ("\n\\cvsection{%s}" . "\n\\cvsection*{%s}")
                 ("\n\\cvsubsection{%s}" . "\n\\cvsubsection*{%s}"))))

(provide 'resumel-altacv)
