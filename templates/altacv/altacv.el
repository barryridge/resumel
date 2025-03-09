(unless (assoc "altacv" org-latex-classes)

(setq resumel-template-class "altacv")

(let* ((altacv-columnratio (or (cdr (assoc "ALTACV_COLUMNRATIO" resumel-template-vars)) "0.6"))
       (geometry (or (cdr (assoc "GEOMETRY" resumel-template-vars)) "left=1.25cm,right=1.25cm,top=1.5cm,bottom=1.5cm,columnsep=1.2cm")))
  (add-to-list 'org-latex-classes
               `("altacv"
                 ,(concat "\\documentclass[10pt,letterpaper,ragged2e,withhyper]{altacv}

% Layout
"
"\\geometry{" geometry "}

% Use paracol for column layout
\\usepackage{paracol}

% Set the left/right column width ratio
\\columnratio{" altacv-columnratio "}

% Fonts
\\usepackage[rm]{roboto}
\\usepackage[defaultsans]{lato}

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
% Update cvtag command to make tag boxes tighter and include skill intensity
%
\\usepackage{tikz}
\\usepackage{xcolor}
\\RenewDocumentCommand{\\cvtag}{m O{5}}{%
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

                 ("\\cvsection{%s}" . "\\cvsection*{%s}")
                 ("\\cvsubsection{%s}" . "\\cvsubsection*{%s}")))

  ;; Debug message to confirm addition
  ;; (message "[resumel - DEBUG]: Added 'altacv' to org-latex-classes: %s" (assoc "altacv" org-latex-classes))
))

(provide 'resumel-altacv)
