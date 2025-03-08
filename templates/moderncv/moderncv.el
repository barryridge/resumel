(unless (assoc "moderncv" org-latex-classes)

;; Disable Org's hyperref template - let moderncv handle it
(setq org-latex-hyperref-template nil)
(setq org-latex-default-packages-alist nil)
(setq org-latex-packages-alist nil)

(add-to-list 'org-latex-classes
               '("moderncv"
                 "\\documentclass[11pt,letterpaper,sans]{moderncv}

% Set default ModernCV color
\\moderncvcolor{blue}


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
\\moderncvstyle{classic}

% To make cover letter text justified
\\usepackage{etoolbox}% http://ctan.org/pkg/etoolbox
\\makeatletter
\\patchcmd{\\makeletterhead}% <cmd>
  {\\raggedright \\@opening}% <search>
  {\\@opening}% <replace>
  {}{}% <success><failure>
\\makeatother

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

% Highlight macro used to boldface name in .bib file
\\newcommand{\\highlight}[1]{\\textbf{#1}} % Replace \\textbf with any desired formatting

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
                 ("\\subsection{%s}" . "\\subsection*{%s}"))))

(provide 'resumel-moderncv)
