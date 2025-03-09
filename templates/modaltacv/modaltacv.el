(unless (assoc "modaltacv" org-latex-classes)

;; Set resumel-template-class buffer-locally
(setq resumel-template-class "altacv")

(setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf")))
  (add-to-list 'org-latex-classes
               '("altacv"
                 "\\documentclass[10pt,letterpaper,ragged2e,withhyper]{altacv}

% Page layout adjustments
\\geometry{left=1.25cm,right=1.25cm,top=1.5cm,bottom=1.5cm,columnsep=1.2cm}

% Use roboto and lato for fonts
\\renewcommand{\\familydefault}{\\sfdefault}

% Update altacv colors
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
\\colorlet{tagline}{LightGrey}
\\colorlet{heading}{Blue}
\\colorlet{headingrule}{Blue}
\\colorlet{subheading}{LightGrey}
\\colorlet{accent}{LightGrey}
\\colorlet{emphasis}{SlateGrey}
\\colorlet{body}{SlateGrey}

% Set moderncv colors
\\definecolor{black}{RGB}{0, 0, 0}
\\definecolor{red}{rgb}{0.95, 0.20, 0.20}
\\definecolor{darkgrey}{rgb}{0.45, 0.45, 0.45}
\\definecolor{orange}{rgb}{0.95, 0.55, 0.15}
\\definecolor{burgundy}{rgb}{0.596078, 0, 0}% 139/255 (0.545098) or 152/255 (0.596078)
\\definecolor{purple}{rgb}{0.50, 0.33, 0.80}
\\definecolor{lightblue}{rgb}{0.22, 0.45, 0.70}
\\definecolor{green}{rgb}{0.35, 0.70, 0.30}
\\colorlet{color0}{black}
\\colorlet{color1}{lightblue}
\\colorlet{color2}{darkgrey}

% Update fonts
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern} % Loads Latin Modern Roman, Sans, and Mono
\\renewcommand{\\familydefault}{\\sfdefault}
\\renewcommand{\\namefont}{\\Huge\\mdseries\\upshape}
\\renewcommand{\\taglinefont}{\\LARGE\\mdseries\\slshape}
\\renewcommand{\\personalinfofont}{\\footnotesize\\mdseries\\slshape}
\\renewcommand{\\cvsectionfont}{\\LARGE}
\\renewcommand{\\cvsubsectionfont}{\\large\\bfseries}

% Update info fields & symbols
\\NewInfoField{location}{\\faMapMarker}
\\NewInfoField{mailaddress}{\\faEnvelope}
\\NewInfoField{phone}{\\faMobile}[tel:]
\\NewInfoField{email}{\\faEnvelope}[mailto:]
\\NewInfoField{homepage}{\\faGlobeAmericas}[https://]
\\NewInfoField{twitter}{\\faTwitter}[https://twitter.com/]
\\NewInfoField{linkedin}{\\faLinkedinIn}[https://linkedin.com/in/]
\\NewInfoField{github}{\\faGithub}[https://github.com/]
% v1.?? Use fontawesome5 for Orcid symbol
\\NewInfoField{orcid}{\\faOrcid}[https://orcid.org/]
\\newcommand*{\\scholarsocialsymbol}{\\includegraphics[height=1em]{google_scholar_logo_bw.pdf}}
\\NewInfoField*{scholar}{\\scholarsocialsymbol}

% CV Header
%
% Update CV header & section headings - socials on multiple lines
%
\\makeatletter
\\renewcommand{\\makecvheader}{%
  \\begingroup
    \\begin{minipage}[t]{0.8365\\linewidth}
    {\\namefont\\color{name}\\@name\\par}
    \\vspace{0.9em}
    {\\taglinefont\\color{tagline}\\@tagline\\par}
    \\end{minipage}%
    \\hfill%
    % Right side for social links, allowing multi-line content
    \\begin{minipage}[t]{0.2\\linewidth}
    \\raggedleft
    {\\personalinfofont\\@personalinfo\\par}
    \\end{minipage}%
  \\endgroup
  \\vspace{-0.7em}
}
\\makeatother

% CV Sections
%
% Update cvsection command to tighten up heading lines
%
\\renewcommand{\\cvsection}[2][]{%
  \\par\\bigskip%
  \\color{heading}%
  \\noindent%
  \\hspace*{-\\parindent}%
  \\begin{minipage}{\\linewidth}%
  \\raisebox{-0.15em}{\\cvsectionfont{#2}}\\hspace{0.5em}%
  \\color{headingrule}\\leaders\\hrule height 4pt\\hfill\\kern0pt%
  \\end{minipage}%
  \\par\\medskip%
  \\color{body}%
}

% CV Events
%
% Update cvevent command to tighten up location next to date
%
\\renewcommand{\\cvevent}[4]{%
  {\\large\\color{color0}#1\\par} % Event title in color0
  \\smallskip\\normalsize
  \\ifstrequal{#2}{}{}{%
  \\textbf{\\color{color0}#2}\\par} % Subtitle/organization in color0
  \\smallskip
  \\ifstrequal{#3}{}{}{%
    {\\small\\makebox[0.5\\linewidth][l]%
      {\\BeginAccSupp{method=pdfstringdef,ActualText={\\datename:}}{\\color{color2}\\cvDateMarker}\\EndAccSupp{}% Date marker in color2
      ~\\textcolor{color0}{#3}% Date text in color0
      \\ifstrequal{#4}{}{}{, ~{\\color{color2}\\cvLocationMarker}~\\textcolor{color0}{#4}}% Location marker in color2, text in color0
    }}%
  }\\par
  \\medskip\\normalsize
}

% CV References
%
% Update to use moderncv colors for symbols
%
\\renewcommand{\\cvref}[3]{%
  \\smallskip
  \\textcolor{emphasis}{\\textbf{#1}}\\par
  \\begin{description}[font=\\color{accent},style=multiline,leftmargin=1.35em,align=left]
  \\item[\\color{color2}\\small\\normalfont\\emailsymbol] #2
  \\item[\\color{color2}\\small\\normalfont\\mailaddresssymbol] #3
  \\end{description}
%   \\medskip
}

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

% CV Award
%
% Modified altacv cvachievement command to override icon size
%
\\newcommand{\\cvaward}[4]{%
  \\begin{tabularx}{\\linewidth}{@{}p{2em} @{\\hspace{1ex}} >{\\raggedright\\arraybackslash}X@{}}
  \\multirow{2}{*}{#1\\color{accent}\\BeginAccSupp{method=escape,ActualText={#2: }}#2\\EndAccSupp{}} & \\bfseries\\textcolor{emphasis}{#3}\\\\
  & #4
  \\end{tabularx}%
  \\smallskip
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

% Itemize labels from moderncv
\\renewcommand{\\labelitemi}{\\strut\\textcolor{color1}{\\large\\rmfamily\\small\\textbullet}}
\\renewcommand{\\labelitemiii}{\\strut\\textcolor{color1}{\\rmfamily\\textperiodcentered}}
\\renewcommand{\\labelitemiv}{\\labelitemiii}
\\newcommand*{\\listitemsymbol}{\\labelitemi~}

% Change the bullets for itemize and rating marker
\\renewcommand*{\\cvItemMarker}{\\strut\\textcolor{color1}{\\tiny\\faCircle[regular]}}
\\renewcommand{\\cvRatingMarker}{\\faCircle}

% Change the bullets for publications
% NOTE: to use these, the \input{pubs-authoryear.cfg} and \input{pubs-num.cfg} inclusions
% below under 'Bibliography' should be commented out.
%
\\usepackage[backend=biber,style=ieee,sorting=ydnt,defernumbers=true]{biblatex}
%% For removing numbering entirely when using a numeric style
\\setlength{\\bibhang}{1.25em}
\\DeclareFieldFormat{labelnumberwidth}{\\makebox[\\bibhang][l]{\\cvItemMarker}}
\\setlength{\\biblabelsep}{0pt}
\\defbibheading{pubtype}{\\cvsubsection{#1}}
\\renewcommand{\\bibsetup}{\\vspace*{-\\baselineskip}}
\\AtEveryBibitem{%
  \\iffieldundef{doi}{}{\\clearfield{url}}%
}

% When using APA6 if you need more author names to be listed
% because you're e.g. the 12th author, add apamaxprtauth=12
% \\usepackage[backend=biber,style=apa6,sorting=ydnt]{biblatex}
% \\defbibheading{pubtype}{\\cvsubsection{#1}}
% \\renewcommand{\\bibsetup}{\\vspace*{-\\baselineskip}}
% \\AtEveryBibitem{%
%   \\makebox[\\bibhang][l]{\\cvItemMarker}%
%   \\iffieldundef{doi}{}{\\clearfield{url}}%
% }
% \\setlength{\\bibitemsep}{0.25\\baselineskip}
% \\setlength{\\bibhang}{1.25em}
"

               ("\\cvsection{%s}" . "\\cvsection*{%s}")
               ("\\cvsubsection{%s}" . "\\cvsubsection*{%s}"))))

(setq org-latex-packages-alist 'nil)
(setq org-latex-default-packages-alist
      '(("" "lmodern" t)  ; Load lmodern package for Latin Modern fonts
        ("" "paracol" t)  ; Keep paracol for column layout
        ))

(provide 'resumel-modaltacv)
