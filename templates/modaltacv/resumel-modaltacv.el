(unless (assoc "modaltacv" org-latex-classes)
  (add-to-list 'org-latex-classes
               '("altacv"
                 "\\documentclass[10pt,letterpaper,ragged2e,withhyper]{altacv}

% Page layout adjustments
\\geometry{left=1.25cm,right=1.25cm,top=1.5cm,bottom=1.5cm,columnsep=1.2cm}

% Use roboto and lato for fonts
\\renewcommand{\\familydefault}{\\sfdefault}

% Update colors
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
% \\colorlet{name}{LightGrey}
\\colorlet{tagline}{LightGrey}
\\colorlet{heading}{Blue}
\\colorlet{headingrule}{Blue}
\\colorlet{subheading}{LightGrey}
\\colorlet{accent}{LightGrey}
\\colorlet{emphasis}{SlateGrey}
\\colorlet{body}{SlateGrey}

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

% Update CV header & section headings - socials on multiple lines
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

% Update cvsection command to tighten up heading lines
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

% Math stuff...
\\usepackage{amsmath}
\\usepackage{amsfonts}

% Itemize labels from moderncv
\\definecolor{color1}{rgb}{0,0,0}
\\colorlet{color1}{Blue}
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
               ("\\cvevent{%s}" . "\\cvevent*{%s}"))))

(provide 'resumel-modaltacv)
