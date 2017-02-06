(defun latex-article (title date)
  (interactive "sTitle: \nsDate: ")
  (insert (format "
\\input{preamble}

\\title{%s}
\\author{Arpon Raksit}
\\date{%s}
"
                  title date))
  (insert "
\\numberwithin{block}{section}

\\begin{document}
\\maketitle

% ---------------------------------------------------------------------

"
          )
  (save-excursion (insert "

% ---------------------------------------------------------------------

\\bibliographystyle{amsalpha}
\\bibliography{refs}

\\end{document}
"
          )))




(defun latex-preamble ()
  (interactive)
  (insert "
\\documentclass[11pt,leqno]{article}

\\linespread{1.1}
\\frenchspacing

\\usepackage[%
  tmargin=1in,bmargin=1in,%
  lmargin=1.6in,rmargin=1.6in,%
  %marginparsep=0.2in, marginparwidth=1.8in%
]{geometry}
\\setlength{\\skip\\footins}{3.0ex}

\\usepackage{fancyhdr}
\\usepackage{titlesec}
\\usepackage{appendix}
\\usepackage{microtype}
\\usepackage[bottom]{footmisc}
\\usepackage[hyphens]{url}
\\usepackage{enumitem}
\\usepackage{xspace}
\\usepackage{calc}
\\usepackage{etoolbox}
\\usepackage{ifthen}
\\usepackage{tikz}
\\usepackage{tikz-cd}


\\definecolor{darkred}{rgb}{0.5,0.0,0.0}
\\usepackage[%
  hyperfootnotes=false,
  colorlinks,%
  linkcolor=darkred,%
  citecolor=darkred,%
  urlcolor=darkred,%
]{hyperref}
\\urlstyle{rm}

\\usepackage{amsmath,amsthm,amssymb}
\\usepackage{fourier}
\\usepackage[osf,scaled=.92]{heuristica}
\\usepackage[%
  cal=euler,   calscaled=0.96,%
  scr=boondox, scrscaled=0.95,%
  bb=boondox,  bbscaled=1.0,%
]{mathalfa}
\\usepackage[textsize=footnotesize,backgroundcolor=orange!50]{todonotes}
\\usepackage{cleveref}
\\usepackage{xr}




\\AtBeginDocument{%
  \\setlength{\\abovedisplayskip}{1.5ex plus 0.3ex minus 0.3ex}%
  \\setlength{\\abovedisplayshortskip}{1.0ex plus 0.3ex minus 0.3ex}%
  \\setlength{\\belowdisplayskip}{1.5ex plus 0.3ex minus 0.3ex}%
  \\setlength{\\belowdisplayshortskip}{1.0ex plus 0.3ex minus 0.3ex}%
}

\\let\\theoldbibliography\\thebibliography
\\renewcommand{\\thebibliography}[1]{%
  \\theoldbibliography{#1}%
  \\setlength{\\parskip}{0ex}
  \\setlength{\\itemsep}{0.5ex plus 0.2ex minus 0.2ex}
  \\small
}

\\pagestyle{fancy}
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}
\\fancyhf{}
\\fancyfoot[C]{\\small\\thepage}

\\renewcommand{\\title}[1]{\\newcommand{\\thetitle}{#1}}
\\renewcommand{\\author}[1]{\\newcommand{\\theauthor}{#1}}
\\renewcommand{\\date}[1]{\\newcommand{\\thedate}{#1}}

\\renewcommand{\\maketitle}{%
  \\begin{flushleft}
    {\\bfseries\\MakeUppercase{%
      \\thetitle}}\\\\[1.5ex]
    {\\footnotesize\\MakeUppercase{%
      \\theauthor}}\\\\[1.5ex]
    \\ifthenelse{\\equal{\\thedate}{}}{}{%
      \\small%
      % \\setlength{\\tabcolsep}{0.2em}%
      % \\begin{tabular}{rl}
        original: \\thedate \\\\
        updated: \\today
      % \\end{tabular}
    }
  \\end{flushleft}
  \\vspace{2.5ex}
  \\thispagestyle{fancy}
}

\\renewenvironment{abstract}{\\section*{Abstract}}{}


% ---------------------------------------------------------------------

\\newlength{\\tagsep}
\\setlength{\\tagsep}{1em}

% center display math with respect to full page
% -- amsart.cls
\\makeatletter
\\def\\fullwidthdisplay{\\displayindent\\z@ \\displaywidth\\columnwidth}
\\edef\\@tempa{\\noexpand\\fullwidthdisplay\\the\\everydisplay}
\\everydisplay\\expandafter{\\@tempa}
\\makeatother

% equation numbering in left margin
% -- http://tex.stackexchange.com/questions/59244
\\makeatletter
\\let\\mytagform@=\\tagform@
\\def\\tagform@#1{\\maketag@@@{\\hbox{\\llap{\\ignorespaces#1\\unskip\\@@italiccorr\\hspace{\\tagsep}}}}\\kern1sp}
\\renewcommand{\\eqref}[1]{{\\mytagform@{\\ref{#1}}}}
\\makeatother

\\titleformat{\\section}{\\bfseries}{\\llap{\\S\\thesection\\hspace{\\tagsep}}}{0em}{}
\\titlespacing*{\\section}{0pt}{*6}{*2}
\\titleformat{\\subsection}{\\scshape}{\\llap{\\S\\thesubsection\\hspace{\\tagsep}}}{0em}{}
\\titlespacing*{\\subsection}{0pt}{*4}{*2}

% Display format for equations
\\newcommand{\\crefeqfmt}[1]{
  \\crefformat{#1}{(##2##1##3)}
  \\Crefformat{#1}{(##2##1##3)}
  \\crefrangeformat{#1}{(##3##1##4--##5##2##6)}
  \\Crefrangeformat{#1}{(##3##1##4--##5##2##6)}
  \\crefmultiformat{#1}{(##2##1##3}{, ##2##1##3)}{, ##2##1##3}{, ##2##1##3)}
  \\Crefmultiformat{#1}{(##2##1##3}{, ##2##1##3)}{, ##2##1##3}{, ##2##1##3)}
  \\crefrangemultiformat{#1}{(##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6)}{, ##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6)}
  \\Crefrangemultiformat{#1}{(##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6)}{, ##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6)}
}
% Display format for sections
\\newcommand{\\crefsecfmt}[1]{%
  \\crefformat{#1}{\\S##2##1##3}
  \\Crefformat{#1}{\\S##2##1##3}
  \\crefrangeformat{#1}{\\S\\S##3##1##4--##5##2##6}
  \\Crefrangeformat{#1}{\\S\\S##3##1##4--##5##2##6}
  \\crefmultiformat{#1}{\\S\\S##2##1##3}{ and~##2##1##3}{, ##2##1##3}{ and~##2##1##3}
  \\Crefmultiformat{#1}{\\S\\S##2##1##3}{ and~##2##1##3}{, ##2##1##3}{ and~##2##1##3}
  \\crefrangemultiformat{#1}{\\S\\S##3##1##4--##5##2##6}{ and~##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6}{ and~##3##1##4--##5##2##6}
  \\Crefrangemultiformat{#1}{\\S\\S##3##1##4--##5##2##6}{ and~##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6}{ and~##3##1##4--##5##2##6}
}
\\crefeqfmt{equation}
\\crefeqfmt{enumi}
\\crefeqfmt{enumii}
\\crefsecfmt{section}
\\crefsecfmt{subsection}
\\crefsecfmt{appendix}
\\crefname{part}{Part}{Parts}
\\crefname{chapter}{Chapter}{Chapters}
\\crefname{figure}{Figure}{Figures}

\\makeatletter


\\newcommand{\\thmnumfont}{}
\\newcommand{\\thmheadfont}{\\scshape}
\\newcommand{\\thmnotefont}{\\scshape}
\\newcommand{\\thmhorizspace}{0.3em}
\\newcommand{\\thmnotespace}{0.25em}
\\newcommand{\\thmsep}{\\hspace{\\thmhorizspace}---}

\\newtheoremstyle{block}%
  {1.5ex plus 0.1ex minus 0.1ex}% Space above
  {1.5ex plus 0.1ex minus 0.1ex}% Space below
  {}% Body font
  {}% Indent amount
  {\\thmheadfont} % Theorem head font
  {}% Punctuation after theorem head
  {0em}% Space after theorem head
  {\\llap{\\thmnumber{{\\thmnumfont #2}\\hspace{\\tagsep}}}%
    \\thmname{#1}%
    \\thmnote{\\hspace{\\thmnotespace}\\thmnotefont(#3)}%
    \\@ifnotempty{#1}{\\thmsep\\hspace{\\thmhorizspace}}%
  }% Custom head spec

\\newcommand{\\defemph}[1]{\\textit{#1}}

\\renewenvironment{proof}[1][Proof]{\\par
  \\pushQED{\\qed}%
  \\normalfont%
  \\topsep1ex plus 0.2ex minus 0.1ex\\relax%
  \\labelsep \\thmhorizspace\\relax%
  \\trivlist
  \\item[\\hskip\\labelsep\\thmheadfont#1\\@addpunct{\\thmsep}]\\ignorespaces
}{%
  \\popQED\\endtrivlist\\@endpefalse%
}

\\makeatother

\\theoremstyle{block}

\\newcounter{block}
\\newcounter{subblock}
\\numberwithin{subblock}{block}
\\numberwithin{equation}{subblock}

\\newcommand{\\defthm}[2]{%
  \\newtheorem{#1}[block]{#2}%
  \\crefeqfmt{#1}%
  \\newtheorem*{#1*}{#2}%

  \\newtheorem{sub#1}[subblock]{#2}%
  \\crefeqfmt{sub#1}%
  \\newtheorem*{sub#1*}{#2}%
}

\\defthm{algorithm}{Algorithm}
\\defthm{assumption}{Assumption}
\\defthm{case}{Case}
\\defthm{conjecture}{Conjecture}
\\defthm{construction}{Construction}
\\defthm{convention}{Convention}
\\defthm{corollary}{Corollary}
\\defthm{definition}{Definition}
\\defthm{definitions}{Definitions}
\\defthm{example}{Example}
\\defthm{examples}{Examples}
\\defthm{exercise}{Exercise}
\\defthm{fact}{Fact}
\\defthm{intuition}{Intuition}
\\defthm{lemma}{Lemma}
\\defthm{notation}{Notation}
\\defthm{nothing}{}
\\defthm{proposition}{Proposition}
\\defthm{question}{Question}
\\defthm{recall}{Recall}
\\defthm{remark}{Remark}
\\defthm{remarks}{Remarks}
\\defthm{situation}{Situation}
\\defthm{theorem}{Theorem}

\\makeatletter
\\long\\def\\XR@test#1#2#3#4\\XR@{%
  \\ifx#1\\newlabel
    \\xr@cref#2@cref\\relax#3\\@nil
  \\else\\ifx#1\\@input
     \\edef\\XR@list{\\XR@list#2\\relax}%
  \\fi\\fi
  \\ifeof\\@inputcheck\\expandafter\\XR@aux
  \\else\\expandafter\\XR@read\\fi}

\\def\\xr@@cref{@cref}
\\def\\xr@cr@add#1{{[\\XR@prefix\\@gobble#1}}
\\def\\xr@cref#1@cref#2\\relax#3\\@nil{%
\\def\\tmp{#2}%
\\ifx\\tmp\\xr@@cref
  \\edef\\tmp{\\noexpand\\newlabel{\\XR@prefix#1@cref}{\\xr@cr@add#3}}%
  \\tmp
\\else
 \\newlabel{\\XR@prefix#1}{#3}%
\\fi
}
\\makeatother

% Display format for equations
\\newcommand{\\externalcrefeqfmt}[2]{
  \\crefformat{#1:#2}{[\\texttt{#1}, ##2##1##3]}
  \\Crefformat{#1:#2}{[\\texttt{#1}, ##2##1##3]}
  \\crefrangeformat{#1:#2}{[\\texttt{#1}, ##3##1##4--##5##2##6]}
  \\Crefrangeformat{#1:#2}{[\\texttt{#1}, ##3##1##4--##5##2##6]}
  \\crefmultiformat{#1:#2}{[\\texttt{#1}, ##2##1##3}{, ##2##1##3]}{, ##2##1##3}{, ##2##1##3]}
  \\Crefmultiformat{#1:#2}{[\\texttt{#1}, ##2##1##3}{, ##2##1##3]}{, ##2##1##3}{, ##2##1##3]}
  \\crefrangemultiformat{#1:#2}{[\\texttt{#1}, ##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6]}{, ##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6]}
  \\Crefrangemultiformat{#1:#2}{[\\texttt{#1}, ##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6]}{, ##3##1##4--##5##2##6}{, ##3##1##4--##5##2##6]}
}
% Display format for sections
\\newcommand{\\externalcrefsecfmt}[2]{%
  \\crefformat{#1:#2}{[\\texttt{#1}, \\S##2##1##3]}
  \\Crefformat{#1:#2}{[\\texttt{#1}, \\S##2##1##3]}
  \\crefrangeformat{#1:#2}{[\\texttt{#1}, \\S\\S##3##1##4--##5##2##6]}
  \\Crefrangeformat{#1:#2}{[\\texttt{#1}, \\S\\S##3##1##4--##5##2##6]}
  \\crefmultiformat{#1:#2}{[\\texttt{#1}, \\S\\S##2##1##3}{ and~##2##1##3]}{, ##2##1##3}{ and~##2##1##3]}
  \\Crefmultiformat{#1:#2}{[\\texttt{#1}, \\S\\S##2##1##3}{ and~##2##1##3]}{, ##2##1##3}{ and~##2##1##3]}
  \\crefrangemultiformat{#1:2}{[\\texttt{#1}, \\S\\S##3##1##4--##5##2##6}{ and~##3##1##4--##5##2##6]}{, ##3##1##4--##5##2##6}{ and~##3##1##4--##5##2##6]}
  \\Crefrangemultiformat{#1:#2}{[\\texttt{#1}, \\S\\S##3##1##4--##5##2##6}{ and~##3##1##4--##5##2##6]}{, ##3##1##4--##5##2##6}{ and~##3##1##4--##5##2##6]}
}

\\newcommand{\\GRAINROOT}{/Users/arpon/Documents/grain}
\\newcommand{\\externalgrain}[1]{
  \\externaldocument[#1:]{\\GRAINROOT /nodes/#1/#1}
  
  \\externalcrefeqfmt{#1}{algorithm}
  \\externalcrefeqfmt{#1}{case}
  \\externalcrefeqfmt{#1}{conjecture}
  \\externalcrefeqfmt{#1}{construction}
  \\externalcrefeqfmt{#1}{convention}
  \\externalcrefeqfmt{#1}{corollary}
  \\externalcrefeqfmt{#1}{definition}
  \\externalcrefeqfmt{#1}{definitions}
  \\externalcrefeqfmt{#1}{example}
  \\externalcrefeqfmt{#1}{examples}
  \\externalcrefeqfmt{#1}{exercise}
  \\externalcrefeqfmt{#1}{fact}
  \\externalcrefeqfmt{#1}{intuition}
  \\externalcrefeqfmt{#1}{lemma}
  \\externalcrefeqfmt{#1}{notation}
  \\externalcrefeqfmt{#1}{nothing}
  \\externalcrefeqfmt{#1}{proposition}
  \\externalcrefeqfmt{#1}{question}
  \\externalcrefeqfmt{#1}{recall}
  \\externalcrefeqfmt{#1}{remark}
  \\externalcrefeqfmt{#1}{remarks}
  \\externalcrefeqfmt{#1}{situation}
  \\externalcrefeqfmt{#1}{theorem}

  \\externalcrefeqfmt{#1}{equation}
  \\externalcrefeqfmt{#1}{enumi}
  \\externalcrefeqfmt{#1}{enumii}
  \\externalcrefsecfmt{#1}{section}
  \\externalcrefsecfmt{#1}{subsection}
  \\externalcrefsecfmt{#1}{appendix}
}

\\setlist{%
  parsep=0ex, listparindent=\\parindent,%
  itemsep=0.75ex, topsep=0.75ex,%
  leftmargin=2.5em,%
}

\\setlist[enumerate, 1]{%
  label=(\\emph{\\alph*}),%
  ref={\\emph{\\alph*}},%
  widest=d,
}
\\setlist[enumerate, 2]{%
  leftmargin=*,%
  label=(\\theenumi.\\arabic*),%
  ref=\\theenumi.\\arabic*,%
}
\\setlist[itemize, 1]{%
  label=$\\vcenter{\\hbox{\\footnotesize$\\blacktriangleright$}}$,%
}
\\setlist[itemize, 2]{%
  label=--,%
}

% ---------------------------------------------------------------------

\\makeatletter

\\let\\ea\\expandafter

\\newcount\\foreachcount

\\def\\foreachletter#1#2#3{\\foreachcount=#1
  \\ea\\loop\\ea\\ea\\ea#3\\@alph\\foreachcount
  \\advance\\foreachcount by 1
  \\ifnum\\foreachcount<#2\\repeat}

\\def\\foreachLetter#1#2#3{\\foreachcount=#1
  \\ea\\loop\\ea\\ea\\ea#3\\@Alph\\foreachcount
  \\advance\\foreachcount by 1
  \\ifnum\\foreachcount<#2\\repeat}

% Roman: \\rA is \\mathrm{A}
\\def\\definerm#1{%
  \\ea\\gdef\\csname r#1\\endcsname{\\ensuremath{\\mathrm{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\definerm}
\\foreachletter{1}{27}{\\definerm}
% Script: \\sA is \\mathscr{A}
\\def\\definescr#1{%
  \\ea\\gdef\\csname s#1\\endcsname{\\ensuremath{\\mathscr{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\definescr}
% Calligraphic: \\cA is \\mathcal{A}
\\def\\definecal#1{%
  \\ea\\gdef\\csname c#1\\endcsname{\\ensuremath{\\mathcal{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\definecal}
% Bold: \\bA is \\mathbf{A}
\\def\\definebold#1{%
  \\ea\\gdef\\csname b#1\\endcsname{\\ensuremath{\\mathbf{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\definebold}
% Blackboard Bold: \\lA is \\mathbb{A}
\\def\\definebb#1{%
  \\ea\\gdef\\csname l#1\\endcsname{\\ensuremath{\\mathbb{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\definebb}
% Fraktur: \\ka is \\mathfrak{a}, \\kA is \\mathfrak{A}
\\def\\definefrak#1{%
  \\ea\\gdef\\csname k#1\\endcsname{\\ensuremath{\\mathfrak{#1}}\\xspace}}
\\foreachletter{1}{27}{\\definefrak}
\\foreachLetter{1}{27}{\\definefrak}
% Sans serif: \\iA \\is \\mathsf{A}
\\def\\definesf#1{%
  \\ea\\gdef\\csname i#1\\endcsname{\\ensuremath{\\mathsf{#1}}\\xspace}}
\\foreachletter{1}{6}{\\definesf}
\\foreachletter{7}{14}{\\definesf}
\\foreachletter{15}{27}{\\definesf}
\\foreachLetter{1}{27}{\\definesf}
% Bar: \\Abar is \\overline{A}, \\abar is \\overline{a}
\\def\\definebar#1{%
  \\ea\\gdef\\csname #1bar\\endcsname{\\ensuremath{\\overline{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\definebar}
\\foreachletter{1}{8}{\\definebar} % \\hbar is something else!
\\foreachletter{9}{15}{\\definebar} % \\obar is something else!
\\foreachletter{16}{27}{\\definebar}
% Tilde: \\Atil is \\widetilde{A}, \\atil is \\widetilde{a}
\\def\\definetil#1{%
  \\ea\\gdef\\csname #1til\\endcsname{\\ensuremath{\\widetilde{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\definetil}
\\foreachletter{1}{27}{\\definetil}
% Hats: \\Ahat is \\widehat{A}, \\ahat is \\widehat{a}
\\def\\definehat#1{%
  \\ea\\gdef\\csname #1hat\\endcsname{\\ensuremath{\\widehat{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\definehat}
\\foreachletter{1}{27}{\\definehat}
% Checks: \\Achk is \\widecheck{A}, \\achk is \\widecheck{a}
\\def\\definechk#1{%
  \\ea\\gdef\\csname #1chk\\endcsname{\\ensuremath{\\widecheck{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\definechk}
\\foreachletter{1}{27}{\\definechk}
% Underline: \\Aund is \\underline{A}, \\aund is \\underline{a}
\\def\\defineul#1{%
  \\ea\\gdef\\csname #1und\\endcsname{\\ensuremath{\\underline{#1}}\\xspace}}
\\foreachLetter{1}{27}{\\defineul}
\\foreachletter{1}{27}{\\defineul}

\\makeatother

% ---------------------------------------------------------------------

\\usetikzlibrary{calc,decorations.pathmorphing,shapes,arrows}
\\tikzcdset{
  arrow style=tikz,
  diagrams={>={stealth}},
}

\\newcommand{\\arrlen}{1.25em}
\\newcommand{\\shortarrlen}{0.85em}
\\renewcommand{\\to}{\\mathrel{\\tikz[baseline]%
    \\draw[>=stealth,->](0,0.5ex)--(\\arrlen,0.5ex);}}
\\newcommand{\\limto}{\\mathrel{\\tikz[baseline]%
    \\draw[>=stealth,->](0,0.4ex)--(\\shortarrlen,0.4ex);}}
\\newcommand{\\from}{\\mathrel{\\tikz[baseline]%
    \\draw[>=stealth,<-](0,0.5ex)--(\\arrlen,0.5ex);}}
\\renewcommand{\\mapsto}{\\mathrel{\\tikz[baseline]%
    \\draw[>=stealth,|->](0,0.5ex)--(\\arrlen,0.5ex);}}
\\newcommand{\\inj}{\\mathrel{\\tikz[baseline]%
    \\draw[>=stealth,right hook->](0,0.5ex)--(\\arrlen,0.5ex);}}
\\newcommand{\\surj}{\\mathrel{\\tikz[baseline]%
    \\draw[>=stealth,->>](0,0.5ex)--(\\arrlen,0.5ex);}}
\\newcommand{\\fromto}{\\mathrel{%
  \\begin{tikzpicture}[baseline]%
    \\draw[>=stealth,<-](0,0.15ex)--(\\arrlen,0.15ex);%
    \\draw[>=stealth,->](0,0.85ex)--(\\arrlen,0.85ex);%
  \\end{tikzpicture}}}
\\newcommand{\\doubto}{\\mathrel{%
  \\begin{tikzpicture}[baseline]%
    \\draw[>=stealth,->](0,0.15ex)--(\\arrlen,0.15ex);%
    \\draw[>=stealth,->](0,0.85ex)--(\\arrlen,0.85ex);%
  \\end{tikzpicture}}}
\\newcommand{\\goesto}{\\mathrel{%
  \\begin{tikzpicture}[baseline= {( $ (current bounding box.south) + (0,-0.3ex) $ )}]%
    \\draw[>=stealth,->,decorate,%
          decoration={zigzag,amplitude=0.15ex,segment length=0.35em,pre=lineto,pre length=.15em,post=lineto,post length=.3em}](0,0.15ex)--(\\arrlen,0.15ex);%
  \\end{tikzpicture}}}
\\newcommand{\\lblto}[1]{\\mathrel{%
    \\begin{tikzpicture}[baseline= {( $ (current bounding box.south) + (0,-0.5ex) $ )}]
      \\node[inner sep=.4ex] (a) {\\,$\\scriptstyle #1$\\,};
      \\draw[>=stealth,->] (a.south west) -- (a.south east);
    \\end{tikzpicture}}}
\\newcommand{\\isoto}{\\lblto{\\sim}}

\\newcommand{\\simpl}[3]{
  \\begin{tikzcd}[ampersand replacement=\\&, column sep=small]
    #1 \\&
    #2 \\ar[l, shift right=0.35ex]
       \\ar[l, shift left=0.35ex] \\&
    #3 \\ar[l, shift right=0.70ex]
       \\ar[l, shift left=0.70ex]
       \\ar[l] \\&
    \\cdots \\ar[l, shift right=0.35ex]
           \\ar[l, shift left=0.35ex]
           \\ar[l, shift right=1.05ex]
           \\ar[l, shift left=1.05ex]
  \\end{tikzcd}
}
\\newcommand{\\cosimpl}[3]{
  \\begin{tikzcd}[ampersand replacement=\\&, column sep=small]
    #1 \\ar[r, shift right=0.35ex]
       \\ar[r, shift left=0.35ex] \\&
    #2 \\ar[r, shift right=0.70ex]
       \\ar[r, shift left=0.70ex]
       \\ar[r] \\&
    #3 \\ar[r, shift right=0.35ex]
       \\ar[r, shift left=0.35ex]
       \\ar[r, shift right=1.05ex]
       \\ar[r, shift left=1.05ex] \\&
    \\cdots
  \\end{tikzcd}
}

\\newcommand{\\tto}{\\mathrel{\\tikz[baseline]%
    \\draw[>=stealth,->,double, double distance = 0.3ex](0,0.5ex)--(\\arrlen,0.5ex);}}
\\newcommand{\\doubfrom}{\\mathrel{%
  \\begin{tikzpicture}[baseline]%
    \\draw[>=stealth,<-](0,0.15ex)--(\\arrlen,0.15ex);%
    \\draw[>=stealth,<-](0,0.85ex)--(\\arrlen,0.85ex);%
  \\end{tikzpicture}}}
\\newcommand{\\tripfrom}{\\mathrel{%
  \\begin{tikzpicture}[baseline]%
    \\draw[>=stealth,<-](0,0.00ex)--(\\arrlen,0.00ex);%
    \\draw[>=stealth,<-](0,0.50ex)--(\\arrlen,0.50ex);%
    \\draw[>=stealth,<-](0,1.00ex)--(\\arrlen,1.00ex);%
  \\end{tikzpicture}}}


\\renewcommand{\\l}{\\left}
\\renewcommand{\\r}{\\right}
\\renewcommand{\\f}{\\frac}
\\renewcommand{\\o}{\\overline}
\\renewcommand{\\u}{\\underline}
\\newcommand{\\til}{\\widetilde}
\\renewcommand{\\hat}{\\widehat}
\\newcommand{\\del}{\\partial}
\\newcommand{\\bul}{\\bullet}
\\newcommand{\\dash}{\\text{-}}
\\renewcommand{\\c}{\\colon}
\\newcommand{\\lc}{\\,:\\!}
\\newcommand{\\ce}{\\mathrel{:=}}%{\\coloneq}
\\newcommand{\\ec}{\\mathrel{=:}}%{\\eqcolon}
\\newcommand{\\iso}{\\simeq}
\\newcommand{\\lbliso}[1]{\\overset{#1}{\\simeq}}
\\newcommand{\\dual}[1]{#1^\\vee}
\\newcommand{\\ldb}{\\llbracket}
\\newcommand{\\rdb}{\\rrbracket}
\\newcommand{\\shimplies}{\\Rightarrow}
\\newcommand{\\shimplied}{\\Leftarrow}

\\newcommand{\\Obj}{\\operatorname{Obj}}
\\newcommand{\\Hom}{\\operatorname{Hom}}
\\newcommand{\\Map}{\\operatorname{Map}}
\\newcommand{\\Fun}{\\operatorname{Fun}}
\\newcommand{\\Aut}{\\operatorname{Aut}}
\\newcommand{\\End}{\\operatorname{End}}
\\newcommand{\\Iso}{\\operatorname{Iso}}
\\renewcommand{\\id}{\\mathrm{id}}
\\renewcommand{\\im}{\\operatorname{im}}
\\newcommand{\\pt}{\\star}
\\newcommand{\\op}{\\mathrm{op}}
\\newcommand{\\univ}{\\mathrm{univ}}
\\newcommand{\\colim}{\\operatorname*{colim}}
\\newcommand{\\coeq}{\\operatorname*{coeq}}
\\newcommand{\\eq}{\\operatorname*{eq}}
\\newcommand{\\dlim}{\\displaystyle\\lim}
\\newcommand{\\dcolim}{\\displaystyle\\colim}
\\newcommand{\\Spec}{\\operatorname{Spec}}
\\newcommand{\\Spf}{\\operatorname{Spf}}

% ---------------------------------------------------------------------


"
          ))

