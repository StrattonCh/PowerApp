\def\fileversion{1.0}
\def\filename{asa_title}
\def\filedate{2007/01/28}
%%
%% Modification of package `jss' to use with LaTeX2e for JSS publications
%%   http://www.jstatsoft.org/
%% Copyright (C) 2004 Achim Zeileis
%% 
%% %%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{asa}[\filedate\space\fileversion\space asa class based on jss]
%% options
\newif\if@article
\newif\if@shortnames

\@articletrue
\@shortnamesfalse

\DeclareOption{article}{\@articletrue}
\DeclareOption{article}{\@emptytrue}
\DeclareOption{shortnames}{\@shortnamestrue}

\ProcessOptions
\LoadClass[11pt,a4paper,twoside]{article}
%% required packages
\RequirePackage{graphicx,a4wide,color,hyperref,ae,fancyvrb}
\RequirePackage[T1]{fontenc}
%% bibliography
\if@shortnames
  \usepackage[authoryear,round]{natbib}
\else
  \usepackage[authoryear,round,longnamesfirst]{natbib}
\fi
\bibpunct{(}{)}{;}{a}{}{,}
\bibliographystyle{asa}
%% paragraphs
\setlength{\parskip}{1.0em}
\setlength{\parindent}{0em}
%% figure placement
\def\topfraction{.9}
\def\floatpagefraction{.8}
%% for all publications
\newcommand{\Address}[1]{\def\@Address{#1}}
\newcommand{\Plaintitle}[1]{\def\@Plaintitle{#1}}
\newcommand{\Shorttitle}[1]{\def\@Shorttitle{#1}}
\newcommand{\Plainauthor}[1]{\def\@Plainauthor{#1}}
\newcommand{\Volume}[1]{\def\@Volume{#1}}
\newcommand{\Year}[1]{\def\@Year{#1}}
\newcommand{\Month}[1]{\def\@Month{#1}}
\newcommand{\Issue}[1]{\def\@Issue{#1}}
\newcommand{\Submitdate}[1]{\def\@Submitdate{#1}}
%% for articles and code snippets
\newcommand{\Acceptdate}[1]{\def\@Acceptdate{#1}}
\newcommand{\Abstract}[1]{\def\@Abstract{#1}}
\newcommand{\Keywords}[1]{\def\@Keywords{#1}}
\newcommand{\Plainkeywords}[1]{\def\@Plainkeywords{#1}}
%% for internal use
\newcommand{\Seriesname}[1]{\def\@Seriesname{#1}}
\newcommand{\Hypersubject}[1]{\def\@Hypersubject{#1}}
\newcommand{\Hyperauthor}[1]{\def\@Hyperauthor{#1}}
\newcommand{\Footername}[1]{\def\@Footername{#1}}
\newcommand{\Firstdate}[1]{\def\@Firstdate{#1}}
\newcommand{\Seconddate}[1]{\def\@Seconddate{#1}}
\newcommand{\Reviewauthor}[1]{\def\@Reviewauthor{#1}}
%% defaults
\author{Firstname Lastname\\Affiliation}
\title{Title}
\Abstract{---!!!---an abstract is required---!!!---}
\Plainauthor{\@author}
\Volume{VV}
\Year{YYYY}
\Month{MMMMMM}
\Issue{II}
\Submitdate{yyyy-mm-dd}
\Acceptdate{yyyy-mm-dd}
\Address{
  Firstname Lastname\\
  Affiliation\\
  Address, Country\\
  E-mail: \email{name@address}\\
  URL: \url{http://link/to/webpage/}
}


  \Seriesname{Issue}
  \Hypersubject{Journal of Statistical Software}
  \Plaintitle{\@title}
  \Shorttitle{\@title}
  \Plainkeywords{\@Keywords}

  \Hyperauthor{\@Plainauthor}
  \Keywords{---!!!---at least one keyword is required---!!!---}
  \Footername{Affiliation}
  \Firstdate{\textit{Submitted:} \@Submitdate}
  \Seconddate{\textit{Accepted:} \@Acceptdate}

%% Sweave(-like)
\DefineVerbatimEnvironment{Sinput}{Verbatim}{fontshape=sl}
\DefineVerbatimEnvironment{Soutput}{Verbatim}{}
\DefineVerbatimEnvironment{Scode}{Verbatim}{fontshape=sl}
\newenvironment{Schunk}{}{}
\DefineVerbatimEnvironment{Code}{Verbatim}{}
\DefineVerbatimEnvironment{CodeInput}{Verbatim}{fontshape=sl}
\DefineVerbatimEnvironment{CodeOutput}{Verbatim}{}
\newenvironment{CodeChunk}{}{}
\setkeys{Gin}{width=0.8\textwidth}
%% new \maketitle
\def\@myoddhead{
%  {\color{white} JSS}\\[-1.42cm]
%  \hspace{-2em} \includegraphics[height=23mm,keepaspectratio]{jsslogo} \hfill
%  \parbox[b][23mm]{118mm}{\hrule height 3pt
%           \center{
%           {\fontfamily{pzc} \fontsize{28}{32} \selectfont Journal of Statistical Software}
%   \vfill
%           {\it \small \@Month{} \@Year, Volume~\@Volume, \@Seriesname~\@Issue.%
%    \hfill \href{http://www.jstatsoft.org/}{http://www.jstatsoft.org/}}}\\[0.1cm]
%     \hrule height 3pt}
}
  \def\maketitle{\@oddhead{\@myoddhead} \par
   \begingroup
     \def\thefootnote{\fnsymbol{footnote}}
     \def\@makefnmark{\hbox to 0pt{$^{\@thefnmark}$\hss}}
     \long\def\@makefntext##1{\parindent 1em\noindent
                              \hbox to1.8em{\hss $\m@th ^{\@thefnmark}$}##1}
     \@maketitle \@thanks
   \endgroup
   \setcounter{footnote}{0}
   \thispagestyle{empty}
   \markboth{\centerline{\@Shorttitle}}{\centerline{\@Hypersubject}}
   \pagestyle{empty}

   \let\maketitle\relax \let\@maketitle\relax
   \gdef\@thanks{}\gdef\@author{}\gdef\@title{}\let\thanks\relax
  }
  \def\@maketitle{\vbox{\hsize\textwidth \linewidth\hsize %\vskip 1in
   {\centering
   {\LARGE\bf \@title\par}
   \vskip 0.2in plus 1fil minus 0.1in
   {
       \def\and{\unskip\enspace{\rm and}\enspace}%
       \def\And{\end{tabular}\hss \egroup \hskip 1in plus 2fil
          \hbox to 0pt\bgroup\hss \begin{tabular}[t]{c}\large\bf\rule{\z@}{24pt}\ignorespaces}%
       \def\AND{\end{tabular}\hss\egroup \hfil\hfil\egroup
          \vskip 0.1in plus 1fil minus 0.05in
          \hbox to \linewidth\bgroup\rule{\z@}{10pt} \hfil\hfil
          \hbox to 0pt\bgroup\hss \begin{tabular}[t]{c}\large\bf\rule{\z@}{24pt}\ignorespaces}
       \hbox to \linewidth\bgroup\rule{\z@}{10pt} \hfil\hfil
       \hbox to 0pt\bgroup\hss \begin{tabular}[t]{c}\large\bf\rule{\z@}{24pt}\@author
       \end{tabular}\hss\egroup
   \hfil\hfil\egroup}
   }
   \vskip 0.3in minus 0.1in
  }}
%% sections, subsections, and subsubsections
\newlength{\preXLskip}
\newlength{\preLskip}
\newlength{\preMskip}
\newlength{\preSskip}
\newlength{\postMskip}
\newlength{\postSskip}
\setlength{\preXLskip}{1.8\baselineskip plus 0.5ex minus 0ex}
\setlength{\preLskip}{1.5\baselineskip plus 0.3ex minus 0ex}
\setlength{\preMskip}{1\baselineskip plus 0.2ex minus 0ex}
\setlength{\preSskip}{.8\baselineskip plus 0.2ex minus 0ex}
\setlength{\postMskip}{.5\baselineskip plus 0ex minus 0.1ex}
\setlength{\postSskip}{.3\baselineskip plus 0ex minus 0.1ex}

\newcommand{\jsssec}[2][default]{\vskip \preXLskip%
  \pdfbookmark[1]{#1}{Section.\thesection.#1}%
  \refstepcounter{section}%
  \centerline{\textbf{\thesection. #2}} \nopagebreak
  \vskip \postMskip}
\newcommand{\jsssecnn}[1]{\vskip \preXLskip%
  \centerline{\textbf{#1}} \nopagebreak 
  \vskip \postMskip}

\newcommand{\jsssubsec}[2][default]{\vskip \preMskip%
  \pdfbookmark[2]{#1}{Subsection.\thesubsection.#1}%
  \refstepcounter{subsection}%
  \textbf{\thesubsection. #2} \nopagebreak
  \vskip \postSskip}
\newcommand{\jsssubsecnn}[1]{\vskip \preMskip%
  \textbf{#1} \nopagebreak
  \vskip \postSskip}

\newcommand{\jsssubsubsec}[2][default]{\vskip \preSskip%
  \pdfbookmark[3]{#1}{Subsubsection.\thesubsubsection.#1}%
  \refstepcounter{subsubsection}%
  {\textit{#2}} \nopagebreak
  \vskip \postSskip}
\newcommand{\jsssubsubsecnn}[1]{\vskip \preSskip%
  {\textit{#1}} \nopagebreak
  \vskip \postSskip}

\newcommand{\jsssimplesec}[2][default]{\vskip \preLskip%
%%  \pdfbookmark[1]{#1}{Section.\thesection.#1}%
  \refstepcounter{section}%
  \textbf{#1} \nopagebreak
  \vskip \postSskip}
\newcommand{\jsssimplesecnn}[1]{\vskip \preLskip%
  \textbf{#1} \nopagebreak
  \vskip \postSskip}

  \renewcommand{\section}{\secdef \jsssec \jsssecnn}
  \renewcommand{\subsection}{\secdef \jsssubsec \jsssubsecnn}
  \renewcommand{\subsubsection}{\secdef \jsssubsubsec \jsssubsubsecnn}

%% footer
\newlength{\footerskip}
\setlength{\footerskip}{2.5\baselineskip plus 2ex minus 0.5ex}

\newcommand{\makefooter}{%
  \vspace{\footerskip}

 % \begin{samepage}
 % \textbf{\large \@Footername: \nopagebreak}\\[.3\baselineskip] \nopagebreak
%  \@Address \nopagebreak
%  \vfill
%  \hrule \nopagebreak
%  \vspace{.1\baselineskip}
%  {\fontfamily{pzc} \fontsize{13}{15} \selectfont Journal of Statistical Software}
%  \hfill
%  \url{http://www.jstatsoft.org/}\\ \nopagebreak
%  published by the American Statistical Association
%  \hfill
%  \url{http://www.amstat.org/}\\[.3\baselineskip] \nopagebreak
%  {Volume~\@Volume, \@Seriesname~\@Issue}
%  \hfill
%  \@Firstdate\\ \nopagebreak
%  {\@Month{} \@Year}
%  \hfill
%  \@Seconddate  \nopagebreak
%  \vspace{.3\baselineskip}
%  \hrule
 % \end{samepage}
}
%% colors
\definecolor{Red}{rgb}{0.7,0,0}
\definecolor{Blue}{rgb}{0,0,0.8}
  \hypersetup{%
    hyperindex = {true},
    colorlinks = {true},
    linktocpage = {true},
    plainpages = {false},
    linkcolor = {Blue},
    citecolor = {Blue},
    urlcolor = {Red},
    pdfstartview = {Fit},
    pdfpagemode = {UseOutlines},
    pdfview = {XYZ null null null}
  }
\AtBeginDocument{
  \hypersetup{%
    pdfauthor = {\@Hyperauthor},
    pdftitle = {\@Plaintitle},
    pdfsubject = {\@Hypersubject},
    pdfkeywords = {\@Plainkeywords}
  }
}
\AtBeginDocument{\maketitle}
\AtEndDocument{\makefooter}
%% commands
\makeatletter
\newcommand\code{\bgroup\@makeother\_\@makeother\~\@makeother\$\@codex}
\def\@codex#1{{\normalfont\ttfamily\hyphenchar\font=-1 #1}\egroup}
\makeatother
%%\let\code=\texttt
\let\proglang=\textsf
\newcommand{\pkg}[1]{{\normalfont\fontseries{b}\selectfont #1}}
\newcommand{\email}[1]{\href{mailto:#1}{\normalfont\texttt{#1}}}
\newcommand{\doi}[1]{\href{http://dx.doi.org/#1}{\normalfont\texttt{doi:#1}}}
\newcommand{\E}{\mathsf{E}}
\newcommand{\VAR}{\mathsf{VAR}}
\newcommand{\COV}{\mathsf{COV}}
\newcommand{\Prob}{\mathsf{P}}
\endinput
%%
%% End of file `asa.cls'.