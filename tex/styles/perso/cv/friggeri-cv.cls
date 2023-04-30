%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\ProvidesClass{friggeri-cv}[2012/04/30 CV class]
\NeedsTeXFormat{LaTeX2e}

\DeclareOption{print}{\def\@cv@print{}}
\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{article}%
}
\ProcessOptions\relax
\LoadClass{article}


%%%%%%%%%%
% Colors %
%%%%%%%%%%

\RequirePackage{xcolor}

\definecolor{white}{RGB}{255,255,255}
\definecolor{darkgray}{HTML}{333333}
\definecolor{gray}{HTML}{4D4D4D}
\definecolor{lightgray}{HTML}{999999}

\definecolor{pblue}{HTML}{0395DE}


\ifdefined\@cv@print
  \colorlet{green}{gray}
  \colorlet{orange}{gray}
  \colorlet{purple}{gray}
  \colorlet{red}{gray}
  \colorlet{blue}{gray}
  \colorlet{fillheader}{white}
  \colorlet{header}{gray}
\else
  \colorlet{fillheader}{white}
  \colorlet{header}{gray}
  \colorlet{textcolor}{gray}
  \colorlet{headercolor}{pblue}
\fi


%%%%%%%%%
% Fonts %
%%%%%%%%%

%-------------------------------------------------------------------------------
%                Configuration for directory locations
%-------------------------------------------------------------------------------
% Configure a directory location for fonts(default: 'fonts/')
\newcommand*{\fontdir}[1][fonts/]{\def\@fontdir{#1}}
\fontdir

\RequirePackage[quiet]{fontspec}
\RequirePackage[math-style=TeX]{unicode-math}
\newfontfamily\bodyfont[
  Path=\@fontdir,
]{HelveticaNeue-Medium.otf}
\newfontfamily\thinfont[  Path=\@fontdir,
]{helvetica-lighcond.ttf}
\newfontfamily\headingfont[  Path=\@fontdir,
]{HelveticaNeueCondensedBold.ttf}
\defaultfontfeatures{Mapping=tex-text}
\setmainfont[  Path=\@fontdir,
Mapping=tex-text, Color=textcolor]{HelveticaNeue-Light.otf}

%%%%%%%%%%
% Header %
%%%%%%%%%%
\RequirePackage{tikz}

\newcommand{\rolefont}{%
  \fontsize{14pt}{24pt}\selectfont%
  \thinfont%
  \color{white}%
}

% TODO: There is a better way of placing the header flush right
\newcommand{\header}[1]{%
  \begin{tikzpicture}[remember picture,overlay]
    \node [rectangle, fill=fillheader, minimum width=\paperwidth, minimum height=2.5cm] (box) at (current page.north){};
    \node [below = 15mm, right = 0mm] (name) at (box) {%
    \parbox[t]{10cm}{
      \fontsize{22pt}{50pt}\color{headercolor}%
      {\thinfont #1}
    }
    };
    \node [below = 15mm, right = 2mm] (name) at (current page.north west){%
    \parbox[t]{7.5cm}{
      \fontsize{14pt}{18pt}\color{headercolor}%
      {\thinfont Claude-Alban \\ \textsc{RANÉLY-VERGÉ-~DÉPRÉ}}
    }
    };
  \end{tikzpicture}
  \vspace{2cm}
}

%%%%%%%%%%%%%%%%%%%%
%  Profile picture %
%%%%%%%%%%%%%%%%%%%%

\newcommand{\imsize}{\linewidth}
\newlength\imagewidth
\newlength\imagescale
\renewcommand{\imsize}{0.618\linewidth}
\pgfmathsetlength{\imagewidth}{4cm} %
\pgfmathsetlength{\imagescale}{\imagewidth/600} %
\newcommand{\profilepic}[1]{\renewcommand{\givenprofilepic}{#1}}
\newcommand{\givenprofilepic}{}

\renewcommand{\profilepic}{
    \begin{tikzpicture}[x=\imagescale,y=-\imagescale]
    \clip (600/2, 567/2) circle (567/2);
   % \node[anchor=north west, inner sep=0pt, outer sep=0pt] at (0,0) {\includegraphics[width=\imagewidth]{img/photoCV}};
   \node[anchor=north west, inner sep=0pt, outer sep=0pt, fill=white] at (0,0) {};
\end{tikzpicture}   
}



%%%%%%%%%%%%%
% Structure %
%%%%%%%%%%%%%
\RequirePackage{parskip}

\renewcommand{\section}[1]{
  \par\vspace{\parskip}
  % TODO: Define a macro to abstarct this to avoid color leakage
  {\LARGE\headingfont\color{headercolor} #1} % The backets surrounding here are there to "contain" the color
  \par
}

\renewcommand{\subsection}[2]{
  \par\vspace{.5\parskip}
  {\Large\headingfont\color{headercolor} #2}
  \par\vspace{.25\parskip}
}

\pagestyle{empty}


%%%%%%%%%%%%%%%%%%%%
% List environment %
%%%%%%%%%%%%%%%%%%%%

\setlength{\tabcolsep}{0pt}
\newlength{\sepDateTitre}
\setlength{\sepDateTitre}{6mm minus 5mm}
\newenvironment{entrylist}{\begin{tabular*}{\textwidth}{@{\extracolsep{\sepDateTitre}}ll}}
                          {\end{tabular*}}
\renewcommand{\bfseries}{\headingfont\color{headercolor}}
\newlength{\infolength}
\setlength{\infolength}{95mm plus 10 mm minus 10mm}
\newcommand{\entry}[4]{
  \small{#1} & \parbox[t]{\infolength}{
    \textbf{#2}
    \hfill
    {\addfontfeature{Color=pblue} #3}\\
    #4
    \vspace{\parsep}
  }\\
}

\newenvironment{formations}{\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}ll}}
                          {\end{tabular*}}
\newlength{\formationlength}
\setlength{\formationlength}{60mm minus 10mm}
\newcommand{\formation}[3]{
  \small{#1} & \parbox[t]{\formationlength}{
    \textbf{#2}
    \\
    #3
    \vspace{\parsep}
  }\\}
%%%%%%%%%%%%%%
% Side blocks %
%%%%%%%%%%%%%%
\RequirePackage{textpos}
\AtBeginDocument{
    \setlength{\TPHorizModule}{10mm}
    \setlength{\TPVertModule}{10mm}
}
\newenvironment{side}[2]{
  \begin{textblock}{#2}(#1,0)
}{
  \end{textblock}
}

%%%%%%%%%%%%%%%%
% Other tweaks %
%%%%%%%%%%%%%%%%

\RequirePackage[left=4mm,
                top=4mm,
                right=4mm,
                bottom=4mm,
                nohead,nofoot]{geometry}
\RequirePackage{hyperref}
