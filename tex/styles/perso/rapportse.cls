\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{rapportse}[2016/09/01 Classe pour les rapports de Telecom Saint-Etienne]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\ProcessOptions\relax
\LoadClass{report}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		Required Packages	   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%%  Fonts  %%%
%%%%%%%%%%%%%%%
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}
\RequirePackage[mathbf,mathcal,text-hat-accent]{euler}
\RequirePackage{fontspec}
\RequirePackage{microtype}

%%%%%%%%%%%%%%%
%  Languages  %
%%%%%%%%%%%%%%%
\RequirePackage[french]{babel}

%%%%%%%%%%%%%%%
%%  Margins  %%
%%%%%%%%%%%%%%%
\RequirePackage[a4paper,twoside,
				left=2.5cm,
				right=2.5cm,
				top=2.5cm,
				bottom=3cm,
				ignorefoot]{geometry}

%%%%%%%%%%%%%%%
%	 Color	  %
%%%%%%%%%%%%%%%
\RequirePackage{color}

%%%%%%%%%%%%%%%
%	Floats	  %
%%%%%%%%%%%%%%%
\RequirePackage{graphicx}
\graphicspath{{./Images/},{./images},{./img/}}
\RequirePackage{tabularx}
\RequirePackage{float}

%%%%%%%%%%%%%%%
%  Headlines  %
%%%%%%%%%%%%%%%
\RequirePackage{fancyhdr}
\pagestyle{fancy}
\fancyhead[R]{\includegraphics[height = 1cm]{LogoIMT.png} \quad
\includegraphics[height = 1cm]{LogoUJM}}
\fancyhead[L]{\includegraphics[height = 1cm]{LogoTelecomQuadri}}
\fancyhead[C]{\@title}
\fancyfoot[RO,LE]{\bfseries \thepage}
\fancyfoot[C]{\@author\\ \@date}

%%%%%%%%%%%%%
% Frontpage %
%%%%%%%%%%%%%
\RequirePackage[absolute]{textpos}
\definecolor{PetrolBlue}{RGB}{1,95,147}
\definecolor{CloudBlue}{RGB}{178,206,230}
\definecolor{SteelBlue}{RGB}{36,130,192}

\global\let\@logo\@empty
\global\let\logo\relax
\newcommand*{\subtitle}[1]

\renewcommand{\maketitle}{
	\begin{titlepage}
		\thispagestyle{empty}
		\begin{textblock*}{.5\paperwidth}(.5\paperwidth,0cm)
			\textblockcolor{CloudBlue}
			\parindent=3mm % this is used to set the width of the vertical blue band (3mm in this case)
			{\color{PetrolBlue}\vrule height \paperheight width \dimexpr.5\paperwidth-\parindent\relax}
		\end{textblock*}
		\begin{textblock*}{\paperwidth}(0cm,.4\paperwidth)
			\textblockcolor{SteelBlue}\vskip2mm
			\centering
			\color{white}\fontsize{1.5cm}{2.5cm}\selectfont \@title \par \vskip2mm
		\end{textblock*}
		\begin{textblock*}{.3\paperwidth}(.5cm,.5cm)
			\parindent=0mm%
			\includegraphics[width=.3\paperwidth,height=.35\paperwidth]{LogoTelecom}
		\end{textblock*}
		\begin{textblock*}{.12\paperwidth}(.35\paperwidth,.7\paperheight)
			\parindent=0mm%
			\textblockcolor{white}
			\includegraphics[width=.12\paperwidth]{LogoIMT.png}\par\bigskip
			\includegraphics[width=.12\paperwidth]{LogoUJM}\par\bigskip
			%\includegraphics[width=.12\paperwidth,height=.15\paperwidth]{\logo} %Trouver le veritable troisi\`eme logo
		\end{textblock*}
		\begin{textblock*}{.4\paperwidth}(.6\paperwidth,1cm)
			\textblockcolor{PetrolBlue}
			\raggedleft\color{white}\fontsize{1cm}{1.2cm}\selectfont
			\@date \qquad\null
		\end{textblock*}
		\begin{textblock*}{.2\paperwidth}(.6\paperwidth,.6\paperheight)
			\textblockcolor{PetrolBlue}
			\color{white}\fontsize{0.5cm}{0.6cm}\selectfont
			\noindent
			\begin{tabular}{m{.2\paperwidth}}
				\@author
			\end{tabular}
		\end{textblock*}
		\begin{textblock*}{.4\paperwidth}(.6\paperwidth,.8\paperheight)
			\parindent=0mm%
			\textblockcolor{PetrolBlue}
			\color{white}\fontsize{1.2cm}{1.4cm}\selectfont
		\end{textblock*}
		\newpage
		\vfil\null
	\end{titlepage}
}

%%%%%%%%%%%%%%%
%  Hyperlinks %
%%%%%%%%%%%%%%%
\RequirePackage[unicode]{hyperref}
\RequirePackage{cleveref}

%%%%%%%%%%%%%%%
%   Sections  %
%%%%%%%%%%%%%%%
\renewcommand{\thesection}{\arabic{section}} % Numbering for sections}
\renewcommand{\theparagraph}{\alph{paragraph})} % Adding numbering for paragraphs


%%%%%%%%%%%%%%
% Workaround %
%%%%%%%%%%%%%%

\setlength{\headheight}{1.5cm} % Headlines height for logos
