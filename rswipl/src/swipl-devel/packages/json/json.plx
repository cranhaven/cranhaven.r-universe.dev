\documentclass[11pt]{article}
\usepackage{times}
\usepackage{pl}
\usepackage{html}
\sloppy
\makeindex

\onefile
\htmloutput{.}					% Output directory
\htmlmainfile{json}				% Main document file
\bodycolor{white}				% Page colour

\begin{document}

\title{SWI-Prolog JSON library}
\author{Jan Wielemaker \\
	SWI-Prolog Solutions b.v. \\
	The Netherlands \\
	E-mail: \email{jan@swi-prolog.org}}

\maketitle

\begin{abstract}
This package reads and writes JSON documents from and to SWI-Prolog
streams, files and strings.
\end{abstract}

\pagebreak
\tableofcontents

\vfill
\vfill

\newpage

\input{jsonmd.tex}

\printindex

\end{document}

