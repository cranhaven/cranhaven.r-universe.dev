\documentclass[11pt]{article}
\usepackage{times}
\usepackage{pl}
\usepackage{html}
\sloppy
\makeindex

\onefile
\htmloutput{.}					% Output directory
\htmlmainfile{archive}				% Main document file
\bodycolor{white}				% Page colour

\begin{document}

\title{SWI-Prolog binding to libarchive}
\author{Jan Wielemaker \\
	VU University Amsterdam \\
	The Netherlands \\
	E-mail: \email{J.Wielemaker@vu.nl}}

\maketitle

\begin{abstract}
The library \href{https://github.com/libarchive/libarchive}{libarchive}
provides a portable way to access archive files as well as encoded
(typically compressed) data. This package is a Prolog wrapper around
this library. The motivation to introduce this library is twofold. In
the first place, it provides a minimal platform independent API to
access archives. In the second place, it allows accessing archives
through Prolog streams, which often eliminates the need for temporary
files and all related consequences for performance, security and
platform dependency.
\end{abstract}

\pagebreak
\tableofcontents

\vfill
\vfill

\newpage

\section{Motivation}
\label{sec:archive-motivation}

Archives play two roles: they combine multiple documents into a single
one and they typically provide compression and sometimes encryption or
other services. Bundling multiple resources into a single archive may
greatly simplify distribution and guarantee that the individual
resources are consistent. SWI-Prolog provides archiving using its
(rather arcane) saved-state format. See resource/3 and open_resource/3.
It also provides compression by means of library(zlib).

External archives may be accessed through the process interface provided
by process_create/3, but this has disadvantages. The one that motivated
this library was that using external processes provide no decent
platform independent access to archives. Most likely zip files come
closest to platform independent access, but there are many different
programs for accessing zip files that provide slightly different sets of
options and the existence of any of these programs cannot be guaranteed
without distributing our own bundled version. Similar arguments hold for
Unix tar archives, where just about any Unix-derives system has a tar
program but except for very basic commands, the command line options
are not compatible and tar is not part of Windows. The only format
granted on Windows is .cab, but a program to create them is not part of
Windows and the .cab format is rare outside the Windows context.

Discarding availability of archive programs, each archive program comes
with its own set of command line options and its own features and
limitations. Fortunately,
\href{https://github.com/libarchive/libarchive}{libarchive} provides a
consistent interface to a wealth of compression and archiving formats.
The library \pllib{archive} wraps this library, providing access to
archives using Prolog streams both for the archive as a whole and the
archive entries. E.g., archives may be read from Prolog streams and each
member in turn may be processed using Prolog streams without
materialising data using temporary files.

\input{archive4pl.tex}

\section{Status}
\label{sec:archive-status}

The current version is merely a proof-of-concept. It lacks writing
archives and does not support many of the options of the underlying
library. The main motivation for starting this library was to achieve
portability of the upcomming SWI-Prolog package distribution system.
Other functionality will be added on `as needed' basis.

\printindex

\end{document}

