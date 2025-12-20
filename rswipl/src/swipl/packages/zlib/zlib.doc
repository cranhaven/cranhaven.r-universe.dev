\documentclass[11pt]{article}
\usepackage{times}
\usepackage{pl}
\usepackage{html}
\sloppy
\makeindex

\onefile
\htmloutput{.}					% Output directory
\htmlmainfile{zlib}				% Main document file
\bodycolor{white}				% Page colour

\begin{document}

\title{SWI-Prolog binding to zlib}
\author{Jan Wielemaker \\
	University of Amsterdam \\
	VU University Amsterdam \\
	The Netherlands \\
	E-mail: \email{J.Wielemaker@vu.nl}}

\maketitle

\begin{abstract}
The library \pllib{zlib} provides a binding to the
\href{http://www.zlib.net/}{zlib} general purpose compression library.
The prolog library aims as seamlessly reading and writing files
compatible to the \program{gzip} program as well as compressed (network)
communication.
\end{abstract}

\pagebreak
\tableofcontents

\vfill
\vfill

\newpage

\section{Zlib and compression}
\label{sec:zlib}

Zlib is a widespread library implementing the RFC1950 (zlib wrapper),
RFC1951 (deflate stream) and RFC1952 (gzip wrapper) compression
standards. The SWI-Prolog binding is a foreign library that creates a
compressed stream as a wrapper around a normal stream. Implemented this
way, it can perform a wide variety of tasks:

\begin{itemize}
    \item Read/write gzip compatible files
    \item Setup standard compressed stream communication
    \item Realise in-memory compression or decompression
    \item Deal with streams holding embedded compressed objects
\end{itemize}

The core predicate of the library is zopen/3. The remainder of the
functionality of \pllib{zlib} is defined in Prolog and can be used as a
starting point for other high-level primitives. See also \file{ztest.pl}
providing test and demo code. This file is part of the source
distribution.

Part of the functionality of this library can also be realised using
the pipe interface and the \program{gzip} program.  For example, a
gziped file can also be opened in Prolog using the code below.

\begin{code}
	...
	open(pipe('gunzip < file.gz'), read, In),
	...
\end{code}

The advantage of this library over using an external program for such
tasks is enhanced platform independence and reduced time to open a file.
Platform independence is improved as we do not have to worry about
availability of the \program{gunzip} utility and we do not have to worry
about shell and filename quoting issues. While the above replacement
code works well on most modern Unix systems, it only works with special
precautions on Windows.\footnote{Install gunzip, deal with Windows
path-names, the windows shell and quoting.}

The library becomes unavoidable if we consider compressed network
communication. Here we get the stream from tcp_open_socket/3. The
library provides efficient creation of a compressed stream, as well as
support for flushing output through the standard Prolog flush_output/1
call.


\section{Predicate reference}
\label{sec:zlib-predicates}

\begin{description}
    \predicate{zopen}{3}{+Stream, -ZStream, +Options}
Creates \arg{ZStream}, providing compressed access to \arg{Stream}. If
an input stream is wrapped, it recognises a gzip or deflate header. If
an output stream is wrapped, \arg{Options} define the desired wrapper
and compression level. The new \arg{ZStream} inherits its
\jargon{encoding} from \arg{Stream}. In other words, if \arg{Stream} is
a text-stream, so is \arg{ZStream}. The original \arg{Stream} is
switched to binary mode while it is wrapped. The original encoding of
\arg{Stream} is restored if \arg{ZStream} is closed.  Note that zopen/3
does not actually process any data and therefore succeeds on
input streams that do not contain valid data.  Errors may be generated
by read operations performed on the stream.

Defined options on output streams are:

    \begin{description}
	\termitem{format}{+Format}
Either \const{deflate} (default), \const{raw_deflate} or \const{gzip}.
The \const{deflate} envelope is simple and short and is typically used
for compressed (network) communication. The \const{raw_deflate} does not
include an envelope and is often used as a step in crypographic
encodings. The \const{gzip} envelope is compatible to the \program{gzip}
program and intended to read/write compressed files.

	\termitem{level}{+Level}
Number between 0 and 9, specifying the compression level, Higher levels
use more resources. Default is 6, generally believed to be a good
compromise between speed, memory requirement and compression.

	\termitem{multi_part}{+Boolean}
If \const{true}, restart reading if the input is not at end-of-file. The
default is \const{true} for gzip streams.
    \end{description}

Generic options are:

    \begin{description}
	\termitem{close_parent}{Bool}
If \const{true} (default), closing the compressed stream also closes
(and thus invalidates) the wrapped stream. If \const{false}, the wrapped
stream is \emph{not} closed. This can be used to read/write a compressed
data block as partial input/output on a stream.
    \end{description}

    \predicate{gzopen}{3}{+File, +Mode, -Stream}
Same as \term{gzopen}{File, Mode, Stream, []}.

    \predicate{gzopen}{4}{+File, +Mode, -Stream, +Options}
Open \program{gzip} compatible \arg{File} for reading or writing.
If a file is opened in =append= mode,  a   new  gzip image will be
added to the end of the file.   The gzip standard defines that a
file can hold multiple  gzip  images   and  inflating  the  file
results in a concatenated stream of all inflated images.
Options are passed to open/4  and   zopen/3.  Default  format is
\const{gzip}.
\end{description}

\section{Interaction with Prolog stream predicates}
\label{sec:zlib-streams}

Using flush_output/1 on a compressed stream causes a
\const{Z_SYNC_FLUSH} on the stream.  Using close/1 on a compressed
stream causes a \const{Z_FINISH} on the stream.  If the stream uses
the \const{gzip} format, a \program{gzip} compatible footer is
written to the stream.  If \const{close_parent} is set (default)
the underlying stream is closed too.  Otherwise it remains open
and the user can continue communication in non-compressed format
or reopen the stream for compression using zopen/3.

\printindex

\end{document}

