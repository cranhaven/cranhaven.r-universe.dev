\name{Xpager}
\alias{Xpager}
\title{Separate terminal pager for Linux}
\description{Separate terminal pager for Linux X11 (uses some terminal and 'less')}

\usage{Xpager(pager="xterm")}

\arguments{
\item{pager}{name of the terminal application to use, or "old" for the default}
}

\details{Linux pager in the new terminal window. 'xterm' is default,
there is also setting for 'mate-terminal';
'konsole' (KDE terminal) and 'gnome-terminal' are easy to add.

Run Xpager("old") to restore default behavior.

BTW, for some reason, 'editor()' does not work this way.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
\dontrun{
## Linux- (and X11-) specific
Xpager()
?help
Xpager("old")
?help
}
}

\keyword{utilities}
