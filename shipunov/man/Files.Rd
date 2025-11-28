\name{Files}
\alias{Files}
\title{Textual file system browser}

\description{Textual file system browser}

\usage{Files(root=getwd(), multiple=FALSE, hidden=FALSE)}

\arguments{

\item{root}{Root directory to explore, default is the working directory}

\item{multiple}{Allows multiple files to be selected}

\item{hidden}{Show hidden files?}

}

\details{

Interactive text-based file chooser dialog, modified from code published
by "mathematical.coffee" on Stack Overflow as "R command-line file
dialog".

If 'multiple=TRUE', one can select files one by one (they will
"disappear" from the displayed list), and typing "0" will output this
list. If "multiple=FALSE", typing "0" will output the name of the current
directory.

Files() uses normalizePath() so symbolic links will be resolved. Also,
Files() is not very useful when number of files in the directory is
large.

Alternatives for Linux: 'tcltk::tk_choose.files()' and
'tcltk::tk_choose.dir()'

}

\value{

Returns character vector of selected files, or directory name (useful for
'setwd()'), or new user-defined file name with full path.

}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{setwd}}, \code{\link{getwd}}, \code{\link{dir}}}

\examples{
\dontrun{
## interactive commands
setwd <- Files() # then select directory to work in
Files("~", hidden=TRUE) # explore home directory with hidden files (Linux, macOS)
}
}

\keyword{utilities}
