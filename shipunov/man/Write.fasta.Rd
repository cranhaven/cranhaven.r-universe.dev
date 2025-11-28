\name{Write.fasta}
\alias{Write.fasta}
\title{Write 'FASTA' files}
\description{Simple writing of 'FASTA' files}

\usage{Write.fasta(df, file)}

\arguments{
\item{df}{Name of data frame}
\item{file}{File name}
}

\details{Simple writing of 'FASTA' files. If the data frame has more then two columns,
only two first columns will be used (with warning).}

\value{'FASTA' file on the disk.}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
ff <- data.frame(one="some_id", two="ATGC", three="something else")
Write.fasta(ff, file=file.path(tempdir(), "tmp.fasta")) # warning will be produced
file.show(file=file.path(tempdir(), "tmp.fasta")) # interactive
}

\keyword{manip}
