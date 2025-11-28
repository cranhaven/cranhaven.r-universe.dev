\name{Read.fasta}
\alias{Read.fasta}
\title{Read 'FASTA' files}
\description{Simple reading of 'FASTA' files}

\usage{Read.fasta(file)}

\arguments{
\item{file}{File name}
}

\details{Simple reading of 'FASTA' files.}

\value{Data frame with two columns: 'name' and 'sequence'.}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
write(file=file.path(tempdir(), "tmp.fasta"), ">some_id\nATGC")
Read.fasta(file=file.path(tempdir(), "tmp.fasta"))
}

\keyword{manip}

