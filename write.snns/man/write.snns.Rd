\name{write.snns}
\alias{write.snns}
\title{
Interfacing SNNS pattern files.
}
\description{
Creates a pattern file from a matrix or data frame, suitable for training, validating or testing \cr
a neural network with the Stuttgart Neural Network Simulator (SNNS).
}
\usage{
write.snns(x, file="", noutputs=1)}

\arguments{
\item{x}{Matrix or data.frame containing data to export.}
\item{file}{Name of the pattern file to create. If not specified standard output is used as default.}
\item{noutputs}{Number of output units of the neural network the pattern will work with.}
}

\details{
This function generates a pattern file from a data.frame or matrix.
It doesn't cover subpatterns, variable number of dimensions or classes yet.
It goes through the data entity writing its contents into the specified file.
Each row is interpreted as a data vector. The first components are the input variables. The output units are specified in the noutputs last components.

Neural networks have demonstrated its ability in process modelling, data clustering and classification.
It can be useful to compare the results obtained with regression models with those obtained by other AI techniques such as Neural Networks or Evolutionary Computation. The purpose of this routine is to make the data transfer from R to SNNS as immediate as possible.
}

\value{
The function returns no value.
}
\author{
Manuel Castejon Limas. \email{manuel.castejon@unileon.es}\cr
Joaquin Ordieres Mere. \email{joaquin.ordieres@dim.unirioja.es}\cr
Francisco Javier de Cos Juez. \email{decos@api.uniovi.es}\cr
Francisco Javier Martinez de Pison. \email{fjmartin@dim.unirioja.es}
}

\references{
SNNS - Stuttgart Neural Network Simulator V4.1 Manual \url{http://www-ra.informatik.uni-tuebingen.de/SNNS/}
}
\examples{
x <- data.frame(x=c(1.4, 3.6, 4.7, 5.3), y=c(1.7, 3.5, 2.5, 9.4))
x
write.snns(x)
}
\keyword{file}

