\name{Tobin}
\alias{Tobin}

\title{Binarize (make dummy variables)}
\description{Converts vector into matrix with binary columns}

\usage{Tobin(var, convert.names=TRUE)}

\arguments{
\item{var}{character or numerical variable}
\item{convert.names}{if TRUE (default), construct new variable names, otherwise, use unique variable values as variable names}
}

\details{'Tobin()' transforms character or numeric vector into the matrix with 0/1 (absent/present) cells.

Two approaches are in use: through '==' operation and through the conversion into factor.

First approach also constructs new names of variables whereas the second ('convert.names=FALSE') makes variable names from names of factor levels (i.e., labels).

Alternatives: "*dumm*" packages (there are few in CRAN).
}

\value{Matrix with binary columns}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
(ee <- sample(letters[1:5], 10, replace=TRUE))
Tobin(ee, conv=FALSE)
Tobin(ee, conv=TRUE)
}

\keyword{manip}
