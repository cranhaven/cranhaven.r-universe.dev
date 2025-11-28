\name{Ls}

\alias{Ls}

\title{Ls}

\description{Advanced object browser}

\usage{Ls (pos = 1, pattern, mode = "any", type = "any", exclude = "function", sort = "name")}

\arguments{
\item{mode}{which object mode to include, "any" to include all}
\item{type}{which object type to include ("type" is typically, but not always an object's class attribute), "any" to include all}
\item{exclude}{exclude functions (default), "none" to include all}
\item{sort}{sort by name (default), "size" to sort by size}
\item{pos}{specify environment, passed to ls()}
\item{pattern}{optional regular expression, passed to ls()}
}

\details{Based on 'ls()' but outputs data frame.}

\value{Data frame with object features columns.}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{ls}}}

\examples{
data(trees)
Ls()
}


\keyword{environment}
