\name{\%-\%}
\alias{\%-\%}
\title{Minus names}
\description{Subtract names from names}

\usage{x \%-\% y}

\arguments{
\item{x}{Character vector (likely named) to subtract from}
\item{y}{Subtracting character vector}
}

\details{
Instead of 'x', the function uses 'names(x)'. If 'x' has no names, they will be assigned from values.
}

\value{Character vector}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
str(iris[, iris \%-\% "Species"])
str(iris[, !names(iris) \%in\% "Species"]) # this is how to make it without \%-\%
c("apples", "bananas") \%-\% "apples" # simple character string also works
}

\keyword{manip}
