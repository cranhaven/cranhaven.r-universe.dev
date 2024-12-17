\name{arguments}
\alias{arguments}
\title{Arguments of a Function}
\usage{
arguments(fun, width = options("width")$width)
}
\arguments{
\item{fun}{name of the function.}

\item{width}{optional width for line breaking.}
}
\value{
None.
}
\description{
Shows the arguments and their default values of a function.
}
\examples{
arguments(repeat.sample)

}
\seealso{
\code{\link[base]{args}}.
}
\concept{arguments}
\concept{default value}
\concept{function}
