\name{dts}
\alias{dts}
\title{
Converting dates into a numerical format
}
\description{
A function for converting different types of dates into a numerical format.
}
\usage{
dts(x, cent, sep, last)
}
\arguments{
  \item{x}{
  scalar, vector or list with dates to format
}
  \item{cent}{
  use centuries? (optional and logical)
}
  \item{sep}{
  separator, default "\code{ to }" (only for \code{cent})
}
  \item{last}{
  take last input value in \code{x}? (optional and logical)
}
}
\details{
When dating data has a character format like when involving AD, BC, BCE, C.E., etc., or even centuries like Cent., 
it is many times convenient to convert these type of dating data into a numerical format for a further computation.

In case that the input data has two or more plausible dates, then the outcome by default takes the first value of the input; 
otherwise the last date with option \code{last}.

\code{\link{dts}} also accepts dates involving centuries with the \code{cent} option, and in this case, 
it is possible to specify a separator of the century endpoints in \code{sep} or use "\code{ to }" as the default separator.

For dates having character format, then hyphens are regarded as separators of the plausible dates in \code{x}.
}
\value{
Dating data with a numerical format.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
Dating data with unknown year notations produce \code{NA} as output value.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.dates}}, \code{\link{prex}}.
}
\examples{
# negative first value
dts("58 BC - 30 AD")

# positive second value
dts("58 BC - 30 AD", last=TRUE)

# use century notation
dts("15th Cent. AD", cent=TRUE)
}
\keyword{ manip }
\keyword{ utilities }
