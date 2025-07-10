\name{sboptions}
\alias{sboptions}
\title{Options for Multiscale Bootstrap}
\description{
  To set and examine global options for \code{scaleboot} package.
}
\usage{
sboptions(x, value)
}
\arguments{
  \item{x}{character of an option name.}
  \item{value}{When specified, this value is set.}
}
\details{
  Invoking \code{sboptions()} with no arguments returns a list with the
  current values of the options. Otherwise it returns option(s) with name(s)
  specified by \code{x}.  When \code{value} is specified, it is
  set to the option named \code{x}.
}
\author{Hidetoshi Shimodaira}
\examples{
sboptions() # show all the options
sboptions("models") # show the default model names
new.models <- sbmodelnames(m=1:2) # character vector c("poly.1","poly.2")
old.models <- sboptions("models",new.models) # set the new model names
sboptions("models") # show the default model names
sboptions("models",old.models) # set back the default value
sboptions("models") # show the default model names
}
\keyword{environment}
\keyword{ models }
