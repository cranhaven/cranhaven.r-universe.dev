\name{get.edhw}
\alias{get.edhw}
\title{
Wrapper to get data from the Epigraphic Database Heidelberg API
}
\description{
A wrapper function to obtain data from the Epigraphic Database Heidelberg REST like API repository.
}
\usage{
get.edhw(file = NULL, hd_nr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
	JSON file with EDH data (optional)
}
  \item{hd_nr}{
	HD number of inscriptions
}
  \item{\dots}{
	additional arguments
}
}
\details{
This wrapper function aims to obtain sample data from the Epigraphic Database Heidelberg API repository 
by their HD numbers or a file with a valid format JSON can be specified in \code{file}. 

In any case, the JSON output becomes a list object with the \code{\link[rjson]{rjson}} 
package.
}
\value{
A list of lists object with the items described in \code{\link{get.edh}}.

}
\references{
https://edh.ub.uni-heidelberg.de/data/api
}
\author{
Antonio Rivero Ostoic
}
\note{
Large samples can take a lot of time.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.edh}}, \code{\link{simil}}, \code{\link[rjson]{rjson}}
}
\examples{
\dontrun{
# get 10 records from EDH API data
get.edhw(hd_nr=1:10)}
}
\keyword{ IO }
\keyword{ utilities }
