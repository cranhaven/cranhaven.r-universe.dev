\name{timezone}
\alias{timezone}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the supported timezones.
}
\description{
Get the list of supported timezones for the API methods that support them.
}
\usage{
timezone(api_key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
}

\value{
A data.frame with the supported timezones.
}
\references{
https://developers.themoviedb.org/3/getting-started
}
\author{
Andrea Capozio
}

\examples{
\dontrun{
## An example of an authenticated request,
## where api_key is fictitious.
## You can obtain your own at https://www.themoviedb.org/documentation/api

api_key <- "key"

timezone(api_key = api_key)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{timezone}
