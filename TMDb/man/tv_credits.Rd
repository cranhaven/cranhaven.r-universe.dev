\name{tv_credits}
\alias{tv_credits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve credits for a TV series.
}
\description{
Get the cast & crew information about a TV series. Just like the website, TMDb pulls this information from the last season of the series.
}
\usage{
tv_credits(api_key, id, append_to_response = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The TV show ID.
}
  \item{append_to_response}{
Comma separated, any TV series method.
}
}
\value{
A list with the following fields:
\item{id}{The TV show ID.}
\item{cast}{The cast of the TV show.}
\item{crew}{The crew of the TV show.}
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

tv_credits(api_key = api_key, id = 1396)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_credits}
