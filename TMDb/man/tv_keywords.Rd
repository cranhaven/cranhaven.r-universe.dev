\name{tv_keywords}
\alias{tv_keywords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the keywords associated with a TV show.
}
\description{
Get the plot keywords for a specific TV show id.
}
\usage{
tv_keywords(api_key, id, append_to_response = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The TV series ID.
}
  \item{append_to_response}{
Comma separated, any TV series method.
}
}
\value{
A list with the following fields:
\item{id}{The TV show ID.}
\item{results}{The keywords associated to the TV show.}
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

tv_keywords(api_key = api_key, id = 1396)
}   
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_keywords}
