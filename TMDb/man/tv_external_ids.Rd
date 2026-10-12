\name{tv_external_ids}
\alias{tv_external_ids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve external ids for a TV series.
}
\description{
Get the external ids stored in TMDb for a TV series.
}
\usage{
tv_external_ids(api_key, id, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The tv show ID.
}
  \item{language}{
A ISO 639-1 code.
}
}
\value{
A list with the following fields:
\item{imdb_id}{The TV show imdb ID.}
\item{freebase_mid}{The TV show freebase_mid ID.}
\item{freebase_id}{The TV show freebsae ID.}
\item{tvdb_id}{The TV show tvdb ID.}
\item{tvrage_id}{The TV show tvrage ID.}
\item{id}{The TV show TMDb ID.}
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

tv_external_ids(api_key = api_key, id = 1396)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_external_ids}
