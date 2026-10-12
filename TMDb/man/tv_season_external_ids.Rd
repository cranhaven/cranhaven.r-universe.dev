\name{tv_season_external_ids}
\alias{tv_season_external_ids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve external ids for a TV season.
}
\description{
Get the external ids that we have stored for a TV season by season number.
}
\usage{
tv_season_external_ids(api_key, id, season_number, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The TV show ID.
}
  \item{season_number}{
The season number refered to the ID.
}
  \item{language}{
A ISO 639-1 code.
}
}
\value{
A list with the following fields:
\item{freebase_mid}{The TV season freebase_mid ID.}
\item{freebase_id}{The TV season freebsae ID.}
\item{tvdb_id}{The TV season tvdb ID.}
\item{tvrage_id}{The TV season tvrage ID.}
\item{id}{The TV season TMDb ID.}
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

tv_season_external_ids(api_key = api_key, id = 1396, season_number = 3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_season_external_ids}
