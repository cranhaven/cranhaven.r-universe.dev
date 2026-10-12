\name{tv_episode_external_ids}
\alias{tv_episode_external_ids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve external ids for a TV episode.
}
\description{
Get the external ids for a TV episode by comabination of a season and episode number.
}
\usage{
tv_episode_external_ids(api_key, id, season_number, episode_number, language = NA)
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
The season number refered to the TV show ID.
}
  \item{episode_number}{
The episode number refered to the TV show ID.
}
  \item{language}{
A ISO 639-1 code.
}
}
\value{
A list with the following fields:
\item{imdb_id}{The TV episode imdb ID.}
\item{freebase_mid}{The TV episode freebase_mid ID.}
\item{freebase_id}{The TV episode freebsae ID.}
\item{tvdb_id}{The TV episode tvdb ID.}
\item{tvrage_id}{The TV episode tvrage ID.}
\item{id}{The TV episode TMDb ID.}
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

tv_episode_external_ids(api_key = api_key, id = 1396, season_number = 4, episode_number = 4)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_episode_external_ids}
