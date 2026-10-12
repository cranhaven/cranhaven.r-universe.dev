\name{find_tmdb}
\alias{find_tmdb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve objects in TMDb database by an external id.
}
\description{
The find method makes it easy to search for objects in our database by an external id. For instance, an IMDB ID. This will search all objects (movies, TV shows and people) and return the results in a single response. TV season and TV episode searches will be supported shortly.
}
\usage{
find_tmdb(api_key, id, external_source, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The supported external sources for each object are as follows:

- Movies: imdb_id

- People: imdb_id, freebase_mid, freebase_id, tvrage_id

- Series: imdb_id, freebase_mid, freebase_id, tvdb_id, tvrage_id

- TV Seasons: freebase_mid, freebase_id, tvdb_id, tvrage_id

- TV Episodes: imdb_id, freebase_mid, freebase_id, tvdb_id, tvrage_id
}
  \item{external_source}{
An external ID, like an IMDB ID.
}
  \item{language}{
A ISO 639-1 code.
}
}
\value{
A list with the following fields:
\item{movie_results}{The results in the movies department.}
\item{person_results}{The results in the person department.}
\item{tv_results}{The results in the tv department.}
\item{tv_episode_results}{The results in the tv_episode department.}
\item{tv_season_results}{The results in the tv_season department.}
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

find_tmdb(api_key = api_key, id = "tt0266543", external_source = "imdb_id")
}    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{find_tmdb}
