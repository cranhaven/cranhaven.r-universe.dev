\name{tv_latest}
\alias{tv_latest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the latest TV show inserted in TMDb.
}
\description{
Get the latest TV show id.
}
\usage{
tv_latest(api_key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
}
\value{
A list with the following fields:
\item{backdrop_path}{The backdrop of the TV show.}
\item{created_by}{The author of the TV show.}
\item{episode_run_time}{The length of an episode.}
\item{first_air_date}{the first time that TV show is aired}
\item{genres}{The TV show's genre.}
\item{homepage}{The TV show's homepage.}
\item{id}{The TV show ID.}
\item{in_production}{A logical value that indicates if a TV shows is in production (TRUE) or not (FALSE).}
\item{languages}{A list of available languages.}
\item{last_air_date}{The last day when TV show ended.}
\item{name}{The TV show name.}
\item{networks}{The companies involved with the TV show.}
\item{number_of_episodes}{The number of episodes of the TV show.}
\item{number_of_seasons}{The number of seasons of the TV show.}
\item{origin_country}{The country where TV show is produced.}
\item{original_language}{The original language of the TV show.}
\item{original_name}{The original name of the TV show.}
\item{overview}{A brief plot.}
\item{popularity}{An index of the TV show's popularity.}
\item{poster_path}{The poster of the TV show.}
\item{production_companies}{The producer companies.}
\item{seasons}{The TV show's seasons.}
\item{status}{The TV show's status.}
\item{type}{The TV show's type.}
\item{vote_average}{The average of the votes.}
\item{vote_count}{The number of votes.}
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

tv_latest(api_key = api_key)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_latest}
