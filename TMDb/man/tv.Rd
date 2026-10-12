\name{tv}
\alias{tv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV informations.
}
\description{
Get the primary information about a TV series by id.
}
\usage{
tv(api_key, id, language = NA, append_to_response = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The TV series ID.
}
  \item{language}{
A ISO 639-1 code.
}
  \item{append_to_response}{
Comma separated, any TV series method.
}
}
\value{
A list with the following fields:
\item{backdrop_path}{The backdrop of the TV show.}
\item{created_by}{The author of the TV show.}
\item{episode_run_time}{The length of an episode.}
\item{first_air_date}{The first day when TV show begun.}
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

tv(api_key = api_key, id = 1396)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv}
