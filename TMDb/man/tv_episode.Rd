\name{tv_episode}
\alias{tv_episode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve basic informations about a TV episode.
}
\description{
Get the primary information about a TV episode by combination of a season and episode number.
}
\usage{
tv_episode(api_key, id, season_number, episode_number, language = NA,
append_to_response = NA)
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
  \item{append_to_response}{
Comma separated, any TV series method.
}
}
\value{
A list with the following fields:
\item{air_date}{when an episode is aired.}
\item{crew}{The crew of the TV episode.}
\item{episode_number}{The episode number of the TV episode.}
\item{guest_stars}{The guest stars of the TV episode.}
\item{name}{The TV episode title.}
\item{overview}{A brief plot.}
\item{id}{The TV episode ID.}
\item{production_code}{The production code.}
\item{season_number}{The season number of the TV episode.}
\item{still_path}{The image's path.}
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

tv_episode(api_key = api_key, id = 1396, season_number = 4, episode_number = 4)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_episode}
