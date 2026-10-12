\name{tv_episode_credits}
\alias{tv_episode_credits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV episode credits.
}
\description{
Get the TV episode credits by combination of season and episode number.
}
\usage{
tv_episode_credits(api_key, id, season_number, episode_number)
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
}
\value{
A list with the following fields:
\item{cast}{The cast of the TV episode.}
\item{crew}{The crew of the TV episode.}
\item{guest_stars}{The guest stars of the TV episode.}
\item{id}{The TV episode ID.}
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

tv_episode_credits(api_key = api_key, id = 1396, season_number = 4, episode_number = 4)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_episode_credits}
