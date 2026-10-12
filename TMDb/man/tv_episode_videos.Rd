\name{tv_episode_videos}
\alias{tv_episode_videos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV show videos related.
}
\description{
Get the videos that have been added to a TV episode (teasers, clips, etc...)
}
\usage{
tv_episode_videos(api_key, id, season_number, episode_number, language = NA)
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
\item{id}{The TV episode ID.}
\item{results}{The videos associated with the TV episode.}
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

tv_episode_videos(api_key = api_key, id = 62161, season_number = 1, episode_number = 2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_episode_videos}
