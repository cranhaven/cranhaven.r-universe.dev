\name{tv_episode_images}
\alias{tv_episode_images}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV episode images.
}
\description{
Get the images (episode stills) for a TV episode by combination of a season and episode number. Since episode stills don't have a language, this call will always return all images.
}
\usage{
tv_episode_images(api_key, id, season_number, episode_number)
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
\item{id}{The TV episode ID.}
\item{stills}{Heigth, width, votes of the images of the TV episode.}
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

tv_episode_images(api_key = api_key, id = 1396, season_number = 1, episode_number = 1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_episode_images}
