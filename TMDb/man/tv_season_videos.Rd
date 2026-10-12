\name{tv_season_videos}
\alias{tv_season_videos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV show videos related.
}
\description{
Get the videos that have been added to a TV season (trailers, teasers, etc...)
}
\usage{
tv_season_videos(api_key, id, season_number, language = NA)
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
\item{id}{The TV season ID.}
\item{results}{The videos associated with the TV season.}
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

tv_season_videos(api_key = api_key, id = 1396, season_number = 5, language = "en")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_season_videos}
