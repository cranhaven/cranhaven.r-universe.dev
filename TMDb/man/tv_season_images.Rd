\name{tv_season_images}
\alias{tv_season_images}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV season images.
}
\description{
Get the images (posters) stored for a TV season by season number.
}
\usage{
tv_season_images(api_key, id, season_number, language = NA, include_image_language = NA)
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
  \item{include_image_language}{
Comma separated, a valid ISO 69-1. Maximum 5 per request.
}
}
\value{
A list with the following fields:
\item{id}{The TV season ID.}
\item{posters}{Heigth, width, votes of the images of the TV season.}
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

tv_season_images(api_key = api_key, id = 1396, season_number = 2, language = "en", 
include_image_language = "en")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_season_images}
