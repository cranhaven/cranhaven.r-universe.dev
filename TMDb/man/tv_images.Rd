\name{tv_images}
\alias{tv_images}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV series images.
}
\description{
Get the images (posters and backdrops) for a TV series.
}
\usage{
tv_images(api_key, id, language = NA, include_image_language = NA)
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
  \item{include_image_language}{
A comma separated, a valid ISO 69-1. Maximum 5 per request.
}
}
\value{
A list with the following fields:
\item{id}{The TV show ID.}
\item{backdrops}{The backdrops of the TV show.}
\item{posters}{Heigth, width, votes of the images of the TV show.}
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

tv_images(api_key = api_key, id = 1396)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_images}
