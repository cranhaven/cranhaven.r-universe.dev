\name{movie_images}
\alias{movie_images}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve movie images.
}
\description{
Get the images (posters and backdrops) for a specific movie id.
}
\usage{
movie_images(api_key, id, language = NA, append_to_response = NA,
include_image_language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The movie ID.
}
  \item{language}{
A ISO 639-1 code.
}
  \item{append_to_response}{
A comma separated, any movie method.
}
  \item{include_image_language}{
A comma separated, a valid ISO 69-1. Maximum 5 per request.
}
}

\value{
A list with the following fields:
\item{id}{The movie ID.}
\item{backdrops}{The backdrops of the movie.}
\item{posters}{Heigth, width, votes of the images of the movie.}
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

movie_images(api_key = api_key, id = 550, language = "it")
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{movie_images}
