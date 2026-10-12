\name{collection_images}
\alias{collection_images}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve images of a collection.
}
\description{
Get all of the images for a particular collection by collection id.
}
\usage{
collection_images(api_key, id, language = NA, append_to_response = NA,
include_image_language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The collection ID.
}
  \item{language}{
A ISO 639-1 code.
}
  \item{append_to_response}{
Comma separated, any collection method.
}
  \item{include_image_language}{
Comma separated, a valid ISO 69-1. Maximum 5 per request.
}
}
\details{
When include_image_language is present, the API will make an additional request behind the scenes to fetch the data you're asking for.
}
\value{
A list with the following fields:
\item{id}{The collection ID.}
\item{backdrops}{The backdrops of the collection.}
\item{posters}{Heigth, width, votes of the images of the collection.}
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

collection_images(api_key = api_key, id = 10, language = "it", include_image_language = "it")
}    
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{collection_images}
