\name{movie_releases}
\alias{movie_releases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the release informations for a movie.
}
\description{
Get the release date and certification information by country for a specific movie id.
}
\usage{
movie_releases(api_key, id, append_to_response = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The movie ID.
}
  \item{append_to_response}{
Comma separated, any movie method.
}
}
\value{
A list with the following fields:
\item{id}{The movie ID.}
\item{countries}{The release dates in several countries.}
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

movie_releases(api_key = api_key, id = 578, append_to_response = "credits")
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{movie_releases}
