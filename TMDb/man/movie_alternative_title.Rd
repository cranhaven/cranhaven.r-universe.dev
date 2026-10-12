\name{movie_alternative_title}
\alias{movie_alternative_title}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve alternative titles for movies.
}
\description{
Get the alternative titles for a specific movie id.
}
\usage{
movie_alternative_title(api_key, id, country = NA, append_to_response = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The movie ID.
}
  \item{country}{
A ISO 3166-1 code.
}
  \item{append_to_response}{
Comma separated, any movie method.
}
}
\value{
A list with the following fields:
\item{id}{The movie ID.}
\item{titles}{The alternative titles of the movie.}
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

movie_alternative_title(api_key = api_key, id = 550, country = "TW")
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{movie_alternative_title}
