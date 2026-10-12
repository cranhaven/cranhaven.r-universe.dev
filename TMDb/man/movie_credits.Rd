\name{movie_credits}
\alias{movie_credits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve credits for a movie.
}
\description{
Get the cast and crew information for a specific movie id.
}
\usage{
movie_credits(api_key, id, append_to_response = NA)
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
\item{cast}{The cast of the movie.}
\item{crew}{The crew of the movie.}
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

movie_credits(api_key = api_key, id = 550)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{movie_credits}
