\name{genres_movie_list}
\alias{genres_movie_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the available genres for movies.
}
\description{
Get the list of movie genres.
}
\usage{
genres_movie_list(api_key, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{language}{
A ISO 639-1 code.
}
}

\value{
A single list item, with the following fields:
\item{id}{The ID of a movie genre.}
\item{name}{The name of a movie genre.}
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

genres_movie_list(api_key = api_key, language = "it")
}    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{genres_movie_list}
