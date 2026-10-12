\name{genres_movies}
\alias{genres_movies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the list of movies for a particular genre by id.
}
\description{
By default, only movies with 10 or more votes are included.
}
\usage{
genres_movies(api_key, id, page = 1, language = NA, include_all_movies = NA,
include_adult = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The movie genre ID.
}
  \item{page}{
The number of the page to show. Minimum 1, maximum 1000.
}
  \item{language}{
A ISO 639-1 code.
}
  \item{include_all_movies}{
Toggle the inclusion of all movies and not just those with 10 or more ratings. Expected value is: true or false.
}
  \item{include_adult}{
Toggle the inclusion of adult titles. Expected value is: true or false.
}
}

\value{
A list with the following fields:
\item{id}{The ID genre.}
\item{page}{The current page for the results.}
\item{results}{The movies belonging to the id genre.}
\item{total_pages}{The number of pages for the results.}
\item{total_results}{The number of results.}
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

genres_movies(api_key = api_key, id = 18, page = 1, include_all_movies = TRUE,
include_adult = TRUE)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{genres_movie}
