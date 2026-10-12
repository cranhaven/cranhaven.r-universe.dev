\name{movie_popular}
\alias{movie_popular}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the popular movies.
}
\description{
Get the list of popular movies on The Movie Database. This list refreshes every day.
}
\usage{
movie_popular(api_key, page = 1, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{page}{
The number of the page to show. Minimum 1, maximum 1000.
}
  \item{language}{
A ISO 639-1 code.
}
}
\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{The most popular movies, with a brief description.}
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

movie_popular(api_key = api_key, page = 2, language = "en")
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{movie_popular}
