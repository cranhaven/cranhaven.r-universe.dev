\name{search_multi}
\alias{search_multi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search the movie, TV show and person collections with a single query.
}
\description{
Each item returned in the result array has a media_type field that maps to either movie, TV show or person. Each mapped result is the same response you would get from each independent search.
}
\usage{
search_multi(api_key, query, page = 1, include_adult = NA, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{query}{
A CGI escaped string.
}
  \item{page}{
The number of the page to show. Minimum 1, maximum 1000.
}
  \item{include_adult}{
Toggle the inclusion of adult titles. Expected value is: true or false.
}
  \item{language}{
A ISO 639-1 code.
}
}
\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{The movies, TV shows and people that match the query.}
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

search_multi(api_key = api_key, query = "star wars")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{search_multi}
