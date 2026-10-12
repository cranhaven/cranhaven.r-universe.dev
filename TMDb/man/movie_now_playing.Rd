\name{movie_now_playing}
\alias{movie_now_playing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve movies in theatres in the current week.
}
\description{
Get the list of movies playing that have been, or are being released this week. This list refreshes every day.
}
\usage{
movie_now_playing(api_key, page = 1, language = NA)
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
\item{results}{The movies now in theaters, with a brief description.}
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

movie_now_playing(api_key = api_key, page = 2, language = "it")
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{movie_now_playing}
