\name{keyword_movies}
\alias{keyword_movies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve movies for keyword.
}
\description{
Get the list of movies for a particular keyword by id.
}
\usage{
keyword_movies(api_key, id, page = 1, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The keyword ID.
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
\item{id}{The keyword ID.}
\item{page}{The current page for the results.}
\item{results}{The movies associated with the keyword ID.}
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

keyword_movies(api_key = api_key, id = "1721", page = 9, language = "it")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{keyword_movies}
