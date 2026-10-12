\name{search_collection}
\alias{search_collection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search for lists by name and description.
}
\description{
Retrieve all the company ID that match (entirely or partially) the query.
}
\usage{
search_collection(api_key, query, page = 1, language = NA)
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
  \item{language}{
A ISO 639-1 code.
}
}
\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{The collections that match the query.}
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

search_collection(api_key = api_key, query = "Marvel", language = "it")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{search_collection}
