\name{search_keyword}
\alias{search_keyword}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search for keywords by name.
}
\description{
Retrieve all the TMDb ID that match (entirely or partially) the query.
}
\usage{
search_keyword(api_key, query, page = 1)
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
}
\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{The keywords that match the query.}
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

search_keyword(api_key = api_key, query = "furious")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{search_keyword}
