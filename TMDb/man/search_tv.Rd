\name{search_tv}
\alias{search_tv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search for TV shows by title.
}
\description{
Retrieve all the TV shows ID that match (entirely or partially) the query.
}
\usage{
search_tv(api_key, query, page = 1, language = NA, first_air_date_year = NA,
search_type = "phrase")
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
  \item{first_air_date_year}{
Filter the results to only match shows that have a air date with with value.
}
  \item{search_type}{
By default, the search type is 'phrase'. This is almost guaranteed the option you will want. It's a great all purpose search type and by far the most tuned for every day querying. For those wanting more of an "autocomplete" type search, set this option to 'ngram'.
}
}
\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{The TV shows that match the query.}
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

search_tv(api_key = api_key, query = "Game of Thrones")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{search_tv}
