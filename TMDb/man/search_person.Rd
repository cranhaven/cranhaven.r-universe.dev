\name{search_person}
\alias{search_person}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search for people by name.
}
\description{
Retrieve all the person IDs that match (entirely or partially) the query.
}
\usage{
search_person(api_key, query, page = 1, include_adult = NA, search_type = "phrase")
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
Toggle the inclusion of adult titles. Expected value is: true or false
}
  \item{search_type}{
By default, the search type is 'phrase'. This is almost guaranteed the option you will want. It's a great all purpose search type and by far the most tuned for every day querying. For those wanting more of an "autocomplete" type search, set this option to 'ngram'.
}
}
\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{The people that match the query.}
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

search_person(api_key = api_key, query = "Megan Fox")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{search_person}
