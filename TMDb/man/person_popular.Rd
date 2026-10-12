\name{person_popular}
\alias{person_popular}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the popular people.
}
\description{
Get the list of popular people on TMDb.
}
\usage{
person_popular(api_key, page = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{page}{
The number of the page to show. Minimum 1, maximum 1000.
}
}
\details{
This list refreshes every day.
}
\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{The most popular people, with a brief description.}
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

person_popular(api_key = api_key)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{person_popular}
