\name{changes_tv}
\alias{changes_tv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a list of TV show ids that have been edited.
}
\description{
By default TMDb shows the last 24 hours and only 100 items per page. The maximum number of days that can be returned in a single request is 14.
}
\usage{
changes_tv(api_key, page = 1, start_date = NA, end_date = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{page}{
  	The number of the page to show. Minimum 1, maximum 1000.
}
  \item{start_date}{
The date starting from which to search. Format YYYY-MM-DD.
}
  \item{end_date}{
The date until which to search. Format YYYY-MM-DD.
}
}
\details{
The change log system to properly support TV was updated on May 13, 2014. You'll likely only find the edits made since then to be useful in the change log system.
}
\value{
A list with the following fields:

\item{results}{The IDs of the changed TV shows.}
\item{page}{The current page for the results.}
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

changes_tv(api_key = api_key, page = 2)
}	
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{changes_tv}
