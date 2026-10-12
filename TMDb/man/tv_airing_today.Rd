\name{tv_airing_today}
\alias{tv_airing_today}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV shows on air.
}
\description{
Get the list of TV shows that air today.
}
\usage{
tv_airing_today(api_key, page = 1, language = NA, timezone = NA)
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
  \item{timezone}{
Valid value from the list of timezones.
}
}
\details{
Without a specified timezone, this query defaults to EST (Eastern Time UTC-05:00).
}
\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{The TV shows that are on air in the current day.}
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

tv_on_the_air(api_key = api_key)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_airing_today}
