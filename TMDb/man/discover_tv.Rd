\name{discover_tv}
\alias{discover_tv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the TMDb TV shows list.
}
\description{
Discover TV shows by different types of data like average rating, number of votes, genres, the network they aired on and air dates.
}
\usage{
discover_tv(api_key, page = 1, language = NA, sort_by = NA, first_air_date_year = NA,
vote_count.gte = NA, vote_average.gte = NA, with_genres = NA, with_networks = NA,
first_air_date.gte = NA, first_air_date.lte = NA)
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
  \item{sort_by}{
Available options are: vote_average.desc, vote_average.asc, first_air_date.desc, first_air_date.asc, popularity.desc, popularity.asc
}
  \item{first_air_date_year}{
Filter the results release dates to matches that include this value. Expected value is a year.
}
  \item{vote_count.gte}{
Only include TV shows that are equal to, or have a vote count higher than this value. Expected value is an integer.
}
  \item{vote_average.gte}{
Only include TV shows that are equal to, or have a higher average rating than this value. Expected value is a float.
}
  \item{with_genres}{
Only include TV shows with the specified genres. Expected value is an integer (the id of a genre). Multiple values can be specified. Comma separated indicates an 'AND' query, while a pipe (|) separated value indicates an 'OR'.
}
  \item{with_networks}{
Filter TV shows to include a specific network. Expected value is an integer (the id of a network). They can be comma separated to indicate an 'AND' query.
}
  \item{first_air_date.gte}{
The minimum release to include. Expected format is YYYY-MM-DD.
}
  \item{first_air_date.lte}{
The maximum release to include. Expected format is YYYY-MM-DD.
}
}

\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{A list of TV shows that match the query's parameters.}
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

discover_tv(api_key = api_key, first_air_date.gte = "2005-01-01", with_networks = 49)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{discover_tv}
