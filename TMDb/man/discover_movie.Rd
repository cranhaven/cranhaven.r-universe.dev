\name{discover_movie}
\alias{discover_movie}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the TMDb movies list.
}
\description{
Discover movies by different types of data like average rating, number of votes, genres and certifications.
}
\usage{
discover_movie(api_key, certification_country = NA, certification = NA,
certification.lte = NA, include_adult = FALSE, include_video = TRUE,
language = NA, page = 1, primary_release_year = NA, primary_release_date.gte = NA,
primary_release_date.lte = NA, release_date.gte = NA, release_date.lte = NA,
sort_by = NA, vote_count.gte = NA, vote_count.lte = NA, vote_average.gte = NA,
vote_average.lte = NA, with_cast = NA, with_crew = NA, with_companies = NA,
with_genres = NA, with_keywords = NA, with_people = NA, year = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{certification_country}{
Only include movies with certifications for a specific country. When this value is specified, 'certification.lte' is required. A ISO 3166-1 is expected.
}
  \item{certification}{
Only include movies with this certification. Expected value is a valid certification for the specificed 'certification_country'.
}
  \item{certification.lte}{
Only include movies with this certification and lower. Expected value is a valid certification for the specificed 'certification_country'.
}
  \item{include_adult}{
Toggle the inclusion of adult titles. Expected value is a boolean, true or false. Default is false.
}
  \item{include_video}{
Toggle the inclusion of items marked as a video. Expected value is a boolean, true or false. Default is true.
}
  \item{language}{
A ISO 639-1 code.
}
  \item{page}{
The number of the page to show. Minimum 1, maximum 1000.
}
  \item{primary_release_year}{
Filter the results so that only the primary release date year has this value. Expected value is a year.
}
  \item{primary_release_date.gte}{
Filter by the primary release date and only include those which are greater than or equal to the specified value. Expected format is YYYY-MM-DD.
}
  \item{primary_release_date.lte}{
Filter by the primary release date and only include those which are greater than or equal to the specified value. Expected format is YYYY-MM-DD.
}
  \item{release_date.gte}{
Filter by all available release dates and only include those which are greater or equal to the specified value. Expected format is YYYY-MM-DD.
}
  \item{release_date.lte}{
Filter by all available release dates and only include those which are less or equal to the specified value. Expected format is YYYY-MM-DD.
}
  \item{sort_by}{
Available options are: popularity.asc, popularity.desc, release_date.asc, release_date.desc, revenue.asc, revenue.desc, primary_release_date.asc, primary_release_date.desc, original_title.asc, original_title.desc, vote_average.asc, vote_average.desc, vote_count.asc, vote_count.desc
}
  \item{vote_count.gte}{
Filter movies by their vote count and only include movies that have a vote count that is equal to or lower than the specified value.
}
  \item{vote_count.lte}{
Filter movies by their vote count and only include movies that have a vote count that is equal to or lower than the specified value. Expected value is an integer.
}
  \item{vote_average.gte}{
Filter movies by their vote average and only include those that have an average rating that is equal to or higher than the specified value. Expected value is a float.
}
  \item{vote_average.lte}{
Filter movies by their vote average and only include those that have an average rating that is equal to or lower than the specified value. Expected value is a float.
}
  \item{with_cast}{
Only include movies that have this person id added as a cast member. Expected value is an integer (the id of a person). Comma separated indicates an 'AND' query, while a pipe (|) separated value indicates an 'OR'.
}
  \item{with_crew}{
Only include movies that have this person id added as a crew member. Expected value is an integer (the id of a person). Comma separated indicates an 'AND' query, while a pipe (|) separated value indicates an 'OR'.
}
  \item{with_companies}{
Filter movies to include a specific company. Expected value is an integer (the id of a company). Comma separated indicates an 'AND' query, while a pipe (|) separated value indicates an 'OR'.
}
  \item{with_genres}{
Only include movies with the specified genres. Expected value is an integer (the id of a genre). Multiple values can be specified. Comma separated indicates an 'AND' query, while a pipe (|) separated value indicates an 'OR'.
}
  \item{with_keywords}{
Only include movies with the specified genres. Expected value is an integer (the id of a genre). Multiple values can be specified. Comma separated indicates an 'AND' query, while a pipe (|) separated value indicates an 'OR'.
}
  \item{with_people}{
Only include movies that have these person id's added as a cast or crew member. Expected value is an integer (the id or ids of a person). Comma separated indicates an 'AND' query, while a pipe (|) separated value indicates an 'OR'.
}
  \item{year}{
Filter the results by all available release dates that have the specified value added as a year. Expected value is an integer (year).
}
}

\value{
A list with the following fields:
\item{page}{The current page for the results.}
\item{results}{A list of movies that match the query's parameters.}
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

discover_movie(api_key = api_key, certification_country = "IT", certification.lte = 16)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{discover_movie}
