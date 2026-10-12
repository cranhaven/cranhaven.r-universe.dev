\name{tv_season}
\alias{tv_season}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve basic informations about a TV season.
}
\description{
Get the primary information about a TV season by its season number.
}
\usage{
tv_season(api_key, id, season_number, language = NA, append_to_response = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The TV show ID.
}
  \item{season_number}{
The season number refered to the ID.
}
  \item{language}{
A ISO 639-1 code.
}
  \item{append_to_response}{
Comma separated, any tv season method.
}
}
\value{
A list with the following fields:
\item{air_date}{when a season is aired.}
\item{episodes}{A list with the season episodes: dates, crews, guest stars, overviews, titles.}
\item{name}{The TV season title.}
\item{overview}{A brief plot.}
\item{id}{The season ID.}
\item{poster_path}{The image's path.}
\item{season_number}{The season number.}
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

tv_season(api_key = api_key, id = 1396, season_number = 3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_season}
