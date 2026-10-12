\name{tv_season_credits}
\alias{tv_season_credits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV season credits.
}
\description{
Get the cast & crew credits for a TV season by season number.
}
\usage{
tv_season_credits(api_key, id, season_number)
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
}
\value{
A list with the following fields:
\item{cast}{The cast of the TV season.}
\item{crew}{The crew of the TV season.}
\item{id}{The TV season ID.}
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

tv_season_credits(api_key = api_key, id = 1396, season_number = 3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_season_credits}
