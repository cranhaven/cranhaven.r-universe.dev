\name{person_latest}
\alias{person_latest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve new entry people on TMDb.
}
\description{
Get the latest person id.
}
\usage{
person_latest(api_key, page = 1)
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
\value{
A list with the following fields:
\item{adult}{A logical value that indicates if a person plays movie for adult (TRUE) or not (FALSE).}
\item{also_known_as}{The artistic name.}
\item{biography}{The person's biography.}
\item{birthday}{The person's birthday.}
\item{deathday}{The person's (eventual) deathday.}
\item{homepage}{The person's homepage.}
\item{id}{The person ID.}
\item{imdb_id}{The person imdb id.}
\item{name}{The person's name.}
\item{place_of_birth}{Where the person was born.}
\item{popularity}{An index of the person's popularity.}
\item{profile_path}{The person's profile path.}
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

person_latest(api_key = api_key)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{person_latest}
