\name{person_external_ids}
\alias{person_external_ids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve external ids for a person.
}
\description{
Get the external ids for a specific person id.
}
\usage{
person_external_ids(api_key, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The person ID.
}
}
\value{
A list with the following fields:
\item{imdb_id}{The person imdb ID.}
\item{freebase_mid}{The person freebase_mid ID.}
\item{freebase_id}{The person freebsae ID.}
\item{tvrage_id}{The person tvrage ID.}
\item{id}{The person TMDb ID.}
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

person_external_ids(api_key = api_key, id = 287)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{person_external_ids}
