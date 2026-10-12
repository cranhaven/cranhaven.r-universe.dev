\name{person_changes}
\alias{person_changes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the changes for a person.
}
\description{
Get the changes for a specific person id.
}
\usage{
person_changes(api_key, id, start_date = NA, end_date = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The person ID.
}
  \item{start_date}{
The date starting from which to search. Format YYYY-MM-DD.
}
  \item{end_date}{
The date until which to search. Format YYYY-MM-DD.
}
}
\details{
Changes are grouped by key, and ordered by date in descending order. By default, only the last 24 hours of changes are returned. The maximum number of days that can be returned in a single request is 14. The language is present on fields that are translatable.
}
\value{
A list with the following fields:
\item{changes}{The type and the object of the change.}
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

person_changes(api_key = api_key, id = 1245)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{person_changes}
