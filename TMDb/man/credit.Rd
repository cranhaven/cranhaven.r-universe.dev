\name{credit}
\alias{credit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve credits.
}
\description{
Get the detailed information about a particular credit record.
}
\usage{
credit(api_key, credit_id, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{credit_id}{
The credit ID.
}
  \item{language}{
A ISO 639-1 code.
}
}
\details{
This is currently only supported with the new credit model found in TV. These ids can be found from any TV credit response as well as the tv_credits and combined_credits methods for people.
}
\value{
A list with the following fields:
\item{credit_type}{The credit's type.}
\item{department}{The section to which a person belongs.}
\item{job}{The job of a person.}
\item{media}{Name, ID and character associated with the person.}
\item{media_type}{The media's type.}
\item{id}{The credit ID.}
\item{person}{Name and ID of the character associated with the person.}
}
\references{
https://developers.themoviedb.org/3/getting-started
}
\author{
Andrea Capozio
}
\note{
The episodes object returns a list of episodes and are generally going to be guest stars. The season array will return a list of season numbers. Season credits are credits that were marked with the "add to every season" option in the editing interface and are assumed to be "season regulars".
}

\examples{
\dontrun{
## An example of an authenticated request,
## where api_key is fictitious.
## You can obtain your own at https://www.themoviedb.org/documentation/api

api_key <- "key"

credit(api_key = api_key, credit_id = "52542282760ee313280017f9", language = "it")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{credit}
