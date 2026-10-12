\name{jobs}
\alias{jobs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a list of valid jobs.
}
\description{
Retrieve all the jobs for movies and TV shows splitted by department.
}
\usage{
jobs(api_key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
}

\value{
A single list item, with the following fields:
\item{department}{The departments involving for a movie/TV show.}
\item{job_list}{The jobs for each department.}
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

jobs(api_key = api_key)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{jobs}
