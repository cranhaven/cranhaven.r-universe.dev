\name{network}
\alias{network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the name of a TV network by ID.
}
\description{
Retrieve the name of a TV network.
}
\usage{
network(api_key, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The network ID.
}
}
\note{
Use this ID to search for TV shows with the discover method.
}
\value{
A list with the following fields:
\item{id}{The network ID.}
\item{name}{The name of network.}
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

network(api_key = api_key, id = 49)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{network}
