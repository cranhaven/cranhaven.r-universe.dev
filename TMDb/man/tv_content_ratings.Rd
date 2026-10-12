\name{tv_content_ratings}
\alias{tv_content_ratings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve rating for a TV series.
}
\description{
Get the content ratings for a specific TV show id.
}
\usage{
tv_content_ratings(api_key, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The TV show ID.
}
}
\value{
A list with the following fields:
\item{results}{The available ratings for the TV show.}
\item{id}{The TV shows ID.}
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

tv_content_ratings(api_key = api_key, id = 1396)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_content_ratings}
