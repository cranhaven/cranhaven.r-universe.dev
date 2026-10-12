\name{list_item_status}
\alias{list_item_status}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Verify the presence of a movie in a list.
}
\description{
Check to see if a movie ID is already added to a list.
}
\usage{
list_item_status(api_key, id, movie_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The list ID.
}
  \item{movie_id}{
The movie ID to check.
}
}
\value{
A list with the following fields:
\item{id}{The list ID.}
\item{item_present}{A logical value that indicates if a movie belongs (TRUE) or not (FALSE) to the list.}
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

list_item_status(api_key = api_key, id = "509ec17b19c2950a0600050d", movie_id = "279")
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{list_item_status}
