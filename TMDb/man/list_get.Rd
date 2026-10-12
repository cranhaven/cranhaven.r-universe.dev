\name{list_get}
\alias{list_get}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a list by id.
}
\description{
Retrieve all the basic information about a list searched by ID.
}
\usage{
list_get(api_key, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The list ID.
}
}

\value{
A list with the following fields:
\item{created_by}{The author of the list.}
\item{description}{A brief description about the list.}
\item{favorite_count}{The number of favorites.}
\item{id}{The list ID.}
\item{items}{The elements of the list.}
\item{item_count}{The number of list's elements.}
\item{iso_639_1}{The language of the list.}
\item{name}{The name of the list.}
\item{poster_path}{The poster of the list.}
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

list_get(api_key = api_key, id = "509ec17b19c2950a0600050d")
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{list_get}
