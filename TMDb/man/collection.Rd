\name{collection}
\alias{collection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve informations of a collection.
}
\description{
Get the basic collection information for a specific collection id.
}
\usage{
collection(api_key, id, language = NA, append_to_response = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The collection ID.
}
  \item{language}{
A ISO 639-1 code.
}
  \item{append_to_response}{
Comma separated, any collection method.
}
}
\value{
A list with the following fields:
\item{id}{The ID collection.}
\item{name}{The name of the collection.}
\item{overview}{The content of the collection.}
\item{poster_path}{The poster of the collection.}
\item{backdrop_path}{The backdrop of the collection.}
\item{parts}{The items of the collection.}
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

collection(api_key, 10, language = "it", append_to_response = "images")
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{collection}
