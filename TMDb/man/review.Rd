\name{review}
\alias{review}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve basic informations about a review.
}
\description{
Get the full details of a review by ID.
}
\usage{
review(api_key, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The review ID.
}
}

\value{
A list with the following fields:
\item{id}{The review ID.}
\item{author}{The review's author.}
\item{content}{The review'text.}
\item{iso_639_1}{The review's language.}
\item{media_id}{The ID of the reviewed object.}
\item{media_title}{The object's title.}
\item{media_type}{The type of the reviewed object.}
\item{url}{The url of the review.}
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

review(api_key = api_key, id = "5013bc76760ee372cb00253e")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{review}
