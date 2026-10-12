\name{person_images}
\alias{person_images}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve person images.
}
\description{
Get the images for a specific person id.
}
\usage{
person_images(api_key, id)
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
\item{id}{The person ID.}
\item{profiles}{The images of the person.}
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

person_images(api_key = api_key, id = 287)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{person_images}
