\name{person_tagged_images}
\alias{person_tagged_images}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve images related to a person.
}
\description{
Get the images that have been tagged with a specific person id. 
}
\usage{
person_tagged_images(api_key, id, page = 1, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The person ID.
}
  \item{page}{
The number of the page to show. Minimum 1, maximum 1000.
}
  \item{language}{
A ISO 639-1 code.
}
}
\details{
Return all of the image results with a media object mapped for each image.
}
\value{
A list with the following fields:
\item{id}{The person ID.}
\item{page}{The current page for the results.}
\item{results}{Heigth, width, voted of the images related to the person, with a brief description.}
\item{total_pages}{The number of pages for the results.}
\item{total_results}{The number of results.}
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

person_tagged_images(api_key = api_key, id = 287)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{person_tagged_images}
