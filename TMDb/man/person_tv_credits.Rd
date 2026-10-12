\name{person_tv_credits}
\alias{person_tv_credits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve person TV credits.
}
\description{
Get the TV credits for a specific person id.
}
\usage{
person_tv_credits(api_key, id, language = NA, append_to_response = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The person ID.
}
  \item{language}{
A ISO 639-1 code.
}
  \item{append_to_response}{
Comma separated, any person method.
}
}
\value{
A list with the following fields:
\item{cast}{The tv shows in which the person has played.}
\item{crew}{The tv shows in which the person has had crew roles.}
\item{id}{The person ID.}
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

person_tv_credits(api_key = api_key, id = 287)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{person_tv_credits}
