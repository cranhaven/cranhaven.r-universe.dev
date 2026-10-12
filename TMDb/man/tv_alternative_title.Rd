\name{tv_alternative_title}
\alias{tv_alternative_title}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve alternative titles for TV series.
}
\description{
Get the alternative titles for a specific TV show id.
}
\usage{
tv_alternative_title(api_key, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The TV series ID.
}
}
\value{
A list with the following fields:
\item{id}{The TV series ID.}
\item{results}{The alternative titles of the TV show.}
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

tv_alternative_title(api_key = api_key, id = 1396)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_alternative_title}
