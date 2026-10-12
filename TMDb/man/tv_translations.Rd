\name{tv_translations}
\alias{tv_translations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the available translations for a TV series.
}
\description{
Get the list of translations that exist for a TV series. These translations cascade down to the episode level.
}
\usage{
tv_translations(api_key, id)
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
\item{id}{The TV show ID.}
\item{translations}{The available translations of the TV show.}
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

tv_translations(api_key = api_key, id = 1396)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_translations}
