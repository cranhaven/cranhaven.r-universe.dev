\name{tv_videos}
\alias{tv_videos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve TV series videos.
}
\description{
Get the videos that have been added to a TV series (trailers, opening credits, etc...).
}
\usage{
tv_videos(api_key, id, language = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The TV series ID.
}
  \item{language}{
A ISO 639-1 code.
}
}
\value{
A list with the following fields:
\item{id}{The TV show ID.}
\item{results}{The videos associated with the TV show.}
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

tv_videos(api_key = api_key, id = 1396)
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_videos}
