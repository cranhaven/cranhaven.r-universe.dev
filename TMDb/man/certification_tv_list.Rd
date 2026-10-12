\name{certification_tv_list}
\alias{certification_tv_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the list of supported certifications for TV shows.
}
\description{
This method provides the certifications for TV shows in the following country: Australia, Canada, Great Britain, USA, France, Germany, Russia, Thailandia, Brazil, South Korea
}
\usage{
certification_tv_list(api_key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
}

\value{
A list of the supported TV show certifications in the following countries:
\item{AU}{Australia}
\item{CA}{Canada}
\item{GB}{Great Britain}
\item{US}{USA}
\item{FR}{France}
\item{DE}{Germany}
\item{NZ}{Russia}
\item{TH}{Thailandia}
\item{BR}{Brazil}
\item{KR}{South Korea}
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

	certification_tv_list(api_key = api_key)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{certification_tv_shows}