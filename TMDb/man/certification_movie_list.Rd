\name{certification_movie_list}
\alias{certification_movie_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the list of supported certifications for movies.
}
\description{
This method provides the certifications for movies in the following countries: Australia, Canada, Great Britain, USA, France, Germany, New Zeland, India.
}
\usage{
certification_movie_list(api_key)
}

\arguments{
  \item{api_key}{
	Your TMDb Consumer Key.
}
}
\details{
These can be used in conjunction with the certification_country and certification.lte parameters when using discover.
}
\value{
A list of the supported movie certifications in the following countries:
\item{AU}{Australia}
\item{CA}{Canada}
\item{GB}{Great Britain}
\item{US}{USA}
\item{FR}{France}
\item{DE}{Germany}
\item{NZ}{New Zeland}
\item{IN}{India}
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

certification_movie_list(api_key = api_key)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{certification_movies}
