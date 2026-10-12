\name{keyword}
\alias{keyword}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the basic information for a specific keyword id.
}
\description{
Get the basic information for a specific keyword id.
}
\usage{
keyword(api_key, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The keyword ID.
}
}

\value{
A list with the following fields:
\item{id}{The keyword ID.}
\item{name}{The text associated with the ID keyword.}
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

keyword(api_key = api_key, id = "1721")
} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{keyword}
