\name{configuration}
\alias{configuration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the system wide configuration information.
}
\description{
This method currently holds the data relevant to building image URLs as well as the change key map.
}
\usage{
configuration(api_key)
}

\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
}
\details{
To build an image URL, you will need 3 pieces of data. The base_url, size and file_path. Simply combine them all and you will have a fully qualified URL.
}
\value{
The data provided with this method is required for building full image URLs, or getting a list of available image sizes. 
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

configuration(api_key = api_key)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{configuration}
