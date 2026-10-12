\name{company}
\alias{company}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve informations of a company.
}
\description{
This method is used to retrieve all of the basic information about a company.
}
\usage{
company(api_key, id, append_to_response = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The company ID.
}
  \item{append_to_response}{
Comma separated, any company method.
}
}

\value{
A list with the following fields:
\item{description}{A brief description about the company.}
\item{headquarters}{The headquarters of the company.}
\item{homepage}{The homepage of the company.}
\item{id}{The company ID.}
\item{logo_path}{The path of company's logo}
\item{name}{The company's name.}
\item{parent_company}{The associated companies.}
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

company(api_key = api_key , id = 1, append_to_response = "movies")
}  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{company}
