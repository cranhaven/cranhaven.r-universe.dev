\name{tv_episode_changes}
\alias{tv_episode_changes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the changes for an episode TV show.
}
\description{
Look up a TV episode's changes by episode ID.
}
\usage{
tv_episode_changes(api_key, id, start_date = NA, end_date = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api_key}{
Your TMDb Consumer Key.
}
  \item{id}{
The TV show ID.
}
  \item{start_date}{
The date starting from which to search. Format YYYY-MM-DD.
}
  \item{end_date}{
The date until which to search. Format YYYY-MM-DD.
}
}
\details{
This method is used in conjunction with the /tv/{id}/changes method. This method uses the episode_id value found in the change entries.
}
\value{
A list with the following fields:
\item{changes}{The type and the object of the change.}
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

tv_episode_changes(api_key = api_key, id = 63080)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{tv_episode_changes}
