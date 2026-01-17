\name{get_date_from_doi}
\alias{get_date_from_doi}
\concept{Digital Object Identifier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to retrieve publication date based on Digital Object Identifier (DOI)}
\description{
This function retrieves precise publication date by querying the Digital Object Identifier (DOI) web server. Alternatively, if extract_date_from_doi is set to TRUE, the function will first try to extract a publication year from the publication DOI string. If \code{\link{create_bibliography}} is called with retrieve_pubdates = TRUE, it calls \code{\link{get_date_from_doi}} for each record to confirm publication dates.
}
\usage{
get_date_from_doi(doi, extract_date_from_doi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doi}{Character string representing the Digital Object Identifier (DOI) of the publication}
  \item{extract_date_from_doi}{Flag indicating whether to try to simply extract publication year from the DOI string before restorting to online queries to the DOI server}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{Returns a date in YYYY-MM-DD format or YYYY-MM format if extract_date_from_doi is set to TRUE.}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Christian Vincenot (christian@vincenot.biz)
}
\note{Scopus records already contain the year of publication of scientific papers indexed. However, in some cases these are inaccurate and can be verified by comparing them with the date retrieved by this function. Note that }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{create_bibliography}}
}
\examples{
\dontrun{
# Query publication date from DOI server
get_date_from_doi(doi="10.1016/j.procs.2010.04.250",extract_date_from_doi=TRUE)
}
# Extract date from DOI string
get_date_from_doi(doi="10.1016/j.procs.2010.04.250",extract_date_from_doi=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
