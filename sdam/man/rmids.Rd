\name{rmids}
\alias{rmids}
\title{
Restricted multiply-imputed data subsets
}
\description{
A function to perform multiple imputation of missing dating data in the \code{EDH} dataset.
}
\usage{
rmids(x, vars, collapse, pool, type = c("1", "2"))
}
\arguments{
  \item{x}{
  dataframe or list of dataframes with a data set to impute
}
  \item{vars}{
  vector of attribute variables in \code{x}, typically dating data (optional)
}
  \item{collapse}{
  collapse list of dataframes?  (optional and logical, default \code{FALSE})
}
  \item{pool}{
  pool the results?  (optional and logical)
}
  \item{type}{
  type of pooling: \code{"1"} for min TAQ and max TPQ. \code{"2"} for conditional pooling
}
}
\details{
\emph{Imputation} refers to the replacement process of missing data, and this is the case of entries in the 
Epigraphic Database Heidelberg and related datasets. 
In this context, the missing data for imputation are the endpoints of the timespan of existence of epigraphs or inscriptions 
represented by variables TAQ and TPQ (cf. \code{\link{prex}}) as \code{"not_before"} and \code{"not_after"} in the \code{EDH} dataset 
with cases of censoring with one limit of the timespan known.

To perform imputation on subsets of missing dating data in the \code{EDH} dataset, 
function \code{\link{edhwpd}} serves to organize records per Roman province and dates 
by simple match similarity of different attribute variables specified in \code{vars}. 
Such organisation is in the form of a dataframe or a list of dataframes depending on the province characteristics, and 
a restricted multiply-imputed data subsets takes place on this outcome, and where \code{collapse} is for 
collapsing lists of dataframes.

When dating data is complete missing, \code{\link{rpd}} provides the average date, min TAQ, max TPQ, and the average length timespan 
for each Roman province that applies for a multiple imputation. 
}
\value{
A list of dataframes with imputed data where imputed dating data is not preceeded by a zero 
as with the recorded values. Component cases and names are:

  \item{\code{NA-NA}}{all missing}
  \item{\code{taq-NA}}{censored}
  \item{\code{NA-tpq}}{censored}
  \item{\code{complete}}{complete data}

}
\references{
Ostoic, A and Letina, S. ``Network imputation for missing dating data in archaeological artefacts,''
\emph{The Connected Past: Artefactual Intelligence} conference, Aarhus (2021).
}
\author{
Antonio Rivero Ostoic
}
\note{
Rownames of complete dating data belonging to a component having imputed data gets replaced in the collapsed 
dataframe produced from a list of dataframes.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{edhwpd}}, \code{\link{rpd}}, \code{\link{edhw}}, \code{\link{get.edh}}, \code{\link{cln}}
}
\examples{
\dontrun{
# extract from EDH dataset province, dates, and single variable attribute
arm <- edhwpd(vars="type_of_inscription", province="Arm", dates=c("not_after", "not_before"))

# perform restricted imputation
rmids(arm, vars=c("not_after", "not_before"))}
}
\keyword{ datagen }
\concept{ imputation }
