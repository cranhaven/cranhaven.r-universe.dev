\name{defacto}
\alias{defacto}
\title{FAMT factors description}

\description{This function helps the user to describe and interpret the factors using some available external information on either genes or arrays. Diagnostic plots are provided.}

\usage{
defacto(model, plot = TRUE, axes = c(1, 2), select.covar = NULL, 
select.annot = NULL, lim.b = 0.01, lab = TRUE, cex = 1)
}
\arguments{
  \item{model}{'FAMTmodel' object}
  \item{plot}{Boolean (TRUE by default). If TRUE, diagnostic plots are provided (unless the 'FAMTmodel' has less than one factor).}
  \item{axes}{Vector of length 2, specifying the factors to plot.}
  \item{select.covar}{Selection of external covariates. If NULL (default value), the function takes all covariates except the array identifiers and those used in the model.}
  \item{select.annot}{Selection of external annotations. If NULL (default value), the function takes all the available factors in 'annotations'.}
  \item{lim.b}{Proportion of variables with the highest loadings for each factor to appear on plots or in tables (0.01 by default).}
  \item{lab}{Boolean. If TRUE (default value), array names are labeled on the figure}
  \item{cex}{A numerical value giving the amount by which plotting text and symbol should be enlarged relative to the default (1 is the default value)}
}
%\details{}
\value{
  \item{loadings}{highest loadings (B matrix) for each factor. The proportion of loading is determined by "lim.b"}
  \item{covariates}{Matrix of p-values for the tests of linear relationships between scores on each factor (rows) and external covariates (columns).}
  \item{annotations}{Matrix of p-values for the tests of linear relationships between loadings of each factor (rows) and external annotations (columns).}
}
%\references{}
\author{David Causeur, Maela Kloareg}
%\note{}

\seealso{\code{\link{as.FAMTdata}}, \code{\link{modelFAMT}}
}
\examples{
## FAMT data
data(expression)
data(covariates)
data(annotations)

# Create the 'FAMTdata'
############################################
chicken = as.FAMTdata(expression,covariates,annotations,idcovar=2)
# 'FAMTdata' summary 
\dontrun{summaryFAMT(chicken)}

# FAMT complete multiple testing procedure
############################################
model = modelFAMT(chicken,x=c(3,6),test=6,nbf=3)
# summary on the 'FAMT model'
\dontrun{summaryFAMT(model)}

# Factors description
############################################
chicken.defacto = defacto(model,axes=1:2,select.covar=4:5,select.annot=3:6,
cex=0.6)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
