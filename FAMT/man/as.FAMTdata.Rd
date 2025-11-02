\name{as.FAMTdata}
\alias{as.FAMTdata}
\title{Create a 'FAMTdata' object from an expression, covariates and annotations dataset}

\description{The function creates a 'FAMTdata' object containing the expression, the covariates and the annotations dataset if provided. The function checks the consistency of dataframes between them. Then missing values of expression can be imputed.}

\usage{
as.FAMTdata(expression, covariates = NULL, annotations = NULL, idcovar = 1, 
idannot = NULL, na.action=TRUE)
}
\arguments{
  \item{expression}{An expression data frame with genes in rows and arrays in columns. The arrays are identified by the column names.}
  \item{covariates}{An optional data frame with arrays in rows, and covariates in columns. One column must contain the array identification (NULL by default).}
  \item{annotations}{An optional data frame containing informations on the genes (NULL by default)}
  \item{idcovar}{The column number corresponding to the array identification in the covariates data frame (1 by default)}
  \item{idannot}{The column number corresponding to the gene identification in annotations data frame (NULL by default)}
  \item{na.action}{If TRUE (default value), missing expression data are imputed using nearest neighbor averaging (\code{\link{impute.knn}} function of 'impute' package).}
}
\details{The \code{\link{as.FAMTdata}} function creates a single R object containing the data stored: 
- in one mandatory data-frame: the 'expression' dataset with m rows (if m tests) and n columns (n is the sample size) containing the observations of the responses.
- and two optional data frames: the 'covariates' dataset with n rows and at least 2 columns, one giving the specification to match 'expression' and 'covariates' and the other one containing the observations of at least one covariate. The optional dataset,'annotations', can be provided to help interpreting the factors: with m rows and at least one column to identify the variables (ID).
}
\value{
	\item{expression }{The expression data frame}
  \item{covariates }{The optional covariates data frame}
	\item{annotations}{The optional data frame containing annotations. The genes annotations such as the functional categories should be in a character form, not in a factor form.}
	\item{idcovar}{The column number corresponding to the array identification in the covariate data frame (which should correspond to the column names in 'expression')}
	\item{na.expr}{Rows and columns of expression with missing values}
}
%\references{}

\author{ David Causeur }

\note{The class of the data produced with the \code{\link{as.FAMTdata}} function is called 'FAMTdata'. We advise to carry out a summary of FAMT data with the function \code{\link{summaryFAMT}}.}

\seealso{\code{\link{summaryFAMT}}}

\examples{
# The data are divided into one mandatory data-frame, the gene expressions, 
#  and two optional datasets: the covariates, and the annotations.

# The expression dataset with 9893 rows (genes) and 43 columns (arrays)
#  containing the observations of the responses.
# The covariates dataset with 43 rows (arrays) and 6 columns: 
#  the second column gives the specification to match 'expression' 
#  and 'covariates' (array identification), the other ones contain
#  the observations of covariates.
# The annotations dataset contains 9893 rows (genes) and 
#  6 columns to help interpreting the factors, the first one (ID) 
#  identifies the variables (genes). 

data(expression)
data(covariates)
data(annotations)

# Create the 'FAMTdata'
############################################
chicken = as.FAMTdata(expression,covariates,annotations,idcovar=2)
# 'FAMTdata' summary
summaryFAMT(chicken)

}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
