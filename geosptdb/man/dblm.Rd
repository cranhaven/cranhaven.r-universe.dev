\name{dblm}
\alias{dblm}

\title{
Adjusting distance-based linear regression model 
}
\description{
\code{\link{dblm}} is a linear model variety where explanatory information is coded as distances among
individuals so these distances can also be computed from observed explanatory variables (a mix of continuous, qualitative explanatory variables or from more general quantities). The response is a continuous variable as in the classic linear model.

\code{\link[stats]{lm}} is used internally to adjust a distance-based linear regression model. The method considers the Gower's distance for mixed covariates (numeric, ordered, or factor), for explanation on the meaning of distance-based linear regression model and distance of Gower see the bibliography references below.
}
\usage{
dblm(data,y,sc,ev.min, ...)
}

\arguments{
\item{data}{matrix or data frame containing the explanatory variables. These variables can be numeric, ordered, or factor. Symmetric or asymmetric binary variables should be numeric and only contain 0 and 1. character variables will be converted to factor. NAs are tolerated. With these variables are built, the principal coordinates which later become the regressors in the linear model.}
\item{y}{the response variable used to fit the model}
\item{sc}{the value of the correlation squared to select the principal coordinates more related to the response variable. The default value is 0.003.}
\item{ev.min}{the minimum value to select the eigenvalues. These eigenvalues must be positive, the default value is 0.007}
\item{...}{further parameters to be passed to the \code{\link[FD]{gowdis}} function (see \code{\link[FD]{gowdis}}) of low level.}
}
\details{
The \code{\link{dblm}} model builds; principal coordinates matrix, eigenvalues, and a linear regression model. gowdis function used in \code{\link{dblm}} compute the Gower (1971) similarity coefficient exactly as described by Podani (1999), then converts it to a dissimilarity coefficient by using \eqn{D = 1 - S}. It integrates variable weights as described by Legendre and Legendre (1998).
}
\value{
A list containing the following components:
\item{table}{table with eigenvalues, correlations squared, and percentages of inertia associated with the most statistically significant principal coordinates (5\%) with the response variable.}
\item{ev}{the \eqn{n} eigenvalues computed during the scaling process (see \code{\link[stats]{cmdscale}}).}
\item{cp}{the \eqn{k} most statistically significant principal coordinates (5\%) with the response variable.}
\item{dbmodel}{returns a list of summary statistics of the fitted linear model.}
}

\references{
Cuadras, CM., Arenas C. and Fortiana, J. (1996). \emph{Some computational aspects of a distance-based model
for prediction}. Communications in Statistics B - Simulation and Computation 25, 593-609.

Cuadras, CM. and Arenas, C. (1990).\emph{A distance-based regression model for prediction with mixed data}. 
Communications in Statistics A - Theory and Methods 19, 2261-2279

Gower, J. C. (1971). \emph{A general coefficient of similarity and some of its properties}. Biometrics 27:857-871.

Legendre, P. and Legendre, L. (1998). \emph{Numerical Ecology}. 2nd English edition. Amsterdam: Elsevier.

Melo, C. E. (2012). \emph{Analisis geoestadistico espacio tiempo basado en distancias y splines con
aplicaciones}. PhD. Thesis. Universitat de Barcelona. 276 p. \href{http://www.tdx.cat/bitstream/handle/10803/101202/CEMM_TESIS.pdf}{[link]} 

Podani, J. (1999). \emph{Extending Gower's general coefficient of similarity to ordinal characters}. Taxon 48:331-340. 
}

\seealso{
See function \code{\link[FD]{gowdis}} in the \code{FD} package.
}

\examples{
# considering 10 principal coordinates (constructed from a distance-based linear 
# regression model)
\dontrun{
data(croatia.temp)
data(croatiadb)
croatia.temp[,7] <- as.factor(croatia.temp[,7])
dblm1 <- dblm(data=croatia.temp,y=croatiadb$MTEMP) 
str(dblm1)
}
}

\keyword{principal coordinates}
