\name{cp.xnews}
\alias{cp.xnews}

\title{
Generate the principal coordinates of a new individual from Gower's distance.
}
\description{
Function for generates a numeric matrix with principal coordinates of a new individual then you could obtain distances from this matrix and you can do a prediction using a Gower's result (1971) and Cuadras & Arenas (1990) which relates the squared distances vector with the principal coordinates vector associated to the new individual.
}
\usage{
cp.xnews(newdata,eigenvalues, data,trend, ...)
}

\arguments{
\item{newdata}{data frame values of new individual.}
\item{eigenvalues}{the \eqn{n} eigenvalues computed during the scaling process (see \code{\link[stats]{cmdscale}})}
\item{data}{matrix or data frame containing the explanatory variables. These variables can be numeric, ordered, or factor, the symmetric or asymmetric binary variables should be numeric and only contain 0 and 1 character variables will be converted to factor. NAs are tolerated. With these variables the principal coordinates are built which become the regressors in the linear model.}
\item{trend}{matrix \eqn{nxk} of the \eqn{k} most statistically significant principal coordinates (5\%) with the response variable, obtained from the matrix or data frame containing explanatory variables.}
\item{...}{further parameters to be passed to the \code{\link[StatMatch]{gower.dist}} function (see \code{\link[StatMatch]{gower.dist}}).}
}
\references{
Cuadras, CM. and Arenas, C. (1990).\emph{A distance-based regression model for prediction with mixed data}. 
Communications in Statistics A - Theory and Methods 19, 2261-2279

Gower, J. C. (1971). \emph{A general coefficient of similarity and some of its properties}. Biometrics 27:857-871.

Melo, C. E. (2012). \emph{Analisis geoestadistico espacio tiempo basado en distancias y splines con
aplicaciones}. PhD. Thesis. Universitat de Barcelona. 276 p. \href{http://www.tdx.cat/bitstream/handle/10803/101202/CEMM_TESIS.pdf}{[link]} 
}
\value{
Returns a numeric matrix with principal coordinates of the new individual.
}
\seealso{
\code{\link{dblm}},  \code{\link{rbfST}}
}
\examples{
\dontrun{
data(croatia.temp)
data(croatiadb)
# prediction case: one point
point <- data.frame(670863,5043464,5,170,200,15.7,3)
names(point) <- c("x","y","t","dem","dsea","twi","est")

croatia.temp[,7] <- as.factor(croatia.temp[,7])
dblm1 <- dblm(data=croatia.temp,y=croatiadb$MTEMP)                                 
newdata1 <- t(cp.xnews(newdata=point,eigenvalues=dblm1$ev, data=croatia.temp,
                       trend=dblm1$cp))
colnames(newdata1) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")
}
}

\keyword{ spatial }
