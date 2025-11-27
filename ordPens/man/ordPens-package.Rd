\name{ordPens-package}
\alias{ordPens-package}
\alias{ordPens}
\docType{package}
\title{
Selection and/or Smoothing and Principal Components Analysis for Ordinal Variables
}
\description{
Selection, and/or smoothing/fusing of ordinally scaled independent variables using 
a group lasso or generalized ridge penalty. Nonlinear principal components analysis for ordinal variables using a second-order difference penalty. 

}
\details{
\tabular{ll}{
Package: \tab ordPens\cr
Type: \tab Package\cr
Version: \tab 1.1.0\cr
Date: \tab 2023-07-10\cr
Depends: \tab grplasso, mgcv, RLRsim, quadprog, glmpath\cr
Imports: \tab ordinalNet\cr
Suggests: \tab psy\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}

Smoothing and selection of ordinal predictors is done by the function 
\code{\link{ordSelect}}; smoothing only, by \code{\link{ordSmooth}}; fusion and selection of ordinal predictors by \code{\link{ordFusion}}. For
ANOVA with ordinal factors, use \code{\link{ordAOV}}. Nonlinear PCA, performance evaluation and selection of an optimal 
penalty parameter can be done using \code{\link{ordPCA}}.
}
\author{
\emph{Authors:}
Jan Gertheiss \email{jan.gertheiss@hsu-hh.de},
Aisouda Hoshiyar \email{aisouda.hoshiyar@hsu-hh.de}.

\emph{Contributors:}
Fabian Scheipl

\emph{Maintainer:} Aisouda Hoshiyar \email{aisouda.hoshiyar@hsu-hh.de}
}
\references{
Gertheiss, J. (2014). \emph{ANOVA for factors with ordered levels}, Journal of
Agricultural, Biological and Environmental Statistics, 19, 258-277.

Gertheiss, J., S. Hogger, C. Oberhauser and G. Tutz (2011). \emph{Selection
of ordinally scaled independent variables with applications to international
classification of functioning core sets}.
Journal of the Royal Statistical Society C (Applied Statistics), 60, 377-395.

Gertheiss, J. and F. Oehrlein (2011). \emph{Testing relevance and linearity of
ordinal predictors}, Electronic Journal of Statistics, 5, 1935-1959.

Gertheiss, J., F. Scheipl, T. Lauer, and H. Ehrhardt (2022). \emph{Statistical 
inference for ordinal predictors in generalized linear and additive models with
application to bronchopulmonary dysplasia}. BMC research notes, 15, 112.

Gertheiss, J. and G. Tutz (2009). \emph{Penalized regression with ordinal 
predictors}. International Statistical Review, 77, 345-365.

Gertheiss, J. and G. Tutz (2010). \emph{Sparse modeling of categorial explanatory
variables}. The Annals of Applied Statistics, 4, 2150-2180.

Hoshiyar, A., H.A.L. Kiers, and J. Gertheiss (2021). \emph{Penalized non-linear principal components analysis for ordinal variables with an application to international classification of functioning core sets},  British Journal of Mathematical and Statistical Psychology, 76, 353-371.

Hoshiyar, A., Gertheiss, L.H., and Gertheiss, J. (2023). \emph{Regularization and    Model Selection for Item-on-Items Regression with Applications to Food Products' Survey Data.} Preprint, available from https://arxiv.org/abs/2309.16373.

Tutz, G. and J. Gertheiss (2014). \emph{Rating scales as predictors -- the old
question of scale level and some answers}. Psychometrica, 79, 357-376.
  
Tutz, G. and J. Gertheiss (2016). \emph{Regularized regression for categorical
data}. Statistical Modelling, 16, 161-200.
}

\keyword{package}

\seealso{\code{\link{ordSelect}}, \code{\link{ordSmooth}}, 
\code{\link{ordFusion}}, \code{\link{ordAOV}}, \code{\link{ordPCA}} }


\examples{
\dontrun{
### smooth modeling of a simulated dataset
set.seed(123)

# generate (ordinal) predictors
x1 <- sample(1:8,100,replace=TRUE)
x2 <- sample(1:6,100,replace=TRUE)
x3 <- sample(1:7,100,replace=TRUE)

# the response
y <- -1 + log(x1) + sin(3*(x2-1)/pi) + rnorm(100)

# x matrix
x <- cbind(x1,x2,x3)

# lambda values
lambda <- c(1000,500,200,100,50,30,20,10,1)

# smooth modeling
o1 <- ordSmooth(x = x, y = y, lambda = lambda)

# results
round(o1$coef,digits=3)
plot(o1)

# If for a certain plot the x-axis should be annotated in a different way,
# this can (for example) be done as follows:
plot(o1, whx = 1, xlim = c(0,9), xaxt = "n")
axis(side = 1, at = c(1,8), labels = c("no agreement","total agreement"))


### nonlinear PCA on chronic widespread pain data 
# load example data 
data(ICFCoreSetCWP)

# adequate coding to get levels 1,..., max 
H <- ICFCoreSetCWP[, 1:67] + matrix(c(rep(1, 50), rep(5, 16), 1),
                                    nrow(ICFCoreSetCWP), 67, 
                                    byrow = TRUE)

# nonlinear PCA
ordPCA(H, p = 2, lambda = 0.5, maxit = 1000,
       Ks = c(rep(5, 50), rep(9, 16), 5), 
       constr = c(rep(TRUE, 50), rep(FALSE, 16), TRUE))


# k-fold cross-validation
set.seed(1234)
lambda <- 10^seq(4,-4, by = -0.1) 
cvResult1 <- ordPCA(H, p = 2, lambda = lambda, maxit = 100,
       Ks = c(rep(5, 50), rep(9, 16), 5), 
       constr = c(rep(TRUE, 50), rep(FALSE, 16), TRUE),
       CV = TRUE, k = 5)
            
# optimal lambda                    
lambda[which.max(apply(cvResult1$VAFtest,2,mean))]                      
}
}

