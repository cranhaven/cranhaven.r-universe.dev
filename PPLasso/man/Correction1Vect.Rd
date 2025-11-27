\name{Correction1Vect}
\alias{Correction1Vect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correction on two vectors 
}
\description{
For the estimation of \eqn{\beta} in  Zhu et al. (2022), this function keeps only the M largest values coefficientss set the others to 0. 
}
\usage{
Correction1Vect(X, Y, te = NULL, vector, top_grill. = c(1:length(vector)), delta = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{X}{
Design matrix
}
  \item{Y}{
Response vector
  }
  \item{te}{
treatment effects
}
  \item{vector}{
The vector on which we performe the thresholding
}
  \item{top_grill.}{
grill of the thresholding
}
  \item{delta}{
parameter \eqn{\delta} in the thresholding
}
}

\value{
This function returns the thresholded vector.
}

\author{
Wencan Zhu, Celine Levy-Leduc, Nils Ternes
}


\examples{
vect_sample=sample(1:20,20)
X=t(sapply(c(1:10),FUN=function(x) rnorm(20)))
Y=rnorm(10)

Correction1Vect(X=X, Y=Y, vector=vect_sample)

## The function is currently defined as
function(X, Y, te=NULL, vector, top_grill.=c(1:length(vector)), delta=0.95){
  
  beta_interm <- sapply(top_grill., top, vect = vector)
  beta_te <- rbind(rep(te[1],length(top_grill.)), rep(te[2],length(top_grill.)), beta_interm)
  yhat <- as.matrix(X \%*\% beta_te)
  residuals <- sweep(yhat, 1, Y)
  mse_final_top <- colMeans(residuals^2)
  ratio_mse <- c()
  for (k in 1:(length(top_grill.) - 1)) {
    ratio_mse[k] <- round(mse_final_top[k + 1]/mse_final_top[k],6)
  }
  top_ratio <- min(which(ratio_mse >= delta))
  if (is.infinite(top_ratio)) {
    opt_final_top <- length(vector)
  }
  else {
    opt_final_top <- top_grill.[top_ratio]
  }
  
  return(round(top(vect = vector, thresh = opt_final_top), 6))
  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~~thresholding }% use one of  RShowDoc("KEYWORDS")