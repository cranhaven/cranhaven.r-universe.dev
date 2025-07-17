\name{JGL}
\alias{JGL}

\title{
Joint Graphical Lasso
}
\description{
Solve the Joint Graphical Lasso
}
\usage{
JGL(Y,penalty="fused",lambda1,lambda2,rho=1,weights="equal",penalize.diagonal=FALSE,
maxiter=500,tol=1e-5,warm=NULL,return.whole.theta=FALSE, screening="fast",
truncate = 1e-5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
A list of nXp data matrices.   
}
  \item{penalty}{
Determines whether lambda2 controls a "fused" or "group" lasso penalty.  Must take value "fused" or "group".
}
  \item{lambda1}{
The tuning parameter for the graphical lasso penalty.
}
  \item{lambda2}{
The tuning parameter for the fused or group lasso penalty.
}
  \item{rho}{
A step size parameter.  Large values decrease step size.
}
  \item{weights}{
Determines the putative sample size of each class's data.  Allowed values: a vector with 
length equal to the number of classes; "equal", giving each class weight 1; "sample.size",
giving each class weight corresponding to its sample size.
}
  \item{penalize.diagonal}{
If penalty=="fused", determines whether lambda1 is applied to the diagonal of theta.
If penalty=="group", determines whether lambda1 and lambda2 are applied to the diagonal of theta.
}
  \item{maxiter}{
Maximum number of iterations.
}
  \item{tol}{
Determines convergence criterion.  
}
  \item{warm}{
Input a warm start to theta in the form of a K-length list of pXp matrices.
}
  \item{return.whole.theta}{
If TRUE, each class's inverse covariance matrix is returned whole.  If FALSE, the inverse covariance
matrix is only returned over the connected nodes, and only the diagonal of the matrix is returned over
the unconnected nodes.  
}
  \item{screening}{
"fast" or "memory.efficient".  Use of "fast" is recommended unless the number of features prohibits
storage of a pXp matrix.  For very high dimension data, screening="memory.efficient" will allow a 
solution with a much longer computation time.
}
  \item{truncate}{
Defaults to 1e-5.  At convergence, all values of theta below this number will be set to zero.  
}
}
\details{
This function can solve both the Fused Graphical Lasso and the Group Graphical Lasso.  
}
\value{
\item{theta}{A list of the estimated inverse covariance matrices, over all nodes if 
return.whole.theta==TRUE and over only the connected nodes if return.whole.theta==FALSE}
\item{diag.theta.unconnected}{Returned only if return.whole.theta==FALSE.  A list of vectors, 
 each vector the estimated diagonal of an inverse covariance matrix over the unconnected nodes.}
\item{connected}{A logical vector identifying whether each node is connected.}
}
\references{
Patrick Danaher, Pei Wang and Daniela Witten (2011).  The joint graphical lasso for inverse covariance 
estimation across multiple classes.  http://arxiv.org/abs/1111.0324

}
\author{
Patrick Danaher
}

\examples{
## load an example dataset with K=two classes, p=200 features, and n=100 samples per class:
data(example.data)
str(example.data)
## run fgl:
fgl.results = JGL(Y=example.data,penalty="fused",lambda1=.25,lambda2=.1)
str(fgl.results)
print.jgl(fgl.results)
## run ggl:
ggl.results = JGL(Y=example.data,penalty="group",lambda1=.15,lambda2=.2,return.whole.theta=TRUE)
str(ggl.results)
print.jgl(ggl.results)
}

