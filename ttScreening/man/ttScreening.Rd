\name{ttScreening}
\alias{ttScreening}
\title{
 A screening process built upon training and testing samples
}
\description{
 A screening process to filter out non-informative DNA methylation sites by applying (ordinary or robust) linear regressions to training data, and the results are further examined using testing samples. Surrogate variables are included to account for unknown factors.
}
\usage{
ttScreening(y = y, formula, imp.var, data, B.values=FALSE,iterations = 100, 
sva.method = c("two-step", "irw"), cv.cutoff = 50, n.sv = NULL, 
train.alpha = 0.05, test.alpha = 0.1, FDR.alpha = 0.05, Bon.alpha = 0.05,
percent = (2/3),linear = c("robust", "ls"), vfilter = NULL, B = 5,
 numSVmethod = "be", rowname = NULL, maxit=20)
}

\arguments{
  \item{y}{
    Data matrix of DNA methylation measures (m by n, m CpG sites and n subjects). Each column represents DNA methylation measures of all CpG sites for one subject.
}
  \item{formula}{
     An object of class \code{\link{formula}} (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under "Details".
}
  \item{imp.var}{
    A vector indicating the location of the term(s) in the formula option on which the selection of CpG sites are made. Interactions are considered a single term. For example, suppose the right-hand side of the equation is: x + z + x:z. If the decision of selecting a CpG site is based on one single term, e.g., the significance of interaction effect, then imp.var is set as the location of that term, e.g., imp.var=3 (the third term). If the decision is desired to base on all the three terms, then imp.var=c(1,2,3).
}
  \item{data}{
	Data frame created from model.frame. Also is the data frame containing the variables defined in formula.
}
  \item{B.values}{
	Logical, TRUE if indicating the methylation is measured as beta values, FALSE if methylation is measured as M-values. The default is FALSE.
}
  \item{iterations}{
   Number of loops for the training/testing (TT) procedure. The default is 100.
}
  \item{sva.method}{
	Option of the two surrogate variable estimation algorithms, the iteratively re-weighted, \code{"irw"}, or two-step, \code{"two-step"}. The default is \code{"two-step"}.
}
  \item{cv.cutoff}{
    The minimum frequency required for a DNA methylation site to be treated as an informative site. After "iterations" iterations, the frequency of each DNA methylation being selected out of \code{"iterations"} iterations is recorded. The higher the frequency, the more likely the site is informative. The default is 50.
}
  \item{n.sv}{
  Number of surrogate variables. If NULL, the number of surrogate variables will be determined based on the data. The default is NULL.
}
  \item{train.alpha}{
     Significance level for training samples. The default is 0.05.
}
  \item{test.alpha}{
  Significance level for testing samples. The default is 0.05.
}
  \item{FDR.alpha}{
    False discovery rate. The default is 0.05. This is to fit the need of selecting variables based on FDR.
}
  \item{Bon.alpha}{
    Overall significance level by use of the Bonferroni method for mulitple testing correction. The default is 0.05. This is to fit the need of selecting variables based on the Bonferroni multiple testing correction.
}
  \item{percent}{
     Proportion of the full sample to be used for training. The default is 2/3.
}
  \item{linear}{
	Choice of linear regression methods, \code{"robust"} (robust regression) or \code{"ls"} (ordinary least squares). The default is \code{"ls"}.
}
  \item{vfilter}{
    The number of most variable CpG sites to use when building SVs, must be between
100 and the number of genes; Must be NULL or numeric (> 0), The default is NULL. 
}
  \item{B}{
   Number of iterations in generating surrogate variables. The default is 5.
}
  \item{numSVmethod}{
   The method for determining the number of surrogate variables to use. The default is \code{"be"}, the other method is \code{"leek"}.
}
  \item{rowname}{
 Optional, NULL or \code{"TRUE"}. The default is NULL. If rownames are not already present within the data, the order in which the DNA methylation sites are listed will become the rowname. Surrogate variable estimates are formed based on the algorithms in Leek and Storey (2007).
}
 \item{maxit}{
 Optional, controls the number of iterations for linear regression estimation methods. The default is 20.
}

}
\details{
See \cite{\link{lm}} or \cite{\link{glm}} for details.
}
\value{
 \item{sub.remove}{
Denotes which subjects (based on order) were removed due to incomplete or missing data within the prediction variables defined in the formula arguement.
}
 \item{train.cpg}{
Number of DNA methylation sites selected after the training step of each loop.
}
  \item{test.cpg}{
Number of DNA methylation sites selected after the testing step of each loop.
}
  \item{selection}{
Indicator matrix for the TT method after the testing step. The number of rows is the number of methylation sites, and the number of columns is the number of iterations. An entry of 1 indiates the selection of a site, and 0 otherwise.
}
  \item{pvalue.matrix}{
Matrix of p-values of the selected DNA methylation sites after the testing step. The number of rows is the number of methylation sites and the number of columns is the number of iterations. For methylation sites not selected, NA is listed.
}
  \item{TT.cpg}{
Final list of the DNA methylation sites by their original rownames selected from the TT method.
}
  \item{FDR.cpg}{
Final list of the DNA methylation sites by their original rownames selected from the FDR method.
}
  \item{Bon.cpg}{
Final list of the DNA methylation sites by their original rownames selected from the Bonferroni method.
}
  \item{SV.output}{
Data frame containing the estimated surrogate variables.
}
  \item{TT.output}{
Data frame containing the list of DNA methylation sites selected from the TT method and the respective coefficients and pvalues for the variables and SVs.
}
  \item{FDR.output}{
Data frame containing the list of DNA methylation sites selected from the FDR method and the respective coefficients and pvalues for the variables and SVs.
}
  \item{Bon.output}{
Data frame containing the list of DNA methylation sites selected from the Bonferroni method and the respective coefficients and pvalues for the variables and SVs.
}
}

\references{
Meredith Ray, Xin Tong, Hongmei Zhang, and Wilfred Karmaus. (2014) "DNA methylation sites screening with surrogate variables", unpublished manuscript.

Leek JT and Storey JD. (2007) "Capturing heterogeneity in gene expression studies by `Surrogate Variable Analysis'." PLoS Genetics, 3: e161.
}

\examples{
\dontrun{
library(mvtnorm)
nsub=600
imp=100
num=2000

set.seed(1)
x1= rnorm(nsub,1,1)
size1<-rmultinom(1,nsub,c(0.15,0.25,0.25,0.35))
x2= matrix(sample(c(rep(0,size1[1,]),
			rep(1,size1[2,]),
			rep(2,size1[3,]),
			rep(3,size1[4,])),replace=F),byrow=250,ncol=1)
			
sur1<-rnorm(nsub,0,5)
sur2<-rnorm(nsub,3,1)
sur3<-rnorm(nsub,0,1)
sur4<-rnorm(nsub,2,4)
sur5<-rnorm(nsub,0,3)

sigma1<-matrix(0,nrow=num,ncol=num)
diag(sigma1)<-1.5

beta0<-0.5
beta1<-0.3
beta2<-0.3
beta3<-0.3

sbeta1<-rnorm(1,0.5,0.01)
sbeta2<-rnorm(1,0.5,0.01)
sbeta3<-rnorm(1,0.5,0.01)
sbeta4<-rnorm(1,0.5,0.01)
sbeta5<-rnorm(1,0.5,0.01)

#beta matrix#
beta<-as.matrix(cbind(beta0,beta1,beta2,beta3,sbeta1,sbeta2,sbeta3,sbeta4,sbeta5))
beta.no2<-as.matrix(cbind(beta0,beta1,beta3,sbeta1,sbeta2,sbeta3,sbeta4,sbeta5))
beta.sur<-as.matrix(cbind(sbeta1,sbeta2,sbeta3,sbeta4,sbeta5))
#design matrix#
X<-as.matrix(cbind(rep(1,length(x1)),x1,x2,x1*x2,sur1,sur2,sur3,sur4,sur5))
X.no2<-as.matrix(cbind(rep(1,length(x1)),x1,x1*x2,sur1,sur2,sur3,sur4,sur5))
X.sur<-as.matrix(cbind(sur1,sur2,sur3,sur4,sur5))
#mu matrix#
imp1.mu<-matrix(rep(X\%*\%t(beta),9),nrow=nsub,ncol=(imp*0.9))
imp2.mu<-matrix(rep(X.no2\%*\%t(beta.no2),1),nrow=nsub,ncol=(imp*0.1))
noimp.mu<-matrix(rep(X.sur\%*\%t(beta.sur),num-imp),nrow=nsub,ncol=num-imp)
mu.matrix=cbind(imp1.mu, imp2.mu, noimp.mu)
error<-rmvnorm(nsub,mean=rep(0,num),sigma=sigma1,method = "chol")
y<-t(mu.matrix+error)


runs<-ttScreening(y=y,formula=~x1+x2+x1:x2,imp.var=3,data=data.frame(x1,x2),sva.method="two-step",
			B.values=FALSE,iterations=100,cv.cutoff=50,n.sv=NULL,train.alpha=0.05,
			test.alpha=0.05,FDR.alpha=0.05,Bon.alpha=0.05,percent=(2/3),linear= "ls",
			vfilter = NULL, B = 5, numSVmethod = "be",rowname=NULL,maxit=20)

runs$TT.output
runs$FDR.output
runs$Bon.output
}
}
