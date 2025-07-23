\name{hierarchicalLasso}
\alias{hierarchicalLasso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Fit hierarchical lasso using LOG penalty
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Fits a linear regression model penalised with a hierarchical lasso penalty, using a latent overlapping group (LOG) lasso penalty.
}
\usage{
hierarchicalLasso(X, Y, groupset, lambda=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
nxp matrix with observed data
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
nx1 vector with response data
}
  \item{groupset}{
%%     ~~Describe \code{groupset} here~~
list with hierarchical group indices
}
  \item{lambda}{
%%     ~~Describe \code{groupset} here~~
Scalar. Penalty parameter for the latent overlapping group penalty.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The LOG penalty can be used to impose hierarchical constraints in the estimation of regression coefficients (Yan, Bien et al. 2007), e.g. a group of covariates (child node in the hierarchical tree) may be selected only if another group is selected (parent node in the hierarchical tree).
This function uses the simple implementation for the LOG penalty described in (Jacob, Obozinski and Vert, 2009). Faster and more scalable algorithms may be available but not yet used in this pacakage.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list with the following elements;
  \item{betas}{Estimated regression coefficients.}
  \item{a0}{Estimated intercept.}
  \item{lambdarange}{Range of penalty parameter used for CV (if lambda was not given).}
  \item{lambda}{Estimated penalty parameter.}
  \item{group.weights}{Fixed group weights used in the LOG-penalty.}
}
\references{
%% ~put references to the literature/web site here ~
Yan, X., Bien, J. et al. (2017). Hierarchical sparse modeling: A choice of two group lasso formulations. Statistical Science 32 531-560.

Jacob, L., Obozinski, G. and Vert, J.-P. (2009). Group lasso with overlap and graph lasso. In: Proceedings of the 26th annual international conference on machine learning 433-440. ACM.
}
\examples{
# Simulate toy data 
p<-60 #number of covariates
n<-30 #sample size training data set
n2<-100 #sample size test data set

#simulate all betas i.i.d. from beta_k~N(mean=0,sd=sqrt(0.1)):
muBeta<-c(0,0) #prior mean
varBeta<-c(0.0001,0.1) #prior variance
#vector with group numbers all 1 (all simulated from same normal distribution)
indT1<-rep(c(1,2),each=p/2)

#simulate test and training data sets:
Dat<-simDat(n,p,n2,muBeta,varBeta,indT1,sigma=1,model='linear')
str(Dat) #Dat contains centered observed data, response data and regression coefficients

#hierarchical grouping: e.g. covariates (p/4+1):(p/2) can only be selected when
#covariates 1:(p/4) are selected
groupset <- list(1:(p/2),(p/2+1):p,1:(p/4),(3*p/4+1):p)

#Fit hierarchical lasso, perform CV to find optimal lambda penalty
res <- hierarchicalLasso(X=Dat$Xctd,Y=Dat$Y,groupset = groupset )
res$lambdarange
plot(res$betas)

#Fit hierarchical lasso for fixed lambda
res2 <- hierarchicalLasso(X=Dat$Xctd,Y=Dat$Y,groupset = groupset,lambda=res$lambdarange[2] )
plot(res2$betas)
}
