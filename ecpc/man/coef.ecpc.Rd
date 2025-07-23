\name{coef.ecpc}
\alias{coef.ecpc}
\alias{penalties}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Obtain coefficients from 'ecpc' object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Obtain regression coefficients or penalties from an existing model fit given in an 'ecpc' object,
re-estimate regression coefficients for a given 'ecpc' object and ridge
penalties, or obtain ridge penalties for given prior parameters and co-data.

}
\usage{
\method{coef}{ecpc}(object, penalties = NULL, 
          X = NULL, Y = NULL, 
          unpen = NULL, intrcpt = TRUE, 
          model = c("linear", "logistic", "cox"), 
          est_beta_method = c("glmnet", "multiridge"), \dots)

penalties(object, tauglobal=NULL, sigmahat=NULL, gamma=NULL, gamma0=NULL, w=NULL,
          Z=NULL, groupsets=NULL,
          unpen=NULL, datablocks=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{x} here~~
An 'ecpc' object returned by \code{\link{ecpc}}.
}
  \item{penalties}{
%%     ~~Describe \code{penalties} here~~
Ridge penalties; p-dimensional vector. If provided to \code{coef.ecpc}, 'X' and 'Y' should be provided too.
}
  \item{tauglobal}{
  Estimated global prior variance; scalar (or vector with datatype-specific global prior variances when multiple `datablocks' are given).) If provided to \code{penalties}, 'Z' or 'groupsets' should be provided too.
  }
  \item{sigmahat}{(linear model) Estimated sigma^2. If provided to \code{penalties}, 'Z' or 'groupsets' should be provided too.}
  \item{gamma}{Estimated co-data variable weights; vector of dimension the total number of groups. If provided to \code{penalties}, 'Z' or 'groupsets' should be provided too.}
  \item{gamma0}{Estimated co-data variable intercept; scalar. If provided to \code{penalties}, 'Z' or 'groupsets' should be provided too.}
  \item{w}{Estimated group set weights; m-dimensional vector. If provided to \code{penalties}, 'Z' or 'groupsets' should be provided too.}
  \item{X}{
%%     ~~Describe \code{X} here~~
Observed data; (nxp)-dimensional matrix (p: number of covariates) with each row the observed high-dimensional feature vector of a sample.
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
Response data; n-dimensional vector (n: number of samples) for linear and logistic outcomes, or \code{\link{Surv}} object for Cox survival.
}
\item{Z}{
 List with m co-data matrices. Each element is a (pxG)-dimensional co-data matrix containing co-data on the p   variables. Co-data should either be provided in `Z' or `groupsets'. 
}
  \item{groupsets}{
%%     ~~Describe \code{groupsets} here~~
Co-data group sets; list with m (m: number of group sets) group sets. Each group set is a list of all groups in that set. Each group is a vector containing the indices of the covariates in that group.
}
  \item{unpen}{
%%     ~~Describe \code{unpen} here~~
Unpenalised covariates; vector with indices of covariates that should not be penalised.
}
  \item{intrcpt}{
%%     ~~Describe \code{intrcpt} here~~
Should an intercept be included? Included by default for linear and logistic, excluded for Cox for which the baseline hazard is estimated.
}
  \item{model}{
%%     ~~Describe \code{model} here~~
Type of model for the response; linear, logistic or cox.
}
  \item{est_beta_method}{
  Package used for estimating regression coefficients, either "glmnet" or "multiridge".
}
  \item{datablocks}{
%%     ~~Describe \code{normalise} here~~
(optional) for multiple data types, the corresponding blocks of data may be given in datablocks; a list of B vectors of the indices of covariates in `X' that belong to each of the B data blocks. Unpenalised covariates should not be given as seperate block, but can be omitted or included in blocks with penalised covariates. Each datatype obtains a datatype-specific `tauglobal' as in multiridge.
}
\item{\dots}{Other parameters}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
For \code{coef.ecpc}, a list with:
\item{intercept}{
  If included, the estimated intercept; scalar.
} 
\item{beta}{
  Estimated regression coefficients; p-dimensional vector.
} 

For \code{penalties}: a p-dimensional vector with ridge penalties.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{penalties}} for obtaining penalties for given prior parameters and co-data.
}
\examples{
\donttest{ 
#####################
# Simulate toy data #
#####################
p<-300 #number of covariates
n<-100 #sample size training data set
n2<-100 #sample size test data set

#simulate all betas i.i.d. from beta_k~N(mean=0,sd=sqrt(0.1)):
muBeta<-0 #prior mean
varBeta<-0.1 #prior variance
indT1<-rep(1,p) #vector with group numbers all 1 (all simulated from same normal distribution)

#simulate test and training data sets:
Dat<-simDat(n,p,n2,muBeta,varBeta,indT1,sigma=1,model='linear') 
str(Dat) #Dat contains centered observed data, response data and regression coefficients

###################
# Provide co-data #
###################
continuousCodata <- abs(Dat$beta) 
Z1 <- cbind(continuousCodata,sqrt(continuousCodata))

#setting 2: splines for informative continuous
Z2 <- createZforSplines(values=continuousCodata)
S1.Z2 <- createS(orderPen=2, G=dim(Z2)[2]) #create difference penalty matrix
Con2 <- createCon(G=dim(Z2)[2], shape="positive+monotone.i") #create constraints

#setting 3: 5 random groups
G <- 5
categoricalRandom <- as.factor(sample(1:G,p,TRUE))
#make group set, i.e. list with G groups:
groupsetRandom <- createGroupset(categoricalRandom)
Z3 <- createZforGroupset(groupsetRandom,p=p)
S1.Z3 <- createS(G=G, categorical = TRUE) #create difference penalty matrix
Con3 <- createCon(G=dim(Z3)[2], shape="positive") #create constraints

#fit ecpc for the three co-data matrices with following penalty matrices and constraints
#note: can also be fitted without paraPen and/or paraCon
Z.all <- list(Z1=Z1,Z2=Z2,Z3=Z3)
paraPen.all <- list(Z2=list(S1=S1.Z2), Z3=list(S1=S1.Z3))
paraCon <- list(Z2=Con2, Z3=Con3)

############
# Fit ecpc #
############
tic<-proc.time()[[3]]
fit <- ecpc(Y=Dat$Y,X=Dat$Xctd,
           Z = Z.all, paraPen = paraPen.all, paraCon = paraCon,
           model="linear",maxsel=c(5,10,15,20),
           Y2=Dat$Y2,X2=Dat$X2ctd)
toc <- proc.time()[[3]]-tic

#estimate coefficients for twice as large penalties
new_coefficients <- coef(fit, penalties=fit$penalties*2, X=Dat$Xctd, Y=Dat$Y)

#change some prior parameters and find penalties
gamma2 <- fit$gamma; gamma2[1:3] <- 1:3
new_penalties <- penalties(fit, gamma=gamma2, Z=Z.all)
new_coefficients2 <- coef(fit, penalties=new_penalties, X=Dat$Xctd, Y=Dat$Y)
}

}
