\name{predict.ecpc}
\alias{predict.ecpc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Predict for new samples for `ecpc' object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Predict the response for new samples based on an `ecpc' object.
}
\usage{
\method{predict}{ecpc}(object, X2, X=NULL, Y=NULL, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{x} here~~
An 'ecpc' object returned by \code{\link{ecpc}}.
}
  \item{X2}{
%%     ~~Describe \code{X2} here~~
Independent observed data for which response is predicted.
}
  \item{X}{
%%     ~~Describe \code{X} here~~
Observed data used in fitting the `object'; (nxp)-dimensional matrix (p: number of covariates) with each row the observed high-dimensional feature vector of a sample.
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
Response data used in fitting the `object'; n-dimensional vector (n: number of samples) for linear and logistic outcomes, or \code{\link{Surv}} object for Cox survival.
}
\item{\dots}{Other parameters}
}
\value{
Vector with predicted values. Note that for Cox response, the relative risks are provided, unless training data X and Y is provided to compute the Breslow estimator.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

predictions <- predict(fit, X2=Dat$X2ctd)
}
}
