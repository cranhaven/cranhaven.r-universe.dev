\name{plot.ecpc}
\alias{plot.ecpc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot an 'ecpc' object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Make a plot of the fitted regression coefficients versus their corresponding fitted
prior variances, or fit the prior variance weight contribution of each co-data source.
}
\usage{
\method{plot}{ecpc}(x, show = c("coefficients", "priorweights"), 
     Z = NULL, values = NULL, groupsets = NULL,
     codataweights=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
An 'ecpc' object returned by \code{\link{ecpc}}.
}
  \item{show}{
%%     ~~Describe \code{show} here~~
Either "coefficients" or "priorweights" to show the fitted regression coefficients
or the prior variances. To plot the prior variances, co-data should be provided 
in either 'Z' or 'groupsets'.
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
List of m co-data matrices, as in \code{\link{ecpc}}.
}
  \item{values}{
%%     ~~Describe \code{values} here~~
List of m elements, containing p-dimensinal vectors with continuous co-data values or NULL.
If provided, the prior variances will be plotted versus the provided continuous co-data.
If NULL, the prior variances will be plotted per co-data variable.
}
  \item{groupsets}{
%%     ~~Describe \code{groupsets} here~~
Co-data provided as list of group sets, as in \code{\link{ecpc}}.
}
  \item{codataweights}{
%%     ~~Describe \code{groupsets} here~~
For the option `show="priorweights"', should the prior variances include the co-data source weights?
}
\item{\dots}{...}
}
\value{
If the packages `ggplot2' and `ggpubr' are installed, a `ggplot' object is shown and returned, else a base plot is shown.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
See \code{\link{ecpc}} for model fitting.
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

values <- list(NULL, continuousCodata, NULL)

plot(fit, show="coefficients")
plot(fit, show="priorweights", Z=Z.all, values=values)
}

}

