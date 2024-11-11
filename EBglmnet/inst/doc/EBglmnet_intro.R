## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("EBglmnet", repos = "http://cran.us.r-project.org")

## -----------------------------------------------------------------------------
rm(list = ls())
library(EBglmnet)

## ---- eval=FALSE--------------------------------------------------------------
#  varNames = colnames(state.x77);
#  varNames
#  y= state.x77[,"Life Exp"]
#  xNames = c("Population","Income","Illiteracy", "Murder","HS Grad","Frost","Area")
#  x = state.x77[,xNames]

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(1)
#  output = EBglmnet(x,y,hyperparameters = c(0.1, 0.1))

## ---- eval=FALSE--------------------------------------------------------------
#  glmfit = output$fit
#  variables = xNames[glmfit[,1,drop=FALSE]]
#  cbind(variables,as.data.frame(round(glmfit[,2:5,drop=FALSE],4)))

## ---- eval=FALSE--------------------------------------------------------------
#  cvfit = cv.EBglmnet(x, y)

## ---- eval=FALSE--------------------------------------------------------------
#  cvfit$CrossValidation

## ---- eval=FALSE--------------------------------------------------------------
#  cvfit$hyperparameters
#  cvfit$fit

## ---- eval=FALSE--------------------------------------------------------------
#  output$Intercept
#  output$residual

## ---- eval=FALSE--------------------------------------------------------------
#  yy = y>mean(y);
#  output = EBglmnet(x,yy,family="binomial", hyperparameters = c(0.1, 0.1))

## ---- eval=FALSE--------------------------------------------------------------
#  output = EBglmnet(x,yy,family="binomial", prior = "elastic net", hyperparameters = c(0.1, 0.1))
#  output$fit

## ---- eval=TRUE---------------------------------------------------------------
data(BASIS)#this is the predictors matrix
N = nrow(BASIS)
p = ncol(BASIS)
j = sample((p-2),1)
cor(BASIS[,c(j,j+1,j+2)]) #Correlation structure among predictors 

## ---- eval=TRUE---------------------------------------------------------------
set.seed(1);
Mu = 100; #population mean;
nTrue = 10; # we assume 10 out of the 481 predictors are true causal ones
trueLoc	= sort(sample(p,nTrue));
trueEff = runif(nTrue,2,3); #effect size from 2-3
xbeta = BASIS[,trueLoc]%*%trueEff;
s2 =  var(xbeta)*0.1/0.9 #residual variance with 10% noise
residual = rnorm(N,mean=0,sd=sqrt(s2))
y = Mu + xbeta + residual;  

## ---- eval=FALSE--------------------------------------------------------------
#  n = 300;
#  index = sample(N,n);
#  CV = cv.EBglmnet(x=BASIS[index,],y=y[index],family="gaussian",prior= "lassoNEG",nfold= 5)

## ---- eval=FALSE--------------------------------------------------------------
#  CV$fit
#  trueLoc

## ---- eval=TRUE---------------------------------------------------------------
n = 300;
set.seed(1)
index = sample(nrow(BASIS),n)
p = ncol(BASIS);
m = p*(p+1)/2;
#1. simulate true causal effects
nMain = 10;
nEpis  = 10;
mainLoc = sample(p,nMain);
episLoc = sample(seq((p+1),m,1),nEpis);
trueLoc = sort(c(mainLoc,episLoc)); #a vector in [1,m]
nTrue = length(trueLoc);
trueLocs = ijIndex(trueLoc, p); #two columns denoting the pair (i,j)
#2. obtain true predictors
basis = matrix(0,n,nTrue);
for(i in 1:nTrue)
{
	if(trueLocs[i,1]==trueLocs[i,2])
	{
		basis[,i] = BASIS[index,trueLocs[i,1]]
	}else
	{
		basis[,i] = BASIS[index,trueLocs[i,1]]*BASIS[index,trueLocs[i,2]]
	}
}
#3. simulate true effect size	
trueEff  = runif(nTrue,2,3);
#4. simulate response variables
xbeta = basis%*%trueEff;
vary = var(xbeta);
Pr = 1/(1+ exp( -xbeta));
y = rbinom(n,1,Pr);


## -----------------------------------------------------------------------------
CV = cv.EBglmnet(x=BASIS[index,],y=y,family="binomial",prior="lasso",nfold=5)
ind = which(CV$fit[,5]<=0.1)#p-value cutoff
CV$fit[ind,]

## -----------------------------------------------------------------------------
X = matrix(0,n,m);
X[,1:p] = BASIS[index,];
kk = p + 1;
for(i in 1:(p-1))
{
	for(j in (i+1):p)
	{
		X[,kk] = BASIS[index,i] * BASIS[index,j];
		kk = kk + 1;
	}
}

## -----------------------------------------------------------------------------
library(glmnet);
alpha = 1
lambdaRatio = 1e-4; #same as in EBlasso
cv = cv.glmnet(X, y, alpha = alpha,family="binomial",nfolds = 5,lambda.min.ratio=lambdaRatio)
nLambda = length(cv$lambda)
nLambda
nbeta = rep(0,nLambda);
fit0 = cv$glmnet.fit;
for(i in 1:nLambda)
{
  nbeta[i] = length(which(fit0$beta[,i]!=0))
}
plot(nbeta,xlab=expression(paste(lambda, " in lasso selection path(n=300,p=115,921)")),
     ylab="No. of nonzero effects",xaxt="n")#
ticks = seq(1,nLambda,10)
axis(side=1, at= ticks,labels=round(cv$lambda[ticks],5), las=1,cex.axis = 0.5)
title("Number of nonzero effects in lasso selection path")

## -----------------------------------------------------------------------------
lambda= cv$lambda.min
coefs = fit0$beta
ind = which(cv$lambda==cv$lambda.min)
beta = coefs[,ind]
betaij = which(beta!=0)
Xdata = X[,betaij];
colnames(Xdata) = betaij; 
refit = glm(y ~ Xdata, family = binomial(link = "logit"))#separation occurs

