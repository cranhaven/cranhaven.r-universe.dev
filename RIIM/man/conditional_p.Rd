\name{conditional_p}
\alias{conditional_p}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The function for calculating the post-matching treatment assignment probabilities}
\description{
This function calculates the regularized post-matching treatment assignment probabilities, as described in Zhu, Zhang, Guo, and Heng (2024).
}
\usage{
conditional_p(treated.subject.index,matched.control.subject.index,p,alpha=0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{treated.subject.index}{The index list for treated subjects after matching.}
  \item{matched.control.subject.index}{The index list for control subjects after matching.}
  \item{p}{The estimated propensity score vector.}
  \item{alpha}{Prespecified small number as the regularization threshold. The default is 0.1.}

}

\value{
  \item{prob}{The regularized post-matching treatment assignment probabilities vector.}


}
\references{
Zhu, J., Zhang, J., Guo, Z., and Heng, S. (2024). Randomization-Based Inference for Average Treatment Effect in Inexactly Matched Observational Studies. arXiv:2308.02005.
}
\author{
Jianan Zhu (maintainer), Jeffrey Zhang, Zijian Guo and Siyu Heng.
}

\examples{
library(MASS)
library(xgboost)
library(optmatch)

# Generate data
set.seed(1)
d = 5
n = 50
sigma = diag(d)

# Generate X
X_d = mvtnorm::rmvnorm(n, mean = rep(0,d), sigma = sigma)
X_d[,4] = VGAM::rlaplace(n, location = 0, scale = sqrt(2)/2)
X_d[,5] = VGAM::rlaplace(n, location = 0, scale = sqrt(2)/2)

# Generate Z
C = -2.5 
fx = 0.1*(X_d[,1])^3 + 0.3*(X_d[,2]) + 0.2*log((X_d[,3])^2) + 0.1*(X_d[,4]) +  
     0.2*X_d[,5] + abs(X_d[,1]*X_d[,2]) + (X_d[,3]*X_d[,4])^2 + 0.5*(X_d[,2]*X_d[,4])^2 +  
     rnorm(n,0,1) + C
p = exp(fx)/(1+exp(fx)) # the probability of receiving the treatment
Z = rep(0,length(p))
for(i in seq_along(p)){
  Z[i] = rbinom(1,1,p[i])
}

# Generate Y 
Y_0 = 0.2*(X_d[,1])^3 + 0.2*abs(X_d[,2]) + 0.2*X_d[,3]^3 + 0.5*abs(X_d[,4]) +   
0.3*X_d[,5] + rnorm(n,0,1)
Y_1 = Y_0 + 1 + 0.3*X_d[,1] + 0.2*X_d[,3]^3
Y = (1-Z)*Y_0 + Z*Y_1

# Smahal function
  smahal=
    function(z,X){
      X<-as.matrix(X)
      n<-dim(X)[1]
      rownames(X)<-1:n
      k<-dim(X)[2]
      m<-sum(z)
      for (j in 1:k) X[,j]<-rank(X[,j])
      cv<-cov(X)
      vuntied<-var(1:n)
      rat<-sqrt(vuntied/diag(cv))
      cv<-diag(rat)\%*\%cv\%*\%diag(rat)
      out<-matrix(NA,m,n-m)
      Xc<-X[z==0,]
      Xt<-X[z==1,]
      rownames(out)<-rownames(X)[z==1]
      colnames(out)<-rownames(X)[z==0]
      icov<-MASS::ginv(cv)
      for (i in 1:m) out[i,]<-mahalanobis(Xc,Xt[i,],icov,inverted=TRUE)
      out
    }
  
  # Matching
  treated.index = which(Z == 1)
  propscore.model = glm(Z ~ X_d, family = 'binomial',x=TRUE,y=TRUE)
  treated = propscore.model$y
  Xmat=propscore.model$x[,-1]
  distmat=smahal(treated,Xmat)
  logit.propscore=predict(propscore.model)
  subject.index=seq(1,length(treated),1)
  rownames(distmat)=subject.index[treated==1]
  colnames(distmat)=subject.index[treated==0]
  matchvec=fullmatch(distmat,min.controls=0.0001,max.controls=10000)
  treated.subject.index=vector("list",length(treated.index))
  matched.control.subject.index=vector("list",length(treated.index))
  matchedset.index=substr(matchvec,start=3,stop=10)
  matchedset.index.numeric=as.numeric(matchedset.index)
  subjects.match.order=as.numeric(names(matchvec))
  matchedset_index = length(unique(matchedset.index.numeric))
  
  # total number in each set
  l <- rep(0,length(treated.subject.index))
  for(i in 1:length(treated.subject.index)){
    matched.set.temp=which(matchedset.index.numeric==i)
    matched.set.temp.indices=subjects.match.order[matched.set.temp]
    l[i] <- length(matched.set.temp.indices)
  }
  
  # the order of matchvec
  for(i in 1:length(treated.index)){
  matched.set.temp=which(matchedset.index.numeric==i)
  matched.set.temp.indices=subjects.match.order[matched.set.temp]
  treated.temp.index=which(matched.set.temp.indices \%in\% treated.index)
  if(length(treated.temp.index) != 0){
    treated.subject.index[[i]]=matched.set.temp.indices[treated.temp.index]
    matched.control.subject.index[[i]]=matched.set.temp.indices[-treated.temp.index]
  }
}
  
  # remove null
  if(sum(sapply(treated.subject.index, is.null)) != 0){
    treated.subject.index<- treated.subject.index[-which(sapply(treated.subject.index, is.null))]
    matched.control.subject.index<-matched.control.subject.index[-which(sapply(   
    matched.control.subject.index,is.null))]
  }

# Use XGBoost to estimate propensity score
  length_all = length(Z)
  length_X = ncol(X_d)
  df = data.frame(Z,X_d)
  index_model1 = sample(length_all,length_all/2)
  df1 = df[index_model1,]
  df2 = df[-index_model1,]
  prob = rep(0,length_all)
  xgb.model1 = xgboost(data = as.matrix(df1[2:length_X]), label = df1$Z, nrounds = 2,   
  objective = "binary:logistic",verbose = 0)
  prob[-index_model1] = predict(xgb.model1, as.matrix(df2[2:length_X]))
  xgb.model2 = xgboost(data = as.matrix(df2[2:length_X]), label = df2$Z, nrounds = 2,   
  objective = "binary:logistic",verbose = 0)
  prob[index_model1] = predict(xgb.model2, as.matrix(df1[2:length_X]))
  
  # calculate the post-matching treatment assignment probabilities
  p = conditional_p(treated.subject.index,matched.control.subject.index,prob,alpha=0.1)
}

