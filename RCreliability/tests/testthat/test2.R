library(RCreliability)
# Regression on only one covariates measured with error
x<-rnorm(3000,0,1)
#ICC=0.7 generate z
r<-c(rep(1,1500),rep(3,700),rep(4,800))
z<-list(rbind(cbind(x[1:1500]+rnorm(1500,0,sqrt(0.4)),NA,NA,NA),
              cbind(x[1501:2200]+rnorm(700,0,sqrt(0.4)),x[1501:2200]+rnorm(700,0,sqrt(0.4)),x[1501:2200]+rnorm(700,0,sqrt(0.4)),NA),
              cbind(x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)))))
#prevalence=0.105
p<-exp(-2.2+log(1.5)*x)/(1+exp(-2.2+log(1.5)*x))
Y<-sapply(p,function(x) rbinom(1,1,x))
fit1 <- RCreliability.in(r,z,W=NULL,Y)
fit1

# Regression on one covariates measured with error and one confounder
x<-rnorm(3000,0,1)
#ICC=0.7 generate z
r<-c(rep(1,1500),rep(3,700),rep(4,800))
z<-list(rbind(cbind(x[1:1500]+rnorm(1500,0,sqrt(0.4)),NA,NA,NA),
              cbind(x[1501:2200]+rnorm(700,0,sqrt(0.4)),x[1501:2200]+rnorm(700,0,sqrt(0.4)),x[1501:2200]+rnorm(700,0,sqrt(0.4)),NA),
              cbind(x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)))))
W<-sapply(x, function(t){if(t>median(x)) {return(rbinom(1,1,0.5))}
  if(t<=median(x)){return(rbinom(1,1,0.3))}})
#prevalence about 0.104
p<-exp(-2.4+log(1.5)*x+log(1.5)*W)/(1+exp(-2.4+log(1.5)*x+log(1.5)*W))
Y<-sapply(p,function(x) rbinom(1,1,x))
fit2<-RCreliability.in(r,z,W=W,Y)
fit2

# Regression on two covariates measured with error and no confounder
x<-mgcv::rmvn(3000,c(0,0),matrix(c(1,0.3,0.3,1),nrow=2))
#ICC=0.7 generate z
r<-c(rep(1,1500),rep(2,500),rep(3,400),rep(4,600))
z<-list(rbind(cbind(x[1:1500,1]+rnorm(1500,0,sqrt(0.4)),NA,NA,NA),
              cbind(x[1501:1500,1]+rnorm(500,0,sqrt(0.4)),x[1501:1500,1]+rnorm(500,0,sqrt(0.4)),NA,NA),
              cbind(x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),NA),
              cbind(x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)))),
        rbind(cbind(x[1:1500,2]+rnorm(1500,0,sqrt(0.4)),NA,NA,NA),
              cbind(x[1501:1500,2]+rnorm(500,0,sqrt(0.4)),x[1501:1500,2]+rnorm(500,0,sqrt(0.4)),NA,NA),
              cbind(x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),NA),
              cbind(x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)))))
#prevalence about 0.105
p<-exp(-2.3+log(1.5)*rowSums(x))/(1+exp(-2.3+log(1.5)*rowSums(x)))
Y<-sapply(p,function(x) rbinom(1,1,x))
fit3<-RCreliability.in(r,z, W=NULL,Y)
fit3


# Regression on two covariates measured with error and two confounders
x<-mgcv::rmvn(3000,c(0,0,0),matrix(c(1,0.3,0.2,0.3,1,0.5,0.2,0.5,1),nrow=3))
w2<-sapply(x[,1], function(t){if(t>median(x[,1])) {return(rbinom(1,1,0.5))}
  if(t<=median(x[,1])){return(rbinom(1,1,0.3))}})
#ICC=0.7 generate z
r<-c(rep(1,1500),rep(2,500),rep(3,400),rep(4,600))
W<-cbind(x[,3],w2)
z<-list(rbind(cbind(x[1:1500,1]+rnorm(1500,0,sqrt(0.4)),NA,NA,NA),
              cbind(x[1501:1500,1]+rnorm(500,0,sqrt(0.4)),x[1501:1500,1]+rnorm(500,0,sqrt(0.4)),NA,NA),
              cbind(x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),NA),
              cbind(x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)))),
        rbind(cbind(x[1:1500,2]+rnorm(1500,0,sqrt(0.4)),NA,NA,NA),
              cbind(x[1501:1500,2]+rnorm(500,0,sqrt(0.4)),x[1501:1500,2]+rnorm(500,0,sqrt(0.4)),NA,NA),
              cbind(x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),NA),
              cbind(x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)))))
#prevalence about 0.104
p<-exp(-2.65+log(1.5)*rowSums(x[,1:3])+log(1.5)*w2)/(1+exp(-2.65+log(1.5)*rowSums(x[,1:3])+log(1.5)*w2))
Y<-sapply(p,function(x) rbinom(1,1,x))
fit4<-RCreliability.in(r,z,W=W,Y)
fit4

# Regression on two covariates measured with error and one confounders
x<-mgcv::rmvn(3000,c(0,0,0),matrix(c(1,0.3,0.2,0.3,1,0.5,0.2,0.5,1),nrow=3))
#ICC=0.7 generate z
r<-c(rep(1,1500),rep(2,500),rep(3,400),rep(4,600))
W<-cbind(x[,3])
z<-list(rbind(cbind(x[1:1500,1]+rnorm(1500,0,sqrt(0.4)),NA,NA,NA),
              cbind(x[1501:1500,1]+rnorm(500,0,sqrt(0.4)),x[1501:1500,1]+rnorm(500,0,sqrt(0.4)),NA,NA),
              cbind(x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),NA),
              cbind(x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)))),
        rbind(cbind(x[1:1500,2]+rnorm(1500,0,sqrt(0.4)),NA,NA,NA),
              cbind(x[1501:1500,2]+rnorm(500,0,sqrt(0.4)),x[1501:1500,2]+rnorm(500,0,sqrt(0.4)),NA,NA),
              cbind(x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),NA),
              cbind(x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)))))
#prevalence about 0.104
p<-exp(-2.65+log(1.5)*rowSums(x[,1:3]))/(1+exp(-2.65+log(1.5)*rowSums(x[,1:3])))
Y<-sapply(p,function(x) rbinom(1,1,x))
fit5<-RCreliability.in(r,z,W=W,Y)
fit5
