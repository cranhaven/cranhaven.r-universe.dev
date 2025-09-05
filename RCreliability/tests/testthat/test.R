library(RCreliability)
# Regression on only one covariates measured with error
x<-rnorm(3000,0,1)
#ICC=0.7 generate z
z.main <- matrix(x[1:1500]+rnorm(1500,0,sqrt(0.4)))
r<-c(rep(3,700),rep(4,800))
z.rep<-list(rbind(cbind(x[1501:2200]+rnorm(700,0,sqrt(0.4)),x[1501:2200]+rnorm(700,0,sqrt(0.4)),x[1501:2200]+rnorm(700,0,sqrt(0.4)),NA),
                  cbind(x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)))))
#prevalence about 0.105
p<-exp(-2.2+log(1.5)*x[1:1500])/(1+exp(-2.2+log(1.5)*x[1:1500]))
Y<-sapply(p,function(x) rbinom(1,1,x))
fit1 <- RCreliability.ex(z.main,r,z.rep,W=NULL,Y)
fit1

# Regression on one covariates measured with error and one confounder
x<-rnorm(3000,0,1)
#ICC=0.7 generate z
z.main <- matrix(x[1:1500]+rnorm(1500,0,sqrt(0.4)))
r<-c(rep(3,700),rep(4,800))
z.rep<-list(rbind(cbind(x[1501:2200]+rnorm(700,0,sqrt(0.4)),x[1501:2200]+rnorm(700,0,sqrt(0.4)),x[1501:2200]+rnorm(700,0,sqrt(0.4)),NA),
                  cbind(x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)),x[2201:3000]+rnorm(800,0,sqrt(0.4)))))
W<-matrix(sapply(x[1:1500], function(t){if(t>median(x)) {return(rbinom(1,1,0.5))}
  if(t<=median(x)){return(rbinom(1,1,0.3))}}))
#prevalence about 0.103
p<-exp(-2.4+log(1.5)*x[1:1500]+log(1.5)*W)/(1+exp(-2.4+log(1.5)*x[1:1500]+log(1.5)*W))
Y<-sapply(p,function(x) rbinom(1,1,x))
fit2<-RCreliability.ex(z.main,r,z.rep,W=W,Y)
fit2

# Regression on two covariates measured with error and no confounder
x<-mgcv::rmvn(3000,c(0,0),matrix(c(1,0.3,0.3,1),nrow=2))
#ICC=0.7 generate z
z.main = x[1:1500,1:2]+rnorm(1500,0,sqrt(0.4))
r<-c(rep(2,500),rep(3,400),rep(4,600))
z.rep<-list(rbind(cbind(x[1501:2000,1]+rnorm(500,0,sqrt(0.4)),x[1501:2000,1]+rnorm(500,0,sqrt(0.4)),NA,NA),
                  cbind(x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),NA),
                  cbind(x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)))),
            rbind(cbind(x[1501:2000,2]+rnorm(500,0,sqrt(0.4)),x[1501:2000,2]+rnorm(500,0,sqrt(0.4)),NA,NA),
                  cbind(x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),NA),
                  cbind(x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)))))
#prevalence about 0.105
p<-exp(-2.3+log(1.5)*rowSums(x[1:1500,]))/(1+exp(-2.3+log(1.5)*rowSums(x[1:1500,])))
Y<-sapply(p,function(x) rbinom(1,1,x))
fit3<-RCreliability.ex(z.main,r,z.rep,W=NULL,Y)
fit3

# Regression on two covariates measured with error and one confounders
x<-mgcv::rmvn(3000,c(0,0,0),matrix(c(1,0.3,0.2,0.3,1,0.5,0.2,0.5,1),nrow=3))
#ICC=0.7 generate z
r<-c(rep(2,500),rep(3,400),rep(4,600))
W<-cbind(x[1:1500,3])

z.main = x[1:1500,1:2]+rnorm(1500,0,sqrt(0.4))

z.rep<-list(rbind(cbind(x[1501:2000,1]+rnorm(500,0,sqrt(0.4)),x[1501:2000,1]+rnorm(500,0,sqrt(0.4)),NA,NA),
                  cbind(x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),NA),
                  cbind(x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)))),
            rbind(cbind(x[1501:2000,2]+rnorm(500,0,sqrt(0.4)),x[1501:2000,2]+rnorm(500,0,sqrt(0.4)),NA,NA),
                  cbind(x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),NA),
                  cbind(x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)))))
#prevalence about 0.105
p<-exp(-2.7+log(1.5)*rowSums(x[1:1500,1:3]))/(1+exp(-2.7+log(1.5)*rowSums(x[1:1500,1:3])))
Y<-sapply(p,function(x) rbinom(1,1,x))[1:1500]
fit5<-RCreliability.ex(z.main,r,z.rep,W=W,Y)
fit5


# Regression on two covariates measured with error and two confounders
x<-mgcv::rmvn(3000,c(0,0,0),matrix(c(1,0.3,0.2,0.3,1,0.5,0.2,0.5,1),nrow=3))
w2<-sapply(x[,1], function(t){if(t>median(x[,1])) {return(rbinom(1,1,0.5))}
  if(t<=median(x[,1])){return(rbinom(1,1,0.3))}})
#ICC=0.7 generate z
r<-c(rep(2,500),rep(3,400),rep(4,600))
W<-cbind(x[1:1500,3],w2[1:1500])

z.main = x[1:1500,1:2]+rnorm(1500,0,sqrt(0.4))

z.rep<-list(rbind(cbind(x[1501:2000,1]+rnorm(500,0,sqrt(0.4)),x[1501:2000,1]+rnorm(500,0,sqrt(0.4)),NA,NA),
                  cbind(x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),x[2001:2400,1]+rnorm(400,0,sqrt(0.4)),NA),
                  cbind(x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)),x[2401:3000,1]+rnorm(600,0,sqrt(0.4)))),
            rbind(cbind(x[1501:2000,2]+rnorm(500,0,sqrt(0.4)),x[1501:2000,2]+rnorm(500,0,sqrt(0.4)),NA,NA),
                  cbind(x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),x[2001:2400,2]+rnorm(400,0,sqrt(0.4)),NA),
                  cbind(x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)),x[2401:3000,2]+rnorm(600,0,sqrt(0.4)))))
#prevalence about 0.105
p<-exp(-2.7+log(1.5)*rowSums(x[1:1500,1:3])+log(1.5)*w2[1:1500])/(1+exp(-2.7+log(1.5)*rowSums(x[1:1500,1:3])+log(1.5)*w2[1:1500]))
Y<-sapply(p,function(x) rbinom(1,1,x))[1:1500]
fit4<-RCreliability.ex(z.main,r,z.rep,W=W,Y)
fit4
