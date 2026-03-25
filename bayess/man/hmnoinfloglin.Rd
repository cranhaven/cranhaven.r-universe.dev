\name{hmnoinfloglin}
\alias{hmnoinfloglin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metropolis-Hastings for the log-linear model under a noninformative prior%%%%  ~~function to do ... ~~
}
\description{
This function is a version of \code{hmnoinflogit} for the log-linear model, using a non-informative
prior defined in Chapter 4 and a proposal based on a random walk Metropolis-Hastings step.
}
\usage{
hmnoinfloglin(niter, y, X, scale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{niter}{
number of iterations%%     ~~Describe \code{niter} here~~
}
  \item{y}{
binary response variable%%     ~~Describe \code{y} here~~
}
  \item{X}{
matrix of covariates with the same number of rows as \code{y}%%     ~~Describe \code{X} here~~
}
  \item{scale}{
scale of the random walk%%     ~~Describe \code{scale} here~~
%%     ~~Describe \code{scale} here~~
}
}
\value{
%%  ~Describe the value returned
The function produces a sample of \eqn{\beta}{beta}'s as a matrix of size \code{niter} x \code{p},
where \code{p} is the number of covariates.
}
\seealso{
\code{\link{hmflatloglin}}
}
\examples{
airqual=na.omit(airquality)
ozone=cut(airqual$Ozone,c(min(airqual$Ozone),median(airqual$Ozone),max(airqual$Ozone)),
include.lowest=TRUE)
month=as.factor(airqual$Month)
tempe=cut(airqual$Temp,c(min(airqual$Temp),median(airqual$Temp),max(airqual$Temp)),
include.lowest=TRUE)
counts=table(ozone,tempe,month)
counts=as.vector(counts)
ozo=gl(2,1,20)
temp=gl(2,2,20)
mon=gl(5,4,20)
x1=rep(1,20)
lulu=rep(0,20)
x2=x3=x4=x5=x6=x7=x8=x9=lulu
x2[ozo==2]=x3[temp==2]=x4[mon==2]=x5[mon==3]=1
x6[mon==4]=x7[mon==5]=x8[ozo==2 & temp==2]=x9[ozo==2 & mon==2]=1
x10=x11=x12=x13=x14=x15=x16=lulu
x10[ozo==2 & mon==3]=x11[ozo==2 & mon==4]=x12[ozo==2 & mon==5]=x13[temp==2 & mon==2]=1
x14[temp==2 & mon==3]=x15[temp==2 & mon==4]=x16[temp==2 & mon==5]=1
X=cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16)
noinloglin=hmnoinfloglin(1000,counts,X,0.5)
par(mfrow=c(4,4),mar=1+c(1.5,1.5,1.5,1.5),cex=0.8)
for (i in 1:16) plot(noinloglin[,i],type="l",ylab="",xlab="Iterations")
}
\keyword{log-linear model}
\keyword{Metropolis-Hastings algorithm}
\keyword{random walk proposal}
\keyword{noninformative prior}
