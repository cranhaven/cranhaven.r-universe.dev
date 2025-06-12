\name{reliabilitybssn}
\alias{reliabilitybssn}
\alias{Rebssn}
\alias{Fbssn}


\title{Reliability Function for the Birnbaum-Saunders model based on Skew-Normal distribution
}
\description{Two useful descriptors in reliability analysis are the reliability function (rf), and the failure rate (fr) function or hazard function. For a non-negative random variable \eqn{t} with pdf \eqn{f(t)} (and cdf \eqn{F(t)}), its distribution can be characterized equally in terms of the rf, or of the fr, which are respectively defined by \eqn{R(t)=1-F(t)}, and \eqn{h(t)=f(t)/R(t)}, for \eqn{t>0},and \eqn{0<R(t)<1}.
}
\usage{
Rebssn(ti,alpha=0.5,beta=1,lambda=1.5)
Fbssn(ti,alpha=0.5,beta=1,lambda=1.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{ti}{
dataset.
}
\item{alpha}{
shape parameter \eqn{\alpha}.
}
  \item{beta}{
scale parameter \eqn{\beta}.
}
  \item{lambda}{
skewness parameter \eqn{\lambda}.
}
}
%\details{

%}
\value{
\code{Rbssn} gives the reliability function, \code{Fbssn} gives the failure rate or hazard function.
}

\references{
Leiva, V., Vilca-Labra, F. E., Balakrishnan, N. e Sanhueza, A. (2008). A skewed sinh-normal distribution and its properties and application to air pollution. Comm. Stat. Theoret. Methods. Submetido.

Guiraud, P., Leiva, V., Fierro, R. (2009). A non-central version of the Birnbaum-Saunders distribution for reliability analysis. IEEE Transactions on Reliability 58, 152-160.
}

\author{Rocio Maehara \email{rmaeharaa@gmail.com} and Luis Benites \email{lbenitesanchez@gmail.com}
}

\seealso{
\code{\link{bssn}}, \code{\link{EMbssn}}, \code{\link{momentsbssn}}, \code{\link{ozone}}, \code{\link{Rebssn}}
}

\examples{
## Let's compute some realiability functions for a Birnbaum-Saunders model based on
## Skew normal Distribution for different values of the shape parameter.

ti  <- seq(0,2,0.01)
f1  <- Rebssn(ti,0.75,1,1)
f2  <- Rebssn(ti,1,1,1)
f3  <- Rebssn(ti,1.5,1,1)
f4  <- Rebssn(ti,2,1,1)
den <- cbind(f1,f2,f3,f4)

matplot(ti,den,type="l", col=c("deepskyblue4","firebrick1","darkmagenta","aquamarine4"),
ylab="S(t)", xlab="t",lwd=2)
legend(1.5,1,c(expression(alpha==0.75), expression(alpha==1), expression(alpha==1.5),
expression(alpha==2)),col= c("deepskyblue4","firebrick1","darkmagenta","aquamarine4"),
lty=1:4,lwd=2,seg.len=2,cex=0.9,box.lty=0,bg=NULL)


## Let's compute some hazard functions for a Birnbaum Saunders model based on
## Skew normal Distribution for different values of the skewness parameter.

ti  <- seq(0,2,0.01)
f1  <- Fbssn(ti,0.5,1,-1)
f2  <- Fbssn(ti,0.5,1,-2)
f3  <- Fbssn(ti,0.5,1,-3)
f4  <- Fbssn(ti,0.5,1,-4)
den <- cbind(f1,f2,f3,f4)
matplot(ti,den,type = "l", col = c("deepskyblue4","firebrick1", "darkmagenta", "aquamarine4"),
ylab = "h(t)" , xlab="t",lwd=2)
legend(0.1,23, c(expression(lambda==-1), expression(lambda==-2), expression(lambda == -3),
expression(lambda == -4)), col = c("deepskyblue4", "firebrick1", "darkmagenta","aquamarine4"),
lty=1:4,lwd=2,seg.len=2,cex=0.9,box.lty=1,bg=NULL)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Birnbaum-Saunders Skew-Normal}
\keyword{reliability function}
\keyword{failure rate}
\keyword{hazard function}
