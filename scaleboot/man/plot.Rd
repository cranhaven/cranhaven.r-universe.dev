\name{plot.scaleboot}
\alias{plot.scaleboot}
\alias{plot.summary.scaleboot}
\alias{lines.scaleboot}
\alias{sblegend}
\alias{plot.scalebootv}
\alias{plot.summary.scalebootv}
\alias{sbplotbeta}
\title{Plot Diagnostics for Multiscale Bootstrap}
\description{
  \code{plot} method for class \code{"scaleboot"}.
}
\usage{

\method{plot}{scaleboot}(x, models=NULL, select=NULL, sort.by=c("aic","none"),
     k=NULL, s=NULL, sp=NULL, lambda=NULL, bpk=NULL,
     xval = c("square", "inverse","sigma"),
     yval = c("psi", "zvalue", "pvalue"), xlab = NULL,
     ylab = NULL,log.xy = "", xlim = NULL, ylim = NULL,
     add = F, length.x = 300, main=NULL,
     col =1:6, lty = 1:5, lwd = par("lwd"), ex.pch=2:7,
     pch = 1, cex = 1, pt.col = col[1],pt.lwd = lwd[1],
     legend.x = NULL, inset = 0.1, cex.legend=1,...)

\method{plot}{summary.scaleboot}(x, select="average",
     k=x$parex$k,s=x$parex$s,sp=x$parex$sp,lambda=x$parex$lambda, ...)


\method{plot}{scalebootv}(x,models=attr(x,"models"),sort.by="none",...)

\method{plot}{summary.scalebootv}(x, select="average",...)


\method{lines}{scaleboot}(x,z,models=names(x$fi), k=NULL,s=NULL,sp=NULL,lambda=NULL,
      bpk=NULL, length.x=z$length.x, col=z$col,lty=z$lty,lwd=z$lwd,... )

sblegend(x="topright",y=NULL,z,inset=0.1,...)

sbplotbeta(beta, p=0.05, col.contour=c("blue","red","green"),
                     drawcontours = TRUE, drawlabels = TRUE,
                     labcex=1,length=100, cex=1, col="black",
                     xlim=NULL, ylim=NULL, lim.countourexpand=0 ) 

}
\arguments{
  \item{x}{an object used to select a method. 
    For \code{sblegend},
    \code{x} is a numeric or character such as "lefttop" or "righttop", which
    is passed to \code{legend}.
  }
  \item{models}{character vector of model names. Numeric is also allowed.}
  \item{select}{"average", "best", or one of the fitted models.}
  \item{sort.by}{"aic" or "none".}
  \item{k}{k for extrapolation.}
  \item{s}{s for extrapolation.}
  \item{sp}{sp for extrapolation.}
  \item{lambda}{a numeric of specifying the type of p-values; Bayesian (lambda=0) Frequentist (lambda=1).}
  \item{bpk}{(experimental for 2-step bootstrap)}
  \item{xval}{specifies x-axis.
    "square" for \eqn{\sigma^2},
    "inverse" for \eqn{1/\sigma},
    "sigma" for \eqn{\sigma}.}
  \item{yval}{specifies y-axis. "zvalue" for
    \eqn{\psi(\sigma^2|\beta)/\sigma} or \code{qnorm(1-bp[i])},
    "pvalue" for \eqn{1-\Phi(\psi(\sigma^2|\beta)/\sigma)} or \code{bp[i]},
    "psi" for \eqn{\psi(\sigma^2|\beta)} or \code{sqrt(sa[i])*qnorm(1-bp[i])}.}
  \item{xlab}{label for x-axis.}
  \item{ylab}{label for y-axis.}
  \item{log.xy}{character to specify log-scale. "", "x", "y", or "xy".}
  \item{xlim}{range for x-axis.}
  \item{ylim}{range for y-axis.}
  \item{add}{logical for adding another plot.}
  \item{length.x}{the number of segments to draw curves.}
  \item{main}{for title.}
  \item{col}{color for model curves.}
  \item{lty}{lty for model curves.}
  \item{lwd}{lwd for model curves.}
  \item{ex.pch}{pch for extrapolation.}
  \item{pch}{pch for bp points.}
  \item{cex}{cex for bp points.}
  \item{pt.col}{col for bp points.}
  \item{pt.lwd}{lwd for bp points.}
  \item{legend.x}{passed to sblegend as the first argument.}
  \item{...}{further arguments passed to or from other methods.}  
  \item{z}{output from previous \code{plot.scaleboot}.}
  \item{y}{numeric passed to \code{legend}.}
  \item{inset}{inset distance from the margins, which is passed to
    \code{legend}.}
  \item{cex.legend}{cex for legend}
  \item{beta}{matrix of beta values. beta[,1] is beta0, beta[,2] is beta1.}
  \item{p}{significance level for drawing contour lines.}
  \item{col.contour}{colors for SI, AU, BP.}
  \item{drawcontours}{draw contours when TRUE.}
  \item{drawlabels}{draw labels at contours when TRUE.}
  \item{labcex}{cex for contours.}
  \item{length}{grid size for drawing contours.}
  \item{lim.countourexpand}{expand contour plotting region}
}
\details{
  The \code{plot} method plots bootstrap probabilities and calls the \code{lines}
  method, which draws fitted curves for models.
}
\author{Hidetoshi Shimodaira}
\seealso{\code{\link{sbfit}}, \code{\link{sbphylo}}.}
\examples{
data(mam15)
## a single plot
a <- mam15.relltest[["t4"]] # an object of class "scaleboot"
plot(a,legend="topleft") # x=sigma^2, y=psi
plot(a,xval="inverse",yval="zvalue",
     legend="topleft") # x=1/sigma, y=z-value
plot(a,xval="sigma",log="x",yval="pvalue",
     legend="topleft") # x=log(sigma), y=probability
## plot of extrapolation
plot(summary(a),legend="topleft")
## multiple plots
b <- mam15.relltest[1:15] # an object of class "scalebootv"
plot(b) # x=sigma^2, y=psi
}
\keyword{ hplot }
\keyword{ models }
