\encoding{utf8}
\name{circ.plugin.hdr}
\alias{circ.plugin.hdr}
\title{Circular plug-in estimation of HDRs and confidence regions
}
\description{
This function computes the circular plug-in estimator of HDRs and confidence regions in Saavedra-Nieves and Crujeiras (2021).
}
\usage{
circ.plugin.hdr(sample,bw=bw.CV(circular(sample),upper=100),tau=NULL,
                tau.method="quantile",level=NULL,conf=.95,plot.hdr=TRUE,
                plot.hdrconf=TRUE,boot=FALSE,k=3,col=NULL,lty=NULL,shrink=NULL,
                lwd=NULL,pch=NULL,cex=NULL)
}
\arguments{
  \item{sample}{Numeric vector of angles in radians.}
  \item{bw}{Smoothing parameter to be used. It can be a numeric value directly selected by the user. Following Oliveira et al. (2014), the value of the smoothing parameter could be also chosen by using the functions \code{bw.rt}, \code{bw.CV}, \code{bw.pi} or \code{bw.boot}. Moreover, function \code{circ.boot.bw} allows to select the new smoothing parameter proposed in Saavedra-Nieves and Crujeiras (2021). Default \code{bw=bw.CV} providing a cross-validation bandwidth.}
  \item{tau}{Numeric probability. According to Saavedra-Nieves and Crujeiras (2021), \code{1-tau} represents the probability coverage required for HDR. If \code{tau=NULL}, a value for the threshold \code{level} of the HDR must be provided.}
  \item{tau.method}{Character value selecting the rule to estimate the threshold of the HDR. This must be one of \code{"quantile"} or \code{"trapezoidal"}. The first option estimates the threshold using the quantile method proposed in Hyndman(1996); the second one, using the trapezoidal rule for numerical integration. Default \code{tau.method=} \code{"quantile"}.}
  \item{level}{Numeric threshold of the HDR provided by the user. When \code{level} is larger than the maximum value of the density, the HDR is equal to the emptyset. If \code{level} is smaller than the minimum of the density, the HDR coincides with the support distribution. If \code{level=NULL}, a value for the probability coverage \code{1-tau} of the HDR must be provided.}
  \item{conf}{Numeric value between 0 and 1 corresponding to the confidence for limits on HDR. Default \code{conf=0.95}.}
  \item{plot.hdr}{Logical string. If \code{TRUE}, the level set or the HDR are depicted. Default \code{plot.hdr=TRUE}. }
  \item{plot.hdrconf}{Logical string. If \code{TRUE}, the confidence region for the estimated HDR is added to the HDR graphical representation. Default \code{plot.hdr=TRUE}. }
   \item{boot}{Logical string. If \code{TRUE}, confidence regions are not computed. This option is only used for function \code{circ.boot.bw} in order to reduce the execution time. Default \code{boot=FALSE}. }
      \item{k}{Positive integer value that controls if the confidence region is plotted near (large values of \code{k}) or far away (small values of \code{k}) the estimated HDR. Default \code{k=3}. }
  \item{col}{Color number for plotting the HDR. Default \code{col="darkgray"} is used.}
  \item{lty}{A numeric value indicating the line type to represent the threshold of HDR. Line type can be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash). Default \code{lty=2}.}
  \item{shrink}{Parameter that controls the size of the plotted circle. Default is 2. Larger than 1 values shrink the circle, while smaller values enlarge the circle.}
  \item{lwd}{A number indicating the line width for drawing symbol. Default \code{3}.}
    \item{pch}{Point type. Default is \code{19}.}
    \item{cex}{Point character size for representing the data on the scatterplot. Default is \code{0.5}.}

}
\details{A detailed definition of plug-in estimators for directional HDRs is given in Saavedra-Nieves and Crujeiras (2021). The density quantile algorithm proposed in Hyndman (1996) or the numerical integration method of trapezoidal rule can be used to compute the threshold of HDR. The confidence region for the estimated HDR is calculated also following Hyndman (1996).}
\value{
If \code{tau} is provided, a list with the next components:%%  If it is a LIST, use
\item{hdr}{Boundaries of the HDR.}
\item{prob.content}{Probability coverage \code{1-tau}.}
\item{level}{Threshold associated to the probability content \code{1-tau}.}
\item{bw}{Value of the smoothing parameter used for kernel density estimation.}
\item{hdr.lo}{HDR corresponding to lower confidence limit.}
\item{level.lo}{Threshold associated to the lower confidence limit.}
\item{hdr.hi}{HDR corresponding to upper confidence limit.}
\item{level.hi}{Threshold associated to the upper confidence limit.}
If \code{level} is provided, a list with the next components:%%  If it is a LIST, use
\item{levelset}{boundaries of the level set or a character indicating if the level set is equal to the emptyset or the support distribution.}
\item{prob.content}{Probability coverage \code{1-tau} associated to the \code{level} value.}
\item{level}{Threshold of the level set.}
\item{bw}{Value of the smoothing parameter used for kernel density estimation.}

}
\references{
Hyndman, R.J. (1996). Computing and graphing highest density regions, The American Statistician, 50, 120-126.\cr
Oliveira, M., Crujeiras. R.M. and Rodríguez-Casal, A. (2014). NPCirc: An R Package for Nonparametric
Circular Methods, Journal of Statistical Software, 61, 1-26.\cr
Saavedra-Nieves, P. and Crujeiras, R. M. (2021). Nonparametric estimation of directional highest density regions. Advances in Data Analysis and Classification, 1-36.
}
\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}

\examples{
# Plug-in HDR for orientations of saltator specie in April and October
data(sandhoppers)
attach(sandhoppers)
#Orientations in October
saltatorO<-angle[(species=="salt")&(time=="afternoon")&(sex=="M")&(month=="October")]
circ.plugin.hdr(sample=saltatorO,tau=0.8,plot.hdrconf=FALSE)
#Orientations in April
saltatorA<-angle[(species=="salt")&(time=="afternoon")&(sex=="M")&(month=="April")]
circ.plugin.hdr(sample=saltatorA,tau=0.8,plot.hdrconf=FALSE)
#HDR confidence bands for model 5 in NPCirc package
library(NPCirc)
set.seed(1)
sample<- rcircmix(500, model=5)
circ.plugin.hdr(sample,bw=bw.CV(circular(sample),upper=100),tau=0.6)
}
