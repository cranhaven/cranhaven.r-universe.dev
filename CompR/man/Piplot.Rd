\name{Piplot}
\alias{Piplot}
\title{
Graphical representation of the Bradley's scores}
\description{
Gives a graphical representation of the Bradley's scores.}
\usage{
Piplot(Pi, SigmaPi = NULL, level=0.05, main = NULL, ylab = "Bradley's scores",
xlab = "Item", labelprod = NULL)
}
\arguments{
  \item{Pi}{
vector of Bradley's scores}
  \item{SigmaPi}{
vector of Bradley's scores standard deviation given by the user.\cr
(default \code{SigmaPi=NULL})}
  \item{level}{
level to use for the confidence intervals. (default \code{level=0.05})}
  \item{main}{
Title of the plot.(default \code{main=NULL})}
  \item{ylab}{
value for \code{ylab}. (default \code{ylab}= Bradley's scores)}
  \item{xlab}{
value for \code{xlab}. (default \code{xlab}=Item)}
  \item{labelprod}{
label vector of the Item. (default \code{labelprod=NULL})
}
}
\details{
The representation is based on plot(x) function, with Item on x axis, and Bradley's scores on y axis. If \code{SigmaPi} is provided by user, a 1-\code{level} (default 95\%)  confidence interval is drawn for each Item.
}
\value{
A graphical representation of bradley's scores.
}
\examples{
  data(Cocktail_Cum)
  res<-C_piBTL(Cocktail_Cum,Constraint=0,eps1=0.0001,Pi=NULL,TestPi=TRUE)
  Res_Pi<-res$Pi
  Res_Varcov<-res$VarcovPi
  Res_Sigma<-sqrt(diag(Res_Varcov))
  Piplot(Res_Pi, SigmaPi = Res_Sigma, level=0.01, main = NULL, ylab = "Bradley's scores",
  xlab = "Item", labelprod = NULL)
  
  
}