\name{mod_OLS}
\alias{mod_OLS}
\title{Extant standard using a linear, ordinary least squares model}
\description{An internal function used by \code{\link{MASSunite}} to project a VD recontruction onto a standard of accuracy.}
\usage{mod_OLS(xvar = extants$log.HCFC, yvar = extants$log.BM, 
                    xlab = "Log Stylopodial Circumference",ylab = "Log Body Mass",
                    xlim = c(1, 3.5), ylim = c(1.5, 8), cex = 0.5, pch = 4, col = "gray", 
                    fit.col = "black", pred.seq = seq(from = 1, to = 4, by = 0.01), 
                    PI.col = "blue", PI.lty = 2, CI.col = "red", CI.lty = 1, 
                    verbose = FALSE, plot = TRUE, ...)}
\arguments{
  \item{xvar, yvar}{the x and y coordinates of the model, defaults to the log combined humeral and femoral circumferences and their associated log body mass, respectively.}
  \item{xlab, ylab}{single character objects specifying the axes labels, as in \code{\link{par}}, defaults to \code{xlab = "Log Stylopodial Circumference"} and \code{ylab = "Log Body Mass"}.}
  \item{xlim, ylim}{numeric vector specifying the axes ranges, as in \code{\link{par}}, defaults to \code{xlim = c(1, 3.5)} and \code{ylim = c(1.5, 8)}.}
  \item{cex, pch, col}{main plotting parameters, as in \code{\link{par}}, defaults to \code{cex = 0.5}, \code{pch = 4}, and \code{col = "gray"}.}
  \item{fit.col}{character object specifying the colour of the fitted line.}
  \item{pred.seq}{integer or numeric sequence use as \code{newdata} to generate confidence and prediction intervals.}
  \item{PI.col, PI.lty}{colour and line-type parameters for the prediction interval lines.}
  \item{CI.col, CI.lty}{colour and line-type parameters for the confidence interval lines.}
  \item{verbose}{a logical value indicating whether the results of the model fitting are returned.}
  \item{plot}{a logical value indicating whether the plot should be produced, defaults to \code{plot = FALSE}.}
  \item{...}{parameter arguments passed to \code{plot}.}
}
\details{
This function is mostly used internally by \code{\link{MASSunite}} to project volumetric reconstruction models onto the linear limb circumferences standard of accuracy, as presented in Campione & Evans (2020). However, the function can technically be used to generate a plot of any x-y coordinates with their fitted ordinary least squares line and associated confidence/prediction intervals. N.B. If modified, the defaults may not suite.
}
\value{
The default function will generate a plot. If \code{verbose = TRUE} a 3 item list will be returned with:
\item{model}{the output from \code{\link{lm}}.}
\item{prediction}{a 3-column matrix with the fitted, lower, and upper prediction intervals for the vector given in \code{pred.seq}.}
\item{confidence}{a 3-column matrix with the fitted, lower, and upper confidence intervals for the vector given in \code{pred.seq}.}
}
\author{Nicolas E. Campione}
\references{
	Campione, N. E. and Evans, D. C. (2012) A universal scaling relationship between body mass and proximal limb bone dimensions in quadrupedal terrestrial tetrapods. BMC Biology, 10, 60.
	
  Campione, N. E. and Evans, D. C. (2020) The accuracy and precisions of body mass estimation in non-avian dinosaurs. Biological Reviews.
}
\seealso{
  \code{\link{MASSunite}}
  \code{\link{mod_QUAD}}
}
\examples{
mod_OLS()
}