\name{plot.expectreg}
\alias{plot.expectreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Default expectreg plotting
}
\description{
Takes a \code{expectreg} object and plots the estimated effects.
}
\usage{
\method{plot}{expectreg}(x, rug = TRUE, xlab = NULL, ylab = NULL, ylim = NULL, 
legend = TRUE, ci = FALSE, ask = NULL, cex.main = 2, mar.min = 5, main = NULL, 
cols = "rainbow", hcl.par = list(h = c(260, 0), c = 185, l = c(30, 85)), 
ylim_spat = NULL, ylim_factor = NULL, range_warning = TRUE, add_intercept = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{expectreg} as returned e.g. by the function \code{\link{expectreg.ls}}.
}
  \item{rug}{
Boolean. Whether nonlinear effects are displayed in a rug plot.
}
  \item{xlab, ylab, ylim}{
Graphic parameters. \code{xlab} should match the number of covariates.
}
  \item{legend}{
Boolean. Decides whether a legend is added to the plots.
}
  \item{ci}{
Boolean. Whether confidence intervals and significances should be plotted.
}
  \item{ask}{
Should always be asked before a new plot is printed.
}
  \item{cex.main}{
Font size of main
}
  \item{mar.min}{
Minimal margins, important when markov fields are plotted
}
  \item{main}{
Vector of main per plot
}
  \item{cols}{
Colours sheme of plots. Default is rainbow. Alternatively \code{hcl} can be used.
}
  \item{hcl.par}{
Parameters to specify the hcl coulour sheme.
}
  \item{ylim_spat}{
y_limits of the markov random field and all other spatial methods.
}
  \item{ylim_factor}{
y_limits of the plots of factor covariates.
}
  \item{range_warning}{
Should a warning be printed in the graphic if the range of the markov random field/factor plot is larger than the specified limits in \code{markov_ylim}/\code{factors_ylim}.
}
\item{add_intercept}{
Should the intercept be added to the plots of splines?
}
  \item{\dots}{
  Graphical parameters passesd on to the standard plot function.
}
}
\details{
The \code{plot} function gives a visual representation of the fitted expectiles
separately for each covariate.
}
\value{
No return value, only graphical output.
}
\references{
Schnabel S and Eilers P (2009)
\emph{ Optimal expectile smoothing }
Computational Statistics and Data Analysis, 53:4168-4177

Sobotka F and Kneib T (2010)
\emph{ Geoadditive Expectile Regression }
Computational Statistics and Data Analysis,
doi: 10.1016/j.csda.2010.11.015.

}
\author{
Fabian Otto- Sobotka \cr
Carl von Ossietzky University Oldenburg \cr
\url{https://uol.de} \cr

Elmar Spiegel \cr
Georg August University Goettingen \cr
\url{https://www.uni-goettingen.de} \cr
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{expectreg.ls}}, \code{\link{expectreg.boost}}, \code{\link{expectreg.qp}}
}
\examples{
data(dutchboys)

expreg <- expectreg.ls(hgt ~ rb(age,"pspline"),data=dutchboys,smooth="f",
                       expectiles=c(0.05,0.2,0.8,0.95))
plot(expreg)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
