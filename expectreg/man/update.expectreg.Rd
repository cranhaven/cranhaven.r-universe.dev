\name{update.expectreg}
\alias{update.expectreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update given expectreg model
}
\description{
Updates a given expectreg model with the specified changes
}
\usage{
\method{update}{expectreg}(object, add_formula, data = NULL, estimate = NULL, 
smooth = NULL, lambda = NULL, expectiles = NULL, delta_garrote = NULL, ci = NULL, 
...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
of class expectreg
}
  \item{add_formula}{
update for formula
}
  \item{data}{
Should other data be used
}
  \item{estimate}{
Change estimate
}
  \item{smooth}{
Change smooth
}
  \item{lambda}{
Change lambda
}
  \item{expectiles}{
Change asymmetries
}
  \item{delta_garrote}{
Change delta_garrote
}
  \item{ci}{
Change ci
}
  \item{\dots}{
additional parameters passed on to \code{expectreg.ls}
}
}
\details{
Re-estimates the given model, with the specified changes. If nothing is specified the characteristics of the original model are used. Except lambda here the default 1 is used as initial value.
}
\value{
object of class \code{expectreg}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Elmar Spiegel \cr
Helmholtz Zentrum Muenchen \cr
\url{https://www.helmholtz-munich.de} \cr
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats]{update}}, \code{\link[stats]{update.formula}}
}
\examples{
data(india)

model1<-expectreg.ls(stunting~rb(cbmi),smooth="fixed",data=india,lambda=30,
                     estimate="restricted",expectiles=c(0.01,0.05,0.2,0.8,0.95,0.99))
plot(model1)

# Change formula and update model
add_formula<-.~.+rb(cage)
update_model1<-update(model1,add_formula)
plot(update_model1)

# Use different asymmetries and update model
update_model2<-update(model1,expectiles=c(0.1,0.5,0.9))
plot(update_model2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ nonparametric }
\keyword{ smooth }% __ONLY ONE__ keyword per line
\keyword{ multivariate }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ models }