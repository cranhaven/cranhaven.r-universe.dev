\name{maTrend}
\alias{maTrend}
\alias{print.maTrend}
\alias{plot.maTrend}
\title{Trend of probability values in a binary choice model}
\description{This function computes the change of probability for a continuous variable based on a binary choice model (either probit or logit), and furthermore, stratifies the probability through a binary independant variable.}

\usage{maTrend(q, n = 300, nam.c, nam.d, simu.c = TRUE)}

\arguments{
  \item{q }{ a object of class of "maBina" estimated from \code{maBina()}.}
  \item{n }{ number of points for calculating probability; the large the number, the smoother the curve.}
  \item{nam.c }{ a name of a continuous indepedent variable; this must be given for the function to work.}
  \item{nam.d }{ an optional name of a binary independent variable; this is used to stratify the probability.}
  \item{simu.c }{ Whether simulation data for the continuous variable of \code{nam.c} should be used (default is TRUE); if not, the original data for this variable is used.}
}
\details{For a continous variable, its probability values can be computed on the basis of a binary probit or logit model. The trend can be stratified by a binary independent variable. In addition, the standard errors of each probability series are also computed using delta method.}

\value{Return a list object of class "maTrend" with the following components:

  \item{q }{ a list object of class "maBina"}
  \item{nam.c }{ the name of a continous variable}
  \item{mm }{ matrix of independant variables for all}

  \item{trend }{ if \code{nam.d} is not specified, this is a data frame of the continous variable, its probability values, standard errors, t values, and p values; if \code{nam.d} is specified, the data frame contains the continuous variable, the probability values for all, and two additional probability series stratified by the dummy variable}
  
  \item{nam.d }{ if \code{nam.d} is specified, the name of a binary variable .}

  \item{m1 }{ if \code{nam.d} is specified, the matrix of \code{mm} with the column value for \code{nam.d} replaced by 1}
  
  \item{m0 }{ if \code{nam.d} is specified, the matrix of \code{mm} with the column value for \code{nam.d} replaced by 0}
 
  \item{trend1 }{ if \code{nam.d} is specified, this is a data frame reporting the probability value, standard error, t value, and p value associated with the dummy variable being 1}
 
   \item{trend0 }{ if \code{nam.d} is specified, this is a data frame reporting the probability value, standard error, t value, and p value associated with the dummy variable being 0}
}


\section{Methods}{
  Two methods are defined as follows:
  \describe{
    \item{\code{print}:}{print the probability output.}
    \item{\code{plot}:}{Plot the probability values for a continuous variable. If a strata is specified through \code{nam.d} in \code{maTrend()}, then the stratified values also are shown.}    
  }
}

\references{Greene, W.H. 2003. Econometric Analysis (5th ed.). Prentice Hall, New York. 1026 P.}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\seealso{\code{\link{maBina}}; \code{\link{print.maTrend}}; \code{\link{plot.maTrend}}.}

\examples{
data(daPe)
ma <- glm(grade ~ gpa + tuce + psi, x = TRUE, 
    data = daPe, family = binomial(link = "probit"))
summary(ma)

(ea <- maBina(w = ma, x.mean = TRUE, rev.dum = TRUE))
(ta <- maTrend(q = ea, nam.c = "gpa", simu.c = FALSE))
(tb <- maTrend(q = ea, nam.c = "gpa", nam.d= "psi", simu.c = TRUE, n=100))
plot(ta)
plot(tb)
}

\keyword{regression}