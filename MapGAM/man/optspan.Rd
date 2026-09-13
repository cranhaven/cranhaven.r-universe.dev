\name{optspan}
\alias{optspan}
\title{
Determine the Optimal Span Size for \code{\link{modgam}}  
}
\description{
Determines the optimal span size for \code{modgam}, a spatial generalized additive model (GAM) with a two-dimensional LOESS smooth for location, by minimizing the AIC.  
}
\usage{
  optspan(formula, data, offset,spans = seq(0.05, 0.95, by = 0.05), m ="adjusted", 
          family = binomial(), verbose =TRUE, degree = 1, \dots)
}
\arguments{
  \item{formula}{
a formula expression for the model, with the response on the left of a ~ oprator, and the predictors on the right. If fitting a Cox additive model, the response must be a survival object as returned by the Surv function. A built-in nonparametric smoothing term is indicated by \code{lo} for loess smooth terms. The two-dimensional predictor (e.g.,geolocation) should be specified as auguments in \code{lo()}. \code{formula} can be missing if \code{data} is well structured and \code{family} is specified.     
}
  \item{data}{
a data frame containing the variables in the model. \code{data} can be missing if \code{formula} is specified. If \code{formula} is missing, the data must be structured so that the outcome variable is in the 1st (1st and 2nd for survival data) column and X and Y location values are in the 2nd and 3rd (3rd and 4th for survival data) columns.  Any additional columns will be entered as covariates with linear effects in the model. 
}
  \item{offset}{
the name in data that will be used as offset in the model. It will be ignored if offset is specified in \code{formula}.
}
  \item{spans}{
a vector of candidate span sizes that the optimal span is chosen from.
}
  \item{m}{
the model type.  Options are "crude" or "adjusted" (default).  If "crude", only the spatial smooth term for location is included in the model.  If "adjusted", all covariates in the data set (columns >= 4) are included in the model.  
}
  \item{family}{
a family function or the result of a call to a family function. (See \code{\link{family}} for details of family functions. \code{famil = "survival"} for survival data.	
}
  \item{verbose}{
a logical argument; if TRUE shows the AIC for each candidate span size.
  }
  \item{degree}{
a smoothing parameter used in \code{\link{modgam}}.  
}
  \item{\dots}{
any additional arguments to pass to the gam function.
}
}

\value{
The optimal span size, determined by minimizing the AIC
}

\author{
Lu Bai, Scott Bartell, Robin Bliss, and Veronica Vieira

Send bug reports to \email{sbartell@uci.edu}.
}

\note{
This function does not return model predictions--only the optimal span size.  To obtain model predictions use the modgam function.   
}

\seealso{
\code{\link{predgrid}}, 
\code{\link{modgam}}, 
\code{\link{colormap}}.
}
\examples{
\donttest{
data(MAdata)
optspan(data=MAdata, m="crude")

data(CAdata)
optspan(Surv(time,event)~AGE+factor(INS)+lo(X,Y), data=CAdata,
        spans=c(0.1,0.2,0.3),family="survival")
}
}

\keyword{misc}
\keyword{smooth}
\keyword{optimize}