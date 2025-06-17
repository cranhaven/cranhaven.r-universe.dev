\name{plot.clikcorr}
\alias{plot.clikcorr}



\title{
Graphical function for visualizing bivariate profile likelihood.
}

\description{
Produces a plot of the profile log likelihood function.
}

\usage{
\method{plot}{clikcorr}(x, type = "l", lwd = 2, col = "red", \dots)
}

\arguments{
  \item{x}{a \code{"clikcorr"} object.}
  \item{type}{line type.}
  \item{lwd}{line weight.}
  \item{col}{line color.}
  \item{\dots}{not used.}
}

\details{
produces a plot of the profile log likelihood function.
}

\references{
Yanming Li, Kerby Shedden, Brenda W. Gillespie and John A. Gillespie (2016). Calculating Profile Likelihood Estimates of the Correlation Coefficient in the Presence of Left, Right or Interval Censoring and Missing Data.
}

\author{
Yanming Li, Kerby Shedden, Brenda W. Gillespie and John A. Gillespie.
}

\examples{

data(ND)
logND <- log(ND)
logND1 <- logND[51:90,]

obj <- clikcorr(logND1, "t1_OCDD", "t2_OCDD", "t1_HxCDF_234678", "t2_HxCDF_234678")
plot(obj, type="o")

\dontrun{
obj <- clikcorr(logND, "t1_OCDD", "t2_OCDD", "t1_HxCDF_234678", "t2_HxCDF_234678")
plot(obj, type="o", col="blue", lwd=1)
}
}

