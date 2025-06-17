\name{splot}
\alias{splot}

\title{
Graphical function for visualizing bivariate censored and/or missing data
}

\description{
Generates matrix of scatter plots for bivariate data with different types of censoring and missing. 
}

\usage{
splot(data, lower.list, upper.list, ti =ifelse(length(lower.list)>2, 
paste("Scatter plots of", lower.list[1], "to", lower.list[length(lower.list)]), 
paste("Scatter plot of", lower.list[1], "and", lower.list[2])),
 legend = TRUE, cex = 1.5, \dots)
}


\arguments{
  \item{data}{a data frame name.}
  \item{lower.list}{the lower bounds names in the data frame of the variables between which the scatter plots are to be generated.}
  \item{upper.list}{the upper bounds names in the data frame of the variables between which the scatter plots are to be generated.}
  \item{ti}{figure title.}
  \item{legend}{figure legend.}
  \item{cex}{simbol sizes.}
  \item{\dots}{not used.}
}

\details{
Generates matrix of scatter plots for bivariate data with different types of censoring and missing. 
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

splot(logND, c("t1_OCDD", "t1_TCDF", "t1_HxCDF_234678"),
 c("t2_OCDD", "t2_TCDF", "t2_HxCDF_234678"), ti="scatter plot matrix")

splot(logND, c("t1_OCDD", "t1_TCDF", "t1_HxCDF_234678"),
 c("t2_OCDD", "t2_TCDF", "t2_HxCDF_234678"), ti="scatter plot matrix", bg="gold")

}

