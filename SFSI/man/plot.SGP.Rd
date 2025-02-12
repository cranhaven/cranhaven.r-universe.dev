
\name{8. SGP accuracy vs penalization plot}
\alias{plot.SGP}
\title{Accuracy vs penalization plot}
\usage{
\method{plot}{SGP}(..., x.stat = c("nsup","lambda"),
          y.stat = c("accuracy","MSE"),
          label = x.stat, nbreaks.x = 6)
}
\arguments{
\item{...}{Other arguments to be passed: 
\itemize{
 \item One or more objects of the class 'SGP'
 \item Optional arguments for method \code{plot}: 'xlab', 'ylab', 'main', 'lwd', 'xlim', 'ylim'
 \item For multi-trait SGP, optional argument 'trait' to plot results for a specific trait
}
}

\item{x.stat}{(character) Either 'nsup' (number of non-zero regression coefficients entering in the prediction of a given testing individual) or 'lambda' (penalization parameter in log scale) to plot in the x-axis}

\item{y.stat}{(character) Either 'accuracy' (correlation between observed and predicted values) or 'MSE' (mean squared error) to plot in the y-axis}

\item{label}{(character) Similar to \code{x.stat} but to show the value in x-axis for which the y-axis is maximum}

\item{nbreaks.x}{(integer) Number of breaks in the x-axis}
}
\value{
Creates a plot of either accuracy or MSE versus either the support set size (average number of predictors with non-zero regression coefficient) or versus lambda. 
}
\description{
Accuracy as a function of the penalization plot for an object of the class 'SGP'
}
\examples{
  # See examples in
  # help(SGP, package="SFSI")
}
