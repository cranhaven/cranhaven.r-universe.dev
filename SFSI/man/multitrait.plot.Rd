
\name{Multi-trait SGP accuracy vs penalization plot}
\alias{multitrait.plot}
\title{Accuracy vs penalization from multi-trait SGP}
\usage{
multitrait.plot(object, trait_names = NULL,
                x.stat = c("nsup","lambda"),
                y.stat = c("accuracy","MSE"), label = x.stat,
                line.color = "orange", point.color = line.color,
                point.size = 1.2, nbreaks.x = 6, ...)

}
\arguments{
\item{object}{An object of the class 'SGP' for a multi-trait case}

\item{x.stat}{(character) Either 'nsup' (number of non-zero regression coefficients entering in the prediction of a given testing individual) or 'lambda' (penalization parameter in log scale) to plot in the x-axis}

\item{y.stat}{(character) Either 'accuracy' (correlation between observed and predicted values) or 'MSE' (mean squared error) to plot in the y-axis}

\item{label}{(character) Similar to \code{x.stat} but to show the value in x-axis for which the y-axis is maximum across traits}

\item{point.color, line.color}{(character) Color of the points and lines}

\item{point.size}{(numeric) Size of the points showing the maximum accuracy}

\item{nbreaks.x}{(integer) Number of breaks in the x-axis}

\item{trait_names}{(character) Names of traits to be shown in the plot}

\item{...}{Other arguments for method \code{plot}: 'xlab', 'ylab', 'main', 'lwd', 'xlim', 'ylim'}
}
\value{
Creates a plot of either accuracy or MSE versus either the support set size (average number of predictors with non-zero regression coefficient) or versus lambda. This is done separately for each trait 
}
\description{
Visualizing results from an object of the class 'SGP'
}
\examples{
  # See examples in
  # help(SGP, package="SFSI")
}
