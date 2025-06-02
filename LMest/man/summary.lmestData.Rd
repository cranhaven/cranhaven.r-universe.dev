\name{summary.lmestData}
\alias{summary.lmestData}
\alias{plot.lmestData}
\alias{print.lmestData}


\title{Summary and plot of \code{lmestData}}
\description{Methods for \code{lmestData} object providing basic descriptive statistics (summary) and plots.}

\usage{
\method{summary}{lmestData}(object, type = c("all","cross", "year"),
        dataSummary = c("all", "responses", "manifest", "initial", "transition"),
        varType = rep("c", x$d), digits = getOption("digits"),
        maxsum = 10, maxobs = 20, \dots)
\method{plot}{lmestData}(x, typePlot = c("s", "sh"),
     dataPlots = c("all", "responses", "manifest", "initial", "transition"),
        \dots)
\method{print}{lmestData}(x, \dots)

}


\arguments{
\item{object}{an object of class \code{lmestData}}
\item{x}{an object of class \code{lmestData}}
\item{type}{type of summary to print. \code{all} prints a summary for each varaible, and a summary for each variables by time. \code{cross} prints a summary for each variable. \code{year} prints a summary for each variable by time. The summary is adapted according to \code{varType} (By default is set to \code{all})}
\item{dataSummary}{a string indicating whether summary is returned: \code{all} for the entire data, \code{responses}  for the responses, \code{manifest} for covariates on the manifest distribution, \code{initial} for the covariate affecting the initial probabilities, and \code{transition} for the covariates affecting the transition probabilities. (By default is set to \code{all})}
\item{varType}{a string vector of lengh equal to the number of variables, "c" for continuous and "d" for discrete, indicating wich varaibles are continuos and which are discrete}
\item{digits}{the number of significant digits}
\item{maxsum}{an integer value indicating the maximum number of levels to print}
\item{maxobs}{an integer value indicating the maximun number of observation in which the summary statistics are reported for each observation}
\item{typePlot}{a string indicating the type of plot. "s" plots a scaterplot matrix. "sh" plots a scatterplot matrix with the histogram for each variable in the diagonal}
\item{dataPlots}{a string indicating whether the plot is returned: \code{all} for the entire data, \code{responses}  for the responses, \code{manifest} for covariates on the manifest distribution, \code{initial} for the covariate affecting the initial probabilities, \code{transition} for the covariates affecting the transition probabilities.  (By default is set to all)}
\item{\dots}{further arguments}


}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}
