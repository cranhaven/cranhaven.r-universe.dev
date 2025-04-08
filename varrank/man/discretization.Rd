% discretization.Rd ---
% Author           : Gilles Kratzer
% Created on :       28.09.2017
% Last modification : 05.02.2018 (spelling + Guidelines for Rd files)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{discretization}
\alias{discretization}
\title{Discretization of a Possibly Continuous Data Frame of Random Variables}

\usage{
discretization(data.df = NULL, discretization.method = "cencov", frequency = FALSE)
}
\arguments{
  \item{data.df}{a data frame containing the data to discretize, binary variables must be declared as factors, other as numeric vector. The data frame must be named.}
  \item{discretization.method}{a character vector giving the discretization method to use; see details. If a number is provided, the variable will be discretized by equal binning.}
  \item{frequency}{logical variable to select the output. If set to TRUE a list with the table of count for each bin and the discretized data frame is returned. If set to FALSE only the discretized data frame is returned.}}

\description{
This function discretizes data frame of possibly continuous random variables through rules for discretization. The discretization algorithms are unsupervised and univariate. See details for the complete list (the desired number of bins could also be provided).
}

\details{
\code{discretization()} supports multiple rules for discretization. Below is the list of supported rules. IQR() stands for interquartile range.

\code{fd} stands for the Freedman–Diaconis rule. The number of bins is given by \deqn{\frac{range(x) * n^{1/3}}{2 * IQR(x)}}{%
range(x) * n^{1/3} / 2 * IQR(x)} The Freedman–Diaconis rule is known to be less sensitive than the Scott's rule to outlier.

\code{doane} stands for doane's rule. The number of bins is given by
\deqn{1 + \log_{2}{n} + \log_{2}{1+\frac{|g|}{\sigma_{g}}}}{%
1 + \log_{2}{n} + \log_{2}{1+\frac{|g|}{\sigma_{g}}}}

is a modification of Sturges' formula which attempts to improve its performance with non-normal data.

\code{cencov} stands for Cencov's rule. The number of bins is given by:
\deqn{n^{1/3}}{n^{1/3}}

\code{rice} stands for Rice' rule. The number of bins is given by:
\deqn{2 n^{1/3}}{2 n^{1/3}}

\code{terrell-scott} stands for Terrell-Scott's rule. The number of bins is given by: \deqn{(2 n)^{1/3}}{(2 n)^{1/3}}

This is known that Cencov, Rice and Terrell-Scott rules over estimates k compared to other rules due to his simplicity.

\code{sturges} stands for Sturges's rule. The number of bins is given by: \deqn{1 + \log_2(n)}{1 + \log_2(n)}

\code{scott} stands for Scott's rule. The number of bins is given by:
\deqn{range(x) / \sigma(x) n^{-1/3}}{range(x) / \sigma(x) n^{-1/3}}

\code{kmeans} apply the classical k-means clustering to one-dimensional continuous data.

}
\value{the discretized dataframe or a list containing the table of counts for each bin the discretized dataframe}

\author{Gilles Kratzer}

\references{Garcia, S., et al.  (2013) A survey of discretization techniques: Taxonomy and empirical analysis in supervised learning. \emph{IEEE Transactions on Knowledge and Data Engineering}, 25.4, 734-750.

Cebeci, Z. and Yıldız, F. (2017) Unsupervised Discretization of Continuous Variables in a Chicken Egg Quality Traits Dataset. \emph{Turkish Journal of Agriculture-Food Science and Technology}, 5.4, 315-320.
}

\examples{
rv <- rnorm(n = 100, mean = 0, sd = 2)

entropy.data(freqs.table = discretization(data.df = rv,
discretization.method = "fd",
frequency=TRUE)[[1]])

}
