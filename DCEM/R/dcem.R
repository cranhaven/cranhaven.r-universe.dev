#' DCEM: Clustering Big Data using Expectation Maximization Star (EM*) Algorithm.
#'
#' Implements the EM* and EM algorithm
#' for clustering the (univariate and multivariate) Gaussian mixture data.
#'
#' @section Demonstration and Testing:
#' \strong{Cleaning the data:}
#' The data should be cleaned (redundant columns should be removed). For example
#' columns containing the labels or redundant entries (such as a column of
#' all 0's or 1's). See \code{\link{trim_data}} for details on
#' cleaning the data. Refer: \code{\link{dcem_test}} for more details.
#'
#' @section Understanding the output of \code{\link{dcem_test}}:
#'
#' The function dcem_test() returns a list of objects.
#' This list contains the parameters associated with the Gaussian(s),
#' posterior probabilities (prob), mean (meu), co-variance/standard-deviation(sigma)
#' ,priors (prior) and cluster membership for data (membership).
#'
#' \strong{Note:} The routine dcem_test() is only for demonstration purpose.
#' The function \code{\link{dcem_test}} calls the main routine
#' \code{\link{dcem_train}}. See \code{\link{dcem_train}} for further details.
#'
#' @section How to run on your dataset:
#' See \code{\link{dcem_train}} and \code{\link{dcem_star_train}} for examples.
#'
#' @section Package organization:
#' The package is organized as a set of preprocessing functions and the core
#' clustering modules. These functions are briefly described below.
#' \enumerate{
#'
#' \item  \code{\link{trim_data}}: This is used to remove the columns
#' from the dataset. The user should clean the dataset before
#' calling the dcem_train routine. \strong{User can also clean the dataset themselves
#' (without using trim_data) and then pass it to the dcem_train function}
#'
#' \item \code{\link{dcem_star_train}} and \code{\link{dcem_train}}: These are the primary
#' interface to the EM* and EM algorithms respectively. These function accept the cleaned dataset and other
#' parameters (number of iterations, convergence threshold etc.) and run the algorithm until:
#'
#' \enumerate{
#'    \item The number of iterations is reached.
#'    \item The convergence is achieved.
#'    }
#'    }
#'
#' @section DCEM supports following initialization schemes:
#'
#' \enumerate{
#' \item \strong{Random Initialization:} Initializes the mean randomly.
#' Refer \code{\link{meu_uv}} and \code{\link{meu_mv}} for initialization
#' on univariate and multivariate data respectively.
#'
#' \item \strong{Improved Initialization:} Based on the Kmeans++ idea published in,
#' K-means++: The Advantages of Careful Seeding, David Arthur and Sergei Vassilvitskii.
#' URL http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf. See \code{\link{meu_uv_impr}} and
#' \code{\link{meu_mv_impr}} for details.
#'
#' \item Choice of initialization scheme can be specified as the \strong{seeding}
#' parameter during the training. See \code{\link{dcem_train}} for further details.
#' }
#'
#'
#' @references
#' Parichit Sharma, Hasan Kurban, Mehmet Dalkilic DCEM: An R package for clustering big data via
#' data-centric modification of Expectation Maximization, SoftwareX, 17, 100944 URL
#' https://doi.org/10.1016/j.softx.2021.100944
#'
#' \strong{External Packages:} DCEM requires R packages 'mvtnorm'[1], 'matrixcalc'[2]
#' 'RCPP'[3] and 'MASS'[4] for multivariate density calculation,
#' checking matrix singularity, compiling routines written in C and
#' simulating mixture of gaussians, respectively.
#'
#' [1] Alan Genz, Frank Bretz, Tetsuhisa Miwa, Xuefei Mi, Friedrich Leisch, Fabian Scheipl,
#' Torsten Hothorn (2019). mvtnorm: Multivariate Normal and t Distributions.
#' R package version 1.0-7. URL http://CRAN.R-project.org/package=mvtnorm
#'
#' [2] Frederick Novomestky (2012). matrixcalc: Collection of functions for matrix calculations. R
#' package version 1.0-3. https://CRAN.R-project.org/package=matrixcalc
#'
#' [3] Dirk Eddelbuettel and Romain Francois (2011). Rcpp: Seamless R and C++ Integration. Journal of
#' Statistical Software, 40(8), 1-18. URL http://www.jstatsoft.org/v40/i08/.
#'
#' [4] Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition.
#' Springer, New York. ISBN 0-387-95457-0
#'
#' [5] K-Means++: The Advantages of Careful Seeding, David Arthur and Sergei Vassilvitskii.
#' URL http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf
#'
#' @docType package
#' @name DCEM
NULL
