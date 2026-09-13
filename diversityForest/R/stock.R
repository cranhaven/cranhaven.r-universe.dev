##' Data on stock prices of aerospace companies
##'
##' This data set contains 950 daily stock prices from January 1988 through October 1991, for ten aerospace companies. 
##' The names of the companies are anonymised and the stock prices for one of these companies (\code{company10}) were 
##' flagged as the outcome variable. Thus, for this data set, both the outcome and the covariates were metric.
##' 
##' The variables are as follows: covariates: \code{company1}, ..., \code{company9}, outcome variable: \code{company10}.
##'
##' @format A data frame with 950 observations, nine covariates and one metric outcome variable
##' @source OpenML: data.name: stock, data.id: 223, link: \url{https://www.openml.org/d/223/}
##'
##' @examples
##' 
##' ## Load data:
##' data(stock)
##' 
##' ## Dimension of data:
##' dim(stock)
##'
##' ## First rows of data:
##' head(stock) 
##' 
##' @references
##' \itemize{
##'   \item Vanschoren, J., van Rijn, J. N., Bischl, B., Torgo, L. (2013). OpenML: networked science in machine learning. SIGKDD Explorations 15(2):49-60, <\doi{10.1145/2641190.2641198}>.
##'   }
##'
##' @name stock
NULL