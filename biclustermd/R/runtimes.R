#' Algorithm run time data
#'
#' This dataset stems from the R journal article introducing \code{biclustermd}
#'   to R users. It describes the data attributes and run time for varying data
#'   sizes and structures.
#'
#' A data frame of 2400 rows and 13 variables (defined range, inclusive):
#' \describe{
#'    \item{combination_no}{Unique identifier of a combination of parameters.}
#'    \item{rows}{Number of rows in the data matrix. (50, 1500)}
#'    \item{cols}{Number of columns in the data matrix. (50, 1500)}
#'    \item{N}{Product of the dimensions of the data. (2500, 2250000)}
#'    \item{row_clusts}{Number of clusters to partition the rows into. (4, 300)}
#'    \item{col_clusts}{Number of clusters to partition the columns into. (4, 300)}
#'    \item{avg_row_clust_size}{Average row cluster size. \code{rows / row_clusts}}
#'    \item{avg_col_clust_size}{Average column cluster size. \code{cols / col_clusts}}
#'    \item{sparsity}{Percent of data values which are missing.}
#'    \item{user.self}{CPU time used executing instructions to calls (from \code{?proc.time}.}
#'    \item{sys.self}{CPU time used executing calls (from \code{?proc.time}.}
#'    \item{elapsed}{Amount of time in seconds it took the algorithm to converge.}
#'    \item{iterations}{Number of iterations to convergence.}
#' }
#'
"runtimes"
