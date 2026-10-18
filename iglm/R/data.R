#' Twitter (X) data list for U.S. state legislators (10-state subset)
#'
#' @description
#' This data object is data derived from the Twitter
#' (X) interactions between U.S. state legislators, which is a subset of the
#' data analyzed in Fritz et al. (2025).'
#' The data is filtered to include only legislators from 10 states (NY, CA, TX,
#' FL, IL, PA, OH, GA, NC, MI) and is further subset to the largest
#' connected component based on mention or retweet activity.
#'
#' This object contains the main \code{\link{iglm.data}} object and 5
#' pre-computed dyadic covariates.
#'
#' @name state_twitter
#' @docType data
#'
#' @format
#' A \code{list} object containing 6 components. Let N be the number of
#' legislators in the filtered 10-state subset.
#'
#' \describe{
#'   \item{iglm.data}{
#'     A \code{\link{iglm.data}} object (which is also a \code{list})
#'     parameterized as follows:
#'     \itemize{
#'       \item \code{x_attribute}: A binary numeric vector of length N.
#'         Value is \code{1} if the legislator's party is 'Republican',
#'         \code{0} otherwise.
#'       \item \code{y_attribute}: A Poisson numeric vector of length N.
#'         Represents the count of hatespeech incidents
#'         (\code{legislator_data$number_hatespeech}) for each legislator.
#'       \item \code{z_network}: A directed edgelist (2-column matrix) of
#'         size \code{n_edges x 2}. A tie \code{(i, j)} exists if legislator
#'         \code{i} either mentioned or retweeted legislator \code{j}.
#'       \item \code{neighborhood}: A directed edgelist (2-column matrix).
#'         Represents the follower network, where a tie \code{(i, j)} exists
#'         if legislator \code{i} follows legislator \code{j}.
#'         Self-loops (diagonal) are included.
#'     }
#'   }
#'   \item{match_gender}{
#'     An N x N \code{matrix}. \code{matrix[i, j] = 1} if legislator \code{i}
#'     and legislator \code{j} have the same gender, \code{0} otherwise.
#'   }
#'   \item{match_race}{
#'     An N x N \code{matrix}. \code{matrix[i, j] = 1} if legislator \code{i}
#'     and legislator \code{j} have the same race, \code{0} otherwise.
#'   }
#'   \item{match_state}{
#'     An N x N \code{matrix}. \code{matrix[i, j] = 1} if legislator \code{i}
#'     and legislator \code{j} are from the same state, \code{0} otherwise.
#'   }
#'   \item{white_attribute}{
#'     A 1 x N \code{matrix} (a row vector). \code{matrix[1, i] = 1} if
#'     legislator \code{i} is 'White', \code{0} otherwise.
#'   }
#'   \item{gender_attribute}{
#'     A 1 x N \code{matrix} (a row vector). \code{matrix[1, i] = 1} if
#'     legislator \code{i} is 'female', \code{0} otherwise.
#'   }
#' }
#'
#' @references
#' Gopal, Kim, Nakka, Boehmke, Harden, Desmarais.
#' The National Network of U.S. State Legislators on Twitter.
#' Political Science Research & Methods, Forthcoming.
#'
#' Kim, Nakka, Gopal, Desmarais,Mancinelli, Harden, Ko, and Boehmke (2022).
#' Attention to the COVID-19 pandemic on Twitter: Partisan differences among
#' U.S. state legislators. Legislative Studies Quarterly 47, 1023–1041.
#'
#' Fritz, C., Schweinberger, M. , Bhadra S., and D. R. Hunter (2025).
#' A Regression Framework for Studying Relationships among Attributes under Network Interference.
#' Journal of the American Statistical Association, to appear.
#'
#' @usage
#' data(state_twitter)
#'
#' @keywords data
NULL


#' Copenhagen Network Study
#'
#' @description
#' A preprocessed dataset containing social ties, physical proximity, and nodal
#' attributes for a subset of participants in the Copenhagen Networks Study.
#' The object is provided as an \code{iglm.data} class.
#'
#' @docType data
#' @name copenhagen
#' @usage data(copenhagen)
#'
#' @format The \code{iglm.data} provides the following information:
#' \describe{
#'   \item{z_network}{A \eqn{E \times 2} integer matrix representing the
#'     undirected friendship network ($Z$).}
#'   \item{x_attribute}{A logical/binomial vector of length \eqn{N} indicating
#'     gender (1 for female, 0 for male).}
#'   \item{y_attribute}{A numeric vector of length \eqn{N} representing the
#'     log-transformed total call duration in minutes:
#'     \eqn{y_i = \log(\frac{\text{seconds}}{60})}.}
#'   \item{neighborhood}{A matrix defining the proximity-based constraint
#'     space. Pairs are included if their cumulative
#'     physical proximity exceeded 24 hours during the observation period.}
#'   \item{fix_x}{Boolean \code{TRUE}, indicating that the \eqn{x} attribute
#'     is treated as exogenous.}
#' }
#'
#' @details
#' The following preprocessing steps were carried out:
#'
#' \itemize{
#'   \item \bold{Temporal Aggregation:} Proximity data (Bluetooth scans)
#'     were aggregated into sessions. A session break was defined by any
#'     temporal gap exceeding 10 minutes.
#'   \item \bold{Recursive Pruning:} A recursive filter removed units with missing
#'     gender information or isolated units in either the
#'     friendship (\code{z_network}) or proximity (\code{neighborhood}) networks,
#' }
#'
#' @references
#'
#' Sapiezynski, P., Stopczynski, A., Lassen, D. D. and Lehmann, S. (2019),
#' Interaction data from the Copenhagen Networks Study. Scientific Data 6(1), 315.
#'
#' @keywords datasets
NULL

