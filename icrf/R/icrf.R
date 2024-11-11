#' @rdname icrf
#' @name icrf
#' @aliases icrf.default
#' @aliases icrf.formula
#' @aliases print.icrf
#'
#' @title Interval Censored Recursive Forests (ICRF)
#'
#' @description \code{icrf} implements the ICRF algorithm to estimate the
#' conditional survival probability for interval censored survival data.
#' (It can also be used for right-censored survival data and current status data.)
#' \code{icrf} recursively builds random forests using the extremely randomized
#' trees (ERT) algorithm and uses kernel smoothing in the time domain.
#' This \code{icrf} package is built based on the \code{randomForest} package
#' by Andy Liaw and Matthew Wiener. (Quoted statements are from
#' \code{randomForest} by Liaw and Wiener unless otherwise mentioned.)
#'
#'
#' @param formula,data.type,interval.label,right.label,currentstatus.label a formula object, with the
#' response in a Surv 'interval2' or \code{cbind}.
#' Alternatively, the survival outcome may be omitted in the formula and the labels relevent
#' to the survival outcome can be entered in either \code{interval.label}, \code{right.label},
#' or \code{currentstatus.label} with the \code{data.type} being specified.
#' @param data a data frame that includes the intervals and the predictor values.
#' @param na.action 'a function to specify the action to be taken if NAs are
#' found. (NOTE: If given, this argument must be named.)'
#' @param x a data frame or a matrix of predictors. \code{x} is not needed when \code{formula} is specified.
#' @param L,R the left and right end point of the interval. \code{R} should be greater
#' than or equal to \code{L}. In case of equality, a small number \code{epsilon} (the smaller of
#' minimum nonzero interval length and 1e-10) is added.
#' @param tau the study end time. ([0, \code{tau}] is the window for the analysis.)
#' @param bandwidth a positive number. The bandwidth of the kernel smoothing. For faster computing,
#' set \code{bandwidth = 0} for no smoothing.
#' @param initialSmoothing if \code{TRUE}, the initial survival curve used for
#' interval-conditional survival probability estimate is smoothed using the Gaussian kernel.
#' @param quasihonesty if \code{TRUE}, the terminal node prediction is given by the NPMLE of the
#' interval data. If \code{FALSE}, the terminal node prediction is given by the average of the
#' conditional probabilities (exploitative).
#' @param xtest a dataset or matrix of predictors for the test dataset.
#' @param ytest a true survival curve for the test set in a form of the dataframe or matrix.
#' The number of rows is the same as \code{xtest} and each column corresponds to the
#' time points of \code{timeSmooth}.
#' @param timeSmooth a numeric vector of time points at which the smoothed
#' survival curves are estimated. It should be in an increasing order.
#' If \code{null}, a set of distinct interval end points is used.
#' @param nfold Number of forests to iterate. In practice, numbers between 5 and 10 is reasonable.
#' @param ntree Number of trees to build within each forest. 'This should not be set to too small a number, to ensure that every input
#' row gets predicted at least a few times.'
#' @param mtry Number of candidate predictors tried at each split.
#' The default value is sqrt(p) where p is number of variables in \code{x}.
#' @param split.rule Splitting rules. See details. The default is
#' \code{"Wilcoxon", or equivalently "GWRS"}.
#' @param ERT If \code{ERT=TRUE} ERT algorithm applies. If \code{FALSE},
#' a comprehensive greedy algorithm (Breiman's random forest algorithm) applies.
#' @param uniformERT Only relevant when \code{ERT=TRUE}. If \code{uniformERT=TRUE},
#' random candidate cutpoints are selected using uniform distribution. If
#' \code{FALSE}, random candidate cutpoints are chosen among the midpoints of two
#' neighboring predictor values.
#' @param returnBest If \code{returnBest=TRUE}, the survival curve estimate at the
#' best iteration is returned. If \code{FALSE}, the estimate at the last iteration
#' is returned. The best iteration is determined by the type of IMSE measures specified
#' in \code{imse.monitor}. By default, \code{returnBest=TRUE} when the out-of-bag
#' sample is available (sampsize < n).
#' @param imse.monitor Which type of IMSE is used to monitor which fold is the best?
#' @param replace Whether the cases are sampled with or without replacement?
#' @param sampsize Size of random sampling.
#' @param nodesize Each terminal node cannot be smaller than this value. 'Setting this number
#' larger causes smaller trees to be grown (and thus take less time).'
#' @param maxnodes Up to how many terminal nodes can a tree have?
#' 'If not given, trees are grown to the maximum possible
#' (subject to limits by nodesize). If set larger than maximum possible,
#' a warning is issued.'
#' @param epsilon A small positive value needed to discriminate the left and right
#' interval end points for the uncensored data.
#' @param importance If \code{TRUE}, variable importance measure will be computed.
#' @param nPerm How many permutations (of OOB data) to do for variable importance assessment?
#' 'Number larger than 1 gives slightly more
#' stable estimate, but not very effective. Currently only implemented for regression.'
#' @param proximity If \code{TRUE}, proximity measure among the cases is calculated.
#' @param oob.prox If \code{TRUE}, proximity is calculated only on "out-of-bag" data.
#' @param do.trace If \code{TRUE}, intermediate outputs are printed during the tree building
#' procedure. 'If set to some integer, then running output is printed for every do.trace trees.'
#' @param keep.forest 'If set to FALSE, the forest will not be retained in the output
#' object. If xtest is given, defaults to FALSE.'
#' @param keep.inbag 'Should an n by ntree matrix be returned that keeps track of
#' which samples are "in-bag" in which trees (but not how many times, if sampling
#' with replacement)'
#' @param ... optional arguments to be passed to icrf.default.
#'
#' @return An \code{icrf} class object which contains the following components in a list:
#'  \itemize{
#'    An \code{icrf} class object which contains the following components in a list:
#'   \item{call}{the original call to \code{icrf}}
#'   \item{method}{The input values of \code{split.rule}, \code{ERT},
#'   \item{quasihonest}, \code{bandwith}, and the subsample ratio
#'   (= \code{sampsize} / \code{n})}
#'   \item{predicted}{the estimated survival curves of the training set using
#'     out-of-bag samples.}
#'   \item{predictedNO}{the estimated survival curves of the training set using
#'     non-out-of-bag samples.}
#'   \item{predictedNO.Sm}{the smoothed survival curves of the training set using
#'     non-out-of-bag samples.}
#'   \item{time.points}{time points at which the survival curves are estimated.}
#'   \item{time.points.smooth}{time points at which the smoothed survival curves
#'   are estimated.}
#'   \item{imse.oob}{Integrated mean squared error (IMSE) measured based on the
#'   out-of-bag samples}
#'   \item{imse.NO}{Integrated mean squared error (IMSE) measured based on the
#'   non-out-of-bag samples}
#'   \item{oob.times}{number of times for which each case was 'out-of-bag'}
#'   \item{importance}{an array of three matrices where each matrix has
#'    \code{nfold} columns and \code{p} (number of predictors) rows.
#'    The importance is measured based on increase in IMSE types 1 and 2,
#'    respectively, and the node impurity.}
#'   \item{importanceSD}{'The "standard errors" of the permutation-based
#'   importance measure.' A \code{p} by \code{nfold} by 2
#'   array corresponding to the first two matrices of the importance array.}
#'   \item{nfold}{number of forests iterated over.}
#'   \item{ntree}{number of trees built.}
#'   \item{mtry}{number of candidate predictors tried at each node.}
#'   \item{forest}{'a list that contains the entire forest;' \code{NULL} 'if
#'                  \code{keep.forest=FALSE}.'}
#'   \item{intervals}{\code{n} by {2} matrix of the intervals.}
#'   \item{proximity}{if \code{proximity=TRUE} {if \code{proximity=TRUE} when
#'   \code{icrf} is called, a matrix of proximity measures among
#'   the input (based on the frequency that pairs of data points are in
#'   the same terminal nodes).}
#'   \item{inbag}{if \code{keep.inbag=TRUE} provides a matrix of in-bag indicators
#'   for the last forest iteration.}
#'   \item{runtime}{start and end times and the elapsed time.}
#'   \item{test}{if test set is given (through the \code{xtest} or additionally
#'   \code{ytest} arguments), this component is a list which contains the
#'   corresponding \code{predicted} and  error measures (IMSE's).
#'   If \code{proximity=TRUE}, there is also a component, \code{proximity},
#'   which contains the proximity among the test set as well as proximity
#'   between test and training data.}
#' }}
#'
#' @details
#' Four \code{split.rule} options are available: \code{Wilcoxon}, \code{logrank},
#'  \code{PetoWilcoxon}, \code{PetoLogrank}. The aliases are
#'  \code{GWRS}, \code{GLR}, \code{SWRS}, and \code{SLR}, respectively.
#'  The first two are generalized
#'  Wilcoxon-rank-sum test and generalized log-rank test proposed in Cho et al (2020+),
#'  and the latter two are score-based Wilcoxon-rank-sum test and score-based
#'  log-rank test proposed by Peto and Peto (1972) "Asymptotically efficient
#'  rank invariant test procedures."
#'
#' @seealso \code{\link{predict.icrf}}, \code{\link{plot.icrf}}, \code{\link{survplot}}, \code{\link{importance.icrf}}
#'
#' @examples
#' # rats data example.
#' # The type of this dataset is current status data.
#' # Note that this is a toy example. Use a larger ntree and nfold in practice.
#' data(rat2)
#' \dontshow{
#'  set.seed(2)
#' # 1. formula (currentstatus)
#' rats.icrf <-
#'   icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'        data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'        returnBest = TRUE, ntree=2, nfold=2)
#' }
#' \donttest{
#'  set.seed(2)
#' # 1. formula (currentstatus)
#' rats.icrf <-
#'   icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'        data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'        returnBest = TRUE, ntree=10, nfold=3)
#'
#' # 2. formula containing the interval
#' # Alternatively, create the interval endpoints and use the Surv object.
#' L = ifelse(rat2$tumor, 0, rat2$survtime)
#' R = ifelse(rat2$tumor, rat2$survtime, Inf)
#' library(survival) # for Surv function
#' icrf(Surv(L, R, type = "interval2") ~ dose.lvl + weight + male + cage.no, data = rat2,
#'      ntree=10, nfold=3)
#'
#' # Or, 3. formula (interval)
#' rat2b <- cbind(rat2, L = L, R = R)
#' set.seed(1)
#' icrf( ~ dose.lvl + weight + male + cage.no, data = rat2b,
#'      data.type = "interval", interval.label = c("L", "R"),
#'      ntree=10, nfold=3)
#'
#' # 4. default method
#' set.seed(1)
#' icrf(rat2[, c("dose.lvl", "weight", "male", "cage.no")], L = L, R = R,
#'      ntree=10, nfold=3)
#'  }
#'
#' @author Hunyong Cho, Nicholas P. Jewell, and Michael R. Kosorok.
#'
#' @references
#'  \href{https://arxiv.org/abs/1912.09983}{Cho H., Jewell N. J., and Kosorok M. R. (2020+). "Interval censored
#'  recursive forests"}
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics axis barplot dotchart lines matplot mtext pairs par plot plot.default points legend
#' @importFrom stats delete.response mad median model.frame model.response na.fail na.omit napredict
#' @importFrom stats predict quantile reformulate terms update var weighted.mean
#' @export
#' @useDynLib icrf
#'
"icrf" <-
function(x, ...)
  UseMethod("icrf")
