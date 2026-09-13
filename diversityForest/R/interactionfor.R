# -------------------------------------------------------------------------------
#   This file is part of 'diversityForest'.
#
# 'diversityForest' is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# 'diversityForest' is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with 'diversityForest'. If not, see <http://www.gnu.org/licenses/>.
#
#  NOTE: 'diversityForest' is a fork of the popular R package 'ranger', written by Marvin N. Wright.
#  Most R and C++ code is identical with that of 'ranger'. The package 'diversityForest'
#  was written by taking the original 'ranger' code and making any
#  changes necessary to implement diversity forests.
#
# -------------------------------------------------------------------------------

##' Implements interaction forests as described in Hornung & Boulesteix (2022).
##' Currently, categorical, metric, and survival outcomes are supported. Interaction forests feature the effect importance measure (EIM),
##' which can be used to rank the covariate variable pairs with respect to the impact of their interaction effects on prediction.
##' This allows to identify relevant interaction effects. Interaction forests focus on well interpretable interaction effects.
##' See the 'Details' section below for more details. In addition, we strongly recommend to consult Section C of 
##' Supplementary Material 1 of Hornung & Boulesteix (2022), which uses detailed examples of interaction forest analyses
##' with code to illustrate how interaction forests can be used in 
##' applications: \href{https://ars.els-cdn.com/content/image/1-s2.0-S0167947322000408-mmc1.pdf}{Link}.
##'
##' The effect importance measure (EIM) of interaction forests distinguishes quantitative and qualitative interaction effects (Peto, 1982).
##' This is a common distinction as these two types of interaction effects are interpreted in different ways (see below). 
##' For both of these types, EIM values for each variable pair are obtained: the quantitative and qualitative EIM values.\cr
##' Interaction forests target easily interpretable types of interaction effects. These can be communicated clearly using statements 
##' of the following kind: "The strength of the positive (negative) effect of variable A on the outcome depends on the level of variable B"
##' for quantitative interactions, and "for observations with small values of variable B, the effect of variable A is positive (negative), 
##' but for observations with large values of B, the effect of A is negative (positive)" for qualitative interactions.\cr
##' In addition to calculating EIM values for variable pairs, importance values for the individual variables are calculated as well, the univariable
##' EIM values. These measure the variable importance as in the case of classical variable importance measures of random forests.\cr
##' The effect importance mode can be set via the \code{importance} argument: \code{"qualitative"}: Calculate only qualitative EIM values;
##' \code{"quantitative"}: Calculate only quantitative EIM values; \code{"both"} (the default): Calculate qualitative and quantitative EIM
##' values; \code{"mainonly"}: Calculate only univariable EIM values.\cr
##' The top variable pairs with largest quantitative and qualitative EIM values likely have quantitative and qualitative interactions,
##' respectively, which have a considerable impact on prediction. The top variables with largest univariable EIM values likely have a considerable
##' impact on prediction. Note that it is currently not possible to test the EIM values for 
##' statistical significance using the interaction forests algorithm itself. However, the p-values
##' shown in the plots obtained with \code{\link{plotEffects}} (which are obtained using bivariable
##' models) can be adjusted for multiple testing using the Bonferroni procedure to obtain
##' practical p-values. See the end of the 'Details' section of \code{\link{plotEffects}} for explanation and guidance.\cr
##' If the number of variables is larger than 100, not all possible variable pairs are considered, but, using a screening procedure, the
##' 5000 variable pairs with the strongest indications of interaction effects are pre-selected.\cr
##' \strong{NOTE}: To make interpretations, it is crucial to investigate (visually) the forms the interaction effects of variable pairs 
##' with large quantitative and qualitative EIM values take. This can be done using the plot function \code{\link{plot.interactionfor}} 
##' (first overview) and \code{\link{plotEffects}}.\cr
##' NOTE ALSO: As described in Hornung & Boulesteix (2022), in the case of data with larger numbers of variables (larger than 100, 
##' but more seriously for high-dimensional data), the univariable EIM values can be biased. Therefore, it is strongly recommended 
##' to interpret the univariable EIM values with caution, if the data are high-dimensional. If it is of interest to measure the univariable 
##' importance of the variables for high-dimensional data, an additional conventional random forest (e.g., using the \code{ranger} package)
##' should be constructed and the variable importance measure values of this random forest be used for ranking the univariable effects.\cr
##' For large data sets with many observations the calculation of the EIM values can become very costly - when using fully grown trees.
##' Therefore, when calculating EIM values for data sets with more than 1000 observations we use the following
##' maximum tree depths by default (argument: \code{simplify.large.n = TRUE}):
##' \itemize{
##' \item if \eqn{n \le 1000}: Use fully grown trees.
##' \item if \eqn{1000 < n \le 2000}: Use tree depth 10.
##' \item if \eqn{2000 < n \le 5000}: Use tree depth 7.
##' \item if \eqn{n > 5000}: Use tree depth 5.
##' }
##' Extensive analyses in Hornung & Boulesteix (2022) suggest that by restricting the tree depth in this way,
##' the EIM values that would result when using fully grown trees are approximated well. However, the prediction
##' performance suffers, when using restricted trees. Therefore, we restrict the tree depth only when calculating
##' the EIM values (if \eqn{n > 1000}), but construct a second interaction forest with unrestricted tree depth,
##' which is then used for prediction purposes.
##'
##' @title Construct an interaction forest prediction rule and calculate EIM values as described in Hornung & Boulesteix (2022).
##' @param formula Object of class \code{formula} or \code{character} describing the model to fit.
##' @param data Training data of class \code{data.frame}, \code{matrix}, \code{dgCMatrix} (Matrix) or \code{gwaa.data} (GenABEL).
##' @param importance Effect importance mode. One of the following: "both" (the default), "qualitative", "quantitative", "mainonly", "none". 
##' See the 'Details' section below for explanation.
##' @param num.trees Number of trees. The default number is 20000, if EIM values should be computed 
##' and 2000 otherwise. Note that if \code{simplify.large.n = TRUE} (default), the number of observations 
##' is larger than 1000, and EIM values should be calculated two forests are constructed, one for calculating 
##' the EIM values and one for prediction (cf. 'Details' section). In such cases, the default number of 
##' trees used for the forest for EIM value calculation is 20000 and the default number of trees used 
##' for the forest for prediction is 2000.
##' @param simplify.large.n Should restricted tree depths be used, when calculating EIM values for large data sets? See the 'Details' section below for more information. Default is \code{TRUE}.
##' @param num.trees.eim.large.n Number of trees in the forest used for calculating the EIM values for large data sets. 
##' If \code{num.trees} is provided, but not \code{num.trees.eim.large.n}, the value given by \code{num.trees} 
##' will be used. The default number is 20000. Only used when \code{simplify.large.n = TRUE}.
##' @param write.forest Save \code{interaction.forest} object, required for prediction. Set to \code{FALSE} to reduce 
##' memory usage if no prediction intended.
##' @param probability Grow a probability forest as in Malley et al. (2012).
##' @param min.node.size Minimal node size. Default 1 for classification, 5 for regression, 3 for survival, and 5 for probability.
##' @param max.depth Maximal tree depth. A value of NULL or 0 (the default) corresponds to unlimited depth, 1 to tree stumps (1 split per tree).
##' @param replace Sample with replacement. Default is \code{FALSE}.
##' @param sample.fraction Fraction of observations to sample. Default is 1 for sampling with replacement and 0.7 for sampling without replacement. For classification, this can be a vector of class-specific values. 
##' @param case.weights Weights for sampling of training observations. Observations with larger weights will be selected with higher probability in the bootstrap (or subsampled) samples for the trees.
##' @param class.weights Weights for the outcome classes (in order of the factor levels) in the splitting rule (cost sensitive learning). Classification and probability prediction only. For classification the weights are also applied in the majority vote in terminal nodes.
##' @param splitrule Splitting rule. For classification and probability estimation "gini" or "extratrees" with default "gini". For regression "variance", "extratrees" or "maxstat" with default "variance". For survival "logrank", "extratrees", "C" or "maxstat" with default "logrank". NOTE: For interaction forests currently only the default splitting rules are supported.
##' @param always.split.variables Currently not useable. Character vector with variable names to be always selected.
##' @param keep.inbag Save how often observations are in-bag in each tree. 
##' @param inbag Manually set observations per tree. List of size num.trees, containing inbag counts for each observation. Can be used for stratified sampling.
##' @param holdout Hold-out mode. Hold-out all samples with case weight 0 and use these for variable importance and prediction error. NOTE: Currently, not useable for interaction forests.
##' @param quantreg Prepare quantile prediction as in quantile regression forests (Meinshausen 2006). Regression only. Set \code{keep.inbag = TRUE} to prepare out-of-bag quantile prediction. NOTE: Currently, not useable for interaction forests.
##' @param oob.error Compute OOB prediction error. Set to \code{FALSE} to save computation time, e.g. for large survival forests.
##' @param num.threads Number of threads. Default is number of CPUs available.
##' @param verbose Show computation status and estimated runtime.
##' @param seed Random seed. Default is \code{NULL}, which generates the seed from \code{R}. Set to \code{0} to ignore the \code{R} seed. 
##' @param dependent.variable.name Name of outcome variable, needed if no formula given. For survival forests this is the time variable.
##' @param status.variable.name Name of status variable, only applicable to survival data and needed if no formula given. Use 1 for event and 0 for censoring.
##' @param npairs Number of variable pairs to sample for each split. Default is the square root of the number of independent variables divided by 2 (this number is rounded up).
##' @param classification Only needed if data is a matrix. Set to \code{TRUE} to grow a classification forest.
##' @return Object of class \code{interactionfor} with elements
##'   \item{\code{predictions}}{Predicted classes/values, based on out-of-bag samples (classification and regression only).}
##'   \item{\code{num.trees}}{Number of trees.} 
##'   \item{\code{num.independent.variables}}{Number of independent variables.} 
##'   \item{\code{unique.death.times}}{Unique death times (survival only).} 
##'   \item{\code{min.node.size}}{Value of minimal node size used.} 
##'   \item{\code{npairs}}{Number of variable pairs sampled for each split.} 
##'   \item{\code{eim.univ.sorted}}{Univariable EIM values sorted in decreasing order.} 
##'   \item{\code{eim.univ}}{Univariable EIM values.} 
##'   \item{\code{eim.qual.sorted}}{Qualitative EIM values sorted in decreasing order.} 
##'   \item{\code{eim.qual}}{Qualitative EIM values.} 
##'   \item{\code{eim.quant.sorted}}{Quantitative EIM values sorted in decreasing order.\cr
##'   The labeling of these values
##'   provides the information on the type of quantitative interactions the respective variable
##'   pairs feature. For example, consider a variable pair A and B and say the label reads "A large AND B small".
##'   This would mean that if the value of A is large and, at the same time, the value
##'   of B is small, the expected value of the outcome variable is (considerably) different from all other cases. For this type of quantitative interaction,
##'   the effect of B is weak for small values of A and strong for large values of A. See Hornung & Boulesteix (2022) 
##'   for more information on the types of quantitative interaction effects targeted by interaction forest.}
##'   \item{\code{eim.quant}}{Quantitative EIM values. These values are labeled analoguously as those in \code{eim.quant.sorted}.} 
##'   \item{\code{prediction.error}}{Overall out-of-bag prediction error. 
##'   For classification this is the fraction of misclassified samples, for probability 
##'   estimation the Brier score, for regression the mean squared error and for survival 
##'   one minus Harrell's C-index. This is 'NA' for data sets with more than 100
##'   covariate variables, because for such data sets we pre-select the 5000 variable
##'   pairs with strongest indications of interaction effects. This pre-selection cannot be taken into
##'   account in the out-of-bag error estimation, which is why the out-of-bag error estimates would
##'   be (much) too optimistic for data sets with more than 100 covariate variables.} 
##'   \item{\code{forest}}{Saved forest (If write.forest set to TRUE). Note that the variable IDs in the \code{split.multvarIDs} object do not necessarily represent the column number in R.} 
##'   \item{\code{confusion.matrix}}{Contingency table for classes and predictions based on out-of-bag samples (classification only).} 
##'   \item{\code{chf}}{Estimated cumulative hazard function for each sample (survival only).}
##'   \item{\code{survival}}{Estimated survival function for each sample (survival only).}
##'   \item{\code{splitrule}}{Splitting rule.}
##'   \item{\code{treetype}}{Type of forest/tree. classification, regression or survival.}
##'   \item{\code{r.squared}}{R squared. Also called explained variance or coefficient of determination (regression only). Computed on out-of-bag data.}
##'   \item{\code{call}}{Function call.}
##'   \item{\code{importance.mode}}{Importance mode used.}
##'   \item{\code{num.samples}}{Number of samples.}
##'   \item{\code{replace}}{Sample with replacement.}
##'   \item{\code{eim.quant.rawlists}}{List containing the four vectors of un-adjusted 'raw' quantitative EIM values 
##'   and the four vectors of adjusted EIM values. These are usually not required by the user.\cr
##'   For each of the four types of quantitative splits there exists a separate
##'   vector of raw quantitative EIM values. For example, \code{eim.quant.large.small.raw} contains the raw 
##'   quantitative EIM values of the quantitative split type associated with quantitative interaction effects 
##'   for which the expected values of the outcome variable are different, if the value of variable A is large 
##'   and, at the same time, the value of variable B is small.
##'   The list entries of the un-adjusted 'raw' quantitative EIM values are labeled with the suffix \code{.raw},
##'   while the list entries of the adjusted quantitative EIM values miss this suffix. See Hornung & Boulesteix (2022) for details
##'   on the raw and adjusted EIM values.}
##'   \item{\code{promispairs}}{List giving the indices of the variables in the pre-selected variable pairs. If the number of variables is at most
##'   100, all variable pairs are considered.} 
##'   \item{\code{plotres}}{List ob objects needed by the plot functions: \code{eim.univ.order} contains the sorting of the 
##'   univariable EIM values in descending order, where the first element gives the index of the variable with largest EIM value, 
##'   the second element the index of the variable with second-largest EIM value and so on; \code{eim.qual.order} / \code{eim.quant.order} 
##'   contains the sorting in descending order of the qualitative / quantitative EIM values for the (pre-selected) variable pairs given 
##'   by the object \code{promispairs} above. The first element gives the index of the (pre-selected) variable pair with largest 
##'   qualitative / quantitative EIM value, the second element the index of the variable pair with second-largest 
##'   qualitative / quantitative EIM value; \code{data} contains the data; \code{yvarname} is the name of the outcome variable 
##'   (survival time for survival); \code{statusvarname} is the name of the status variable.}
##' @examples
##' \dontrun{
##' 
##' ## Load package:
##'
##' library("diversityForest")
##' 
##' 
##' 
##' ## Set seed to make results reproducible:
##' 
##' set.seed(1234)
##' 
##' 
##' 
##' ## Construct interaction forests and calculate EIM values:
##' 
##' 
##' # Binary outcome:
##' data(zoo)
##' modelcat <- interactionfor(dependent.variable.name = "type", data = zoo, 
##'   num.trees = 20)
##' 
##' 
##' # Metric outcome:
##' data(stock)
##' modelcont <- interactionfor(dependent.variable.name = "company10", data = stock, 
##'   num.trees = 20)  
##'   
##'   
##' # Survival outcome:
##' library("survival")
##' mgus2$id <- NULL  # 'mgus2' data set is contained in the 'survival' package
##' 
##' # categorical variables need to be of factor format - important!!
##' mgus2$sex <- factor(mgus2$sex)
##' mgus2$pstat <- factor(mgus2$pstat)
##' 
##' # Remove the second time variable 'ptime':
##' mgus2$ptime <- NULL
##' 
##' # Remove missing values:
##' mgus2 <- mgus2[complete.cases(mgus2),]
##' 
##' # Take subset to make the calculations less computationally
##' # expensive for the example (in actual applications, we would of course
##' # use the whole data set):
##' mgus2sub <- mgus2[sample(1:nrow(mgus2), size=500),]
##' 
##' # Apply 'interactionfor':
##' modelsurv <- interactionfor(formula = Surv(futime, death) ~ ., data=mgus2sub, num.trees=20)
##' 
##' # NOTE: num.trees = 20 (in the above) would be much too small for practical 
##' # purposes. This small number of trees was simply used to keep the
##' # runtime of the example short.
##' # The default number of trees is num.trees = 20000 if EIM values are calculated
##' # and num.trees = 2000 otherwise.
##' 
##' 
##' 
##' ## Inspect the rankings of the variables and variable pairs with respect to 
##' ## the univariable, quantitative, and qualitative EIM values:
##' 
##' # Univariable EIM values: 
##' modelcat$eim.univ.sorted
##' 
##' # Pairs with top quantitative EIM values:
##' modelcat$eim.quant.sorted[1:5]
##' 
##' # Pairs with top qualitative EIM values:
##' modelcat$eim.qual.sorted[1:5]
##' 
##' 
##' 
##' ## Investigate visually the forms of the interaction effects of the variable pairs with
##' ## largest quantitative and qualitative EIM values:
##' 
##' plot(modelcat)
##' plotEffects(modelcat, type="quant") # type="quant" is default.
##' plotEffects(modelcat, type="qual")
##' 
##' 
##' 
##' ## Prediction:
##' 
##' # Separate 'zoo' data set randomly in training
##' # and test data:
##' 
##' data(zoo)
##' train.idx <- sample(nrow(zoo), 2/3 * nrow(zoo))
##' zoo.train <- zoo[train.idx, ]
##' zoo.test <- zoo[-train.idx, ]
##' 
##' # Construct interaction forest on training data:
##' # NOTE again: num.trees = 20 is specified too small for practical purposes.
##' modelcattrain <- interactionfor(dependent.variable.name = "type", data = zoo.train, 
##'                                 importance = "none", num.trees = 20)
##' # NOTE: Because we are only interested in prediction here, we do not
##' # calculate EIM values (by setting importance = "none"), because this
##' # speeds up calculations.
##' 
##' # Predict class values of the test data:
##' pred.zoo <- predict(modelcattrain, data = zoo.test)
##' 
##' # Compare predicted and true class values of the test data:
##' table(zoo.test$type, pred.zoo$predictions)
##' }
##'
##' @author Roman Hornung, Marvin N. Wright
##' @references
##' \itemize{
##'   \item Hornung, R., Boulesteix, A.-L. (2022). Interaction forests: Identifying and exploiting interpretable quantitative and qualitative interaction effects. Computational Statistics & Data Analysis 171:107460, <\doi{10.1016/j.csda.2022.107460}>.
##'   \item Hornung, R. (2022). Diversity forests: Using split sampling to enable innovative complex split procedures in random forests. SN Computer Science 3(2):1, <\doi{10.1007/s42979-021-00920-1}>.
##'   \item Peto, R., (1982) Statistical aspects of cancer trials. In: K.E. Halnam (Ed.), Treatment of Cancer. Chapman & Hall: London.
##'   \item Wright, M. N., Ziegler, A. (2017). ranger: A fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software 77:1-17, <\doi{10.18637/jss.v077.i01}>.
##'   \item Breiman, L. (2001). Random forests. Machine Learning 45:5-32, <\doi{10.1023/A:1010933404324}>.
##'   \item Malley, J. D., Kruppa, J., Dasgupta, A., Malley, K. G., & Ziegler, A. (2012). Probability machines: consistent probability estimation using nonparametric learning machines. Methods of Information in Medicine 51:74-81, <\doi{10.3414/ME00-01-0052}>.
##'   \item Meinshausen (2006). Quantile Regression Forests. Journal of Machine Learning Research 7:983-999.
##'   }
##' @seealso \code{\link{predict.divfor}}, \code{\link{plot.interactionfor}}, \code{\link{plotEffects}}
##' @encoding UTF-8
##' @useDynLib diversityForest, .registration = TRUE
##' @importFrom Rcpp evalCpp
##' @import stats 
##' @import utils
##' @importFrom Matrix Matrix
##' @export
interactionfor <- function(formula = NULL, data = NULL, importance = "both", num.trees = NULL,
                           simplify.large.n = TRUE, num.trees.eim.large.n = NULL,
                           write.forest = TRUE, probability = FALSE,
                           min.node.size = NULL, max.depth = NULL, replace = FALSE, 
                           sample.fraction = ifelse(replace, 1, 0.7), 
                           case.weights = NULL, class.weights = NULL, splitrule = NULL, 
                           always.split.variables = NULL,
                           keep.inbag = FALSE, inbag = NULL, holdout = FALSE,
                           quantreg = FALSE, oob.error = TRUE,
                           num.threads = NULL,
                           verbose = TRUE, seed = NULL, 
                           dependent.variable.name = NULL, status.variable.name = NULL, npairs = NULL,
                           classification = NULL) {
  
  respect.unordered.factors <- "order"
  save.memory <- FALSE
  
  maxdepthnull <- is.null(max.depth)
  
  
  
  #asdf
  
  
  
  ## GenABEL GWA data
  if ("gwaa.data" %in% class(data)) {
    snp.names <- data@gtdata@snpnames
    snp.data <- data@gtdata@gtps@.Data
    data <- data@phdata
    if ("id" %in% names(data)) {
      data$"id" <- NULL
    }
    gwa.mode <- TRUE
    save.memory <- FALSE
  } else {
    snp.data <- as.matrix(0)
    gwa.mode <- FALSE
  }
  
  ## Stop, if option "respect.unordered.factors = 'partition'" (cf. ranger) was specified:
  if(!is.null(respect.unordered.factors) && respect.unordered.factors == "partition") {
    stop("Error: Option 'respect.unordered.factors = 'partition'' not (yet) supported for diversity forests.")
  }
  
  ## Sparse matrix data
  if (inherits(data, "Matrix")) {
    if (!("dgCMatrix" %in% class(data))) {
      stop("Error: Currently only sparse data of class 'dgCMatrix' supported.")
    }
    
    if (!is.null(formula)) {
      stop("Error: Sparse matrices only supported with alternative interface. Use dependent.variable.name instead of formula.")
    }
  }
  
  ## Formula interface. Use whole data frame is no formula provided and depvarname given
  if (is.null(formula)) {
    if (is.null(dependent.variable.name)) {
      stop("Error: Please give formula or outcome variable name.")
    }
    if (is.null(status.variable.name)) {
      status.variable.name <- ""
      response <- data[, dependent.variable.name, drop = TRUE]
    } else {
      
      # Neu:
      timename <- dependent.variable.name
      statusname <- status.variable.name
      dependent.variable.name <- "time"
      status.variable.name <- "status"
      names(data)[names(data)==timename] <- "time"
      names(data)[names(data)==statusname] <- "status"
      
      response <- survival::Surv(data[, dependent.variable.name], data[, status.variable.name]) #data[, c(dependent.variable.name, status.variable.name)]
    }
    data.selected <- data
  } else {
    formula <- formula(formula)
    if (!inherits(formula, "formula")) {
      stop("Error: Invalid formula.")
    }
    # New:
    if(length(grep("Surv\\(", deparse(formula)))==1) {
      timename <- strsplit(strsplit(deparse(formula), split="Surv\\(")[[1]][2], split=",")[[1]][1]
      statusname <- strsplit(strsplit(deparse(formula), split=", ")[[1]][2], split="\\)")[[1]][1]
      names(data)[names(data)==timename] <- "time"
      names(data)[names(data)==statusname] <- "status"
      formula <- as.formula(gsub(paste("Surv(", timename, ", ", statusname, ")", sep=""), "Surv(time, status)", deparse(formula), fixed = TRUE))
    }
    data.selected <- parse.formula(formula, data, env = parent.frame())
    response <- data.selected[, 1]
  }
  
  ## Check missing values
  if (any(is.na(data.selected))) {
    offending_columns <- colnames(data.selected)[colSums(is.na(data.selected)) > 0]
    stop("Missing data in columns: ",
         paste0(offending_columns, collapse = ", "), ".", call. = FALSE)
  }
  
  ## Check response levels
  if (is.factor(response)) {
    if (nlevels(response) != nlevels(droplevels(response))) {
      dropped_levels <- setdiff(levels(response), levels(droplevels(response)))
      warning("Dropped unused factor level(s) in outcome variable: ",
              paste0(dropped_levels, collapse = ", "), ".", call. = FALSE)
    }
  }
  
  ## Treetype
  if (is.factor(response)) {
    if (probability) {
      treetype <- 9
    } else {
      treetype <- 1
    }
  } else if (is.numeric(response) && (is.null(ncol(response)) || ncol(response) == 1)) {
    if (!is.null(classification) && classification && !probability) {
      treetype <- 1
    } else if (probability) {
      treetype <- 9
    } else {
      treetype <- 3
    }
  } else if (inherits(response, "Surv") || is.data.frame(response) || is.matrix(response)) {
    treetype <- 5
  } else {
    stop("Error: Unsupported type of outcome variable.")
  }
  
  ## Quantile prediction only for regression
  if (quantreg && treetype != 3) {
    stop("Error: Quantile prediction implemented only for regression outcomes.")
  }
  
  ## Dependent and status variable name. For non-survival dummy status variable name.
  if (!is.null(formula)) {
    if (treetype == 5) {
      dependent.variable.name <- dimnames(response)[[2]][1]
      status.variable.name <- dimnames(response)[[2]][2]
    } else {
      dependent.variable.name <- names(data.selected)[1]
      status.variable.name <- ""
    }
    independent.variable.names <- names(data.selected)[-1]
  } else {
    independent.variable.names <- colnames(data.selected)[colnames(data.selected) != dependent.variable.name &
                                                            colnames(data.selected) != status.variable.name]
  }
  
  ## respect.unordered.factors
  if (is.null(respect.unordered.factors)) {
    #if (!is.null(splitrule) && splitrule == "extratrees") {
    #  respect.unordered.factors <- "partition"
    #} else {
    respect.unordered.factors <- "ignore"
    #}
  }
  
  ## Old version of respect.unordered.factors
  if (respect.unordered.factors == TRUE) {
    respect.unordered.factors <- "order"
  } else if (respect.unordered.factors == FALSE) {
    respect.unordered.factors <- "ignore"
  }
  
  ## Recode characters as factors and recode factors if 'order' mode
  if (!is.matrix(data.selected) && !inherits(data.selected, "Matrix")) {
    character.idx <- sapply(data.selected, is.character)
    
    if (respect.unordered.factors == "order") {
      ## Recode characters and unordered factors
      names.selected <- names(data.selected)
      ordered.idx <- sapply(data.selected, is.ordered)
      factor.idx <- sapply(data.selected, is.factor)
      independent.idx <- names.selected != dependent.variable.name & 
        names.selected != status.variable.name & 
        names.selected != paste0("Surv(", dependent.variable.name, ", ", status.variable.name, ")")
      recode.idx <- independent.idx & (character.idx | (factor.idx & !ordered.idx))
      
      ##if (any(recode.idx) & (importance == "impurity_corrected" || importance == "impurity_unbiased")) {
      ##  warning("Corrected impurity importance may not be unbiased for re-ordered factor levels. Consider setting respect.unordered.factors to 'ignore' or 'partition' or manually compute corrected importance.")
      ##}
      
      ## Numeric response
      if (is.factor(response)) {
        num.response <- as.numeric(response)
      } else {
        num.response <- response
      }
      
      ## Recode each column
      data.selected[recode.idx] <- lapply(data.selected[recode.idx], function(x) {
        if (!is.factor(x)) {
          x <- as.factor(x)
        } 
        
        if ("Surv" %in% class(response)) {
          ## Use median survival if available or largest quantile available in all strata if median not available
          levels.ordered <- largest.quantile(response ~ x)
          
          ## Get all levels not in node
          levels.missing <- setdiff(levels(x), levels.ordered)
          levels.ordered <- c(levels.missing, levels.ordered)
        } else if (is.factor(response) & nlevels(response) > 2) {
          levels.ordered <- pca.order(y = response, x = x)
        } else {
          ## Order factor levels by mean response
          means <- sapply(levels(x), function(y) {
            mean(num.response[x == y])
          })
          levels.ordered <- as.character(levels(x)[order(means)])
        }
        
        ## Return reordered factor
        factor(x, levels = levels.ordered, ordered = TRUE, exclude = NULL)
      })
      
      ## Save levels
      covariate.levels <- lapply(data.selected[independent.idx], levels)
    } else {
      ## Recode characters only
      data.selected[character.idx] <- lapply(data.selected[character.idx], factor)
    }
  }
  
  ## Input data and variable names, create final data matrix
  if (!is.null(formula) && treetype == 5) {
    data.final <- data.matrix(cbind(response[, 1], response[, 2],
                                    data.selected[-1]))
    colnames(data.final) <- c(dependent.variable.name, status.variable.name,
                              independent.variable.names)
  } else if (is.matrix(data.selected) || inherits(data.selected, "Matrix")) {
    data.final <- data.selected
  } else {
    data.final <- data.matrix(data.selected)
  }
  variable.names <- colnames(data.final)
  
  ## If gwa mode, add snp variable names
  if (gwa.mode) {
    variable.names <- c(variable.names, snp.names)
    all.independent.variable.names <- c(independent.variable.names, snp.names)
  } else {
    all.independent.variable.names <- independent.variable.names
  }
  
  
  ## Error if too few covariates
  if (length(all.independent.variable.names) < 2) {
    stop("Error: There are too few covariates. Interaction forests require at least two covariates.")
  }
  
  ## Number of trees
  if (!is.null(num.trees) & (!is.numeric(num.trees) || num.trees < 1)) {
    stop("Error: Invalid value for num.trees.")
  }
  if (!is.null(num.trees.eim.large.n) & (!is.numeric(num.trees.eim.large.n) || num.trees.eim.large.n < 1)) {
    stop("Error: Invalid value for num.trees.eim.large.n.")
  }
  
  ## npairs
  if (is.null(npairs)) {
    npairs <- 0
  } else if (!is.numeric(npairs) || npairs < 1) {
    stop("Error: Invalid value for npairs")
  }
  
  ## ## proptry
  ## if (is.null(proptry)) { ## asdf
  ##   proptry <- 0 ## asdf
  ## } else if (!is.numeric(proptry) || !((proptry > 0) & (proptry <= 1))) { ## asdf
  ##   stop("Error: Invalid value for proptry") ## asdf
  ## }
  
  ## Seed
  if (is.null(seed)) {
    seed <- runif(1 , 0, .Machine$integer.max)
  }
  
  ## Keep inbag
  if (!is.logical(keep.inbag)) {
    stop("Error: Invalid value for keep.inbag")
  }
  
  ## Num threads
  ## Default 0 -> detect from system in C++.
  if (is.null(num.threads)) {
    num.threads = 0
  } else if (!is.numeric(num.threads) || num.threads < 0) {
    stop("Error: Invalid value for num.threads")
  }
  
  ## Minumum node size
  if (is.null(min.node.size)) {
    min.node.size <- 0
  } else if (!is.numeric(min.node.size) || min.node.size < 0) {
    stop("Error: Invalid value for min.node.size")
  }
  
  ## Tree depth
  if (is.null(max.depth)) {
    max.depth <- 0
  } else if (!is.numeric(max.depth) || max.depth < 0) {
    stop("Error: Invalid value for max.depth. Please give a positive integer.")
  }
  
  ## Sample fraction
  if (!is.numeric(sample.fraction)) {
    stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
  }
  if (length(sample.fraction) > 1) {
    if (!(treetype %in% c(1, 9))) {
      stop("Error: Invalid value for sample.fraction. Vector values only valid for classification forests.")
    }
    if (any(sample.fraction < 0) || any(sample.fraction > 1)) {
      stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
    }
    if (sum(sample.fraction) <= 0) {
      stop("Error: Invalid value for sample.fraction. Sum of values must be >0.")
    }
    if (length(sample.fraction) != nlevels(response)) {
      stop("Error: Invalid value for sample.fraction. Expecting ", nlevels(response), " values, provided ", length(sample.fraction), ".")
    }
    if (!replace & any(sample.fraction * length(response) > table(response))) {
      idx <- which(sample.fraction * length(response) > table(response))[1]
      stop("Error: Not enough samples in class ", names(idx), 
           "; available: ", table(response)[idx], 
           ", requested: ", (sample.fraction * length(response))[idx], ".")
    }
    if (!is.null(case.weights)) {
      stop("Error: Combination of case.weights and class-wise sampling not supported.")
    }
  } else {
    if (sample.fraction <= 0 || sample.fraction > 1) {
      stop("Error: Invalid value for sample.fraction. Please give a value in (0,1] or a vector of values in [0,1].")
    }
  }
  
  ## Importance mode
  if (is.null(importance) || importance == "none") {
    importance.mode <- 0
    eim.mode <- 0
  } else if (importance %in% c("pooled", "both", "qualitative", "quantitative", "mainonly")) {
    importance.mode <- 3
    if (importance == "pooled") {
      eim.mode <- 1
    }
    if (importance == "both") {
      eim.mode <- 2
    }
    if (importance == "qualitative") {
      eim.mode <- 3
    }
    if (importance == "quantitative") {
      eim.mode <- 4
    }
    if (importance == "mainonly") {
      eim.mode <- 5
    }
  } else {
    stop("Error: Unknown importance mode.")
  }
  
  
  ## Case weights: NULL for no weights
  if (is.null(case.weights)) {
    case.weights <- c(0,0)
    use.case.weights <- FALSE
    if (holdout) {
      stop("Error: Case weights required to use holdout mode.")
    }
  } else {
    use.case.weights <- TRUE
    
    ## Sample from non-zero weights in holdout mode
    if (holdout) {
      sample.fraction <- sample.fraction * mean(case.weights > 0)
    }
    
    if (!replace && sum(case.weights > 0) < sample.fraction * nrow(data.final)) {
      stop("Error: Fewer non-zero case weights than observations to sample.")
    }
  }
  
  ## Manual inbag selection
  if (is.null(inbag)) {
    inbag <- list(c(0,0))
    use.inbag <- FALSE
  } else if (is.list(inbag)) {
    use.inbag <- TRUE
    if (use.case.weights) {
      stop("Error: Combination of case.weights and inbag not supported.")
    }
    if (length(sample.fraction) > 1) {
      stop("Error: Combination of class-wise sampling and inbag not supported.")
    }
    if (length(inbag) != num.trees) {
      stop("Error: Size of inbag list not equal to number of trees.")
    }
  } else {
    stop("Error: Invalid inbag, expects list of vectors of size num.trees.")
  }
  
  ## Class weights: NULL for no weights (all 1)
  if (is.null(class.weights)) {
    class.weights <- rep(1, nlevels(response))
  } else {
    if (!(treetype %in% c(1, 9))) {
      stop("Error: Argument class.weights only valid for classification forests.")
    }
    if (!is.numeric(class.weights) || any(class.weights < 0)) {
      stop("Error: Invalid value for class.weights. Please give a vector of non-negative values.")
    }
    if (length(class.weights) != nlevels(response)) {
      stop("Error: Number of class weights not equal to number of classes.")
    }
    
    ## Reorder (C++ expects order as appearing in the data)
    class.weights <- class.weights[unique(as.numeric(response))]
  }
  
  ## Always split variables: NULL for no variables
  if (is.null(always.split.variables)) {
    always.split.variables <- c("0", "0")
    use.always.split.variables <- FALSE
  } else {
    use.always.split.variables <- TRUE
  }
  
  ## Currently, for diversity forests, only the default 
  ## splitting rules are supported:  ## asdf
  if (!is.null(splitrule)) {
    if (treetype == 5 && splitrule != "logrank") {
      splitrule <- "logrank"
      warning("'splitrule' changed to 'logrank' as currently only this option is available for diversity forests for survival.")
    } else if (treetype == 3 && splitrule != "variance") {
      splitrule <- "variance"
      warning("'splitrule' changed to 'variance' as currently only this option is available for diversity forests for regression.")
    } else if (treetype %in% c(1, 9) && splitrule != "gini") {
      splitrule <- "gini"
      warning("'splitrule' changed to 'gini' as currently only this option is available for diversity forests for classification.")
    }
  }
  
  ## Splitting rule
  if (is.null(splitrule)) {
    if (treetype == 5) {
      splitrule <- "logrank"
    } else if (treetype == 3) {
      splitrule <- "variance"
    } else if (treetype %in% c(1, 9)) {
      splitrule <- "gini"
    }
    splitrule.num <- 1
  } else if (splitrule == "logrank") {
    if (treetype == 5) {
      splitrule.num <- 1
    } else {
      stop("Error: logrank splitrule applicable to survival data only.")
    }
  } else if (splitrule == "gini") {
    if (treetype %in% c(1, 9)) {
      splitrule.num <- 1
    } else {
      stop("Error: Gini splitrule applicable to classification data only.")
    }
  } else if (splitrule == "variance") {
    if (treetype == 3) {
      splitrule.num <- 1
    } else {
      stop("Error: variance splitrule applicable to regression data only.")
    }
  } else if (splitrule == "auc" || splitrule == "C") {
    if (treetype == 5) {
      splitrule.num <- 2
    } else {
      stop("Error: C index splitrule applicable to survival data only.")
    }
  } else if (splitrule == "auc_ignore_ties" || splitrule == "C_ignore_ties") {
    if (treetype == 5) {
      splitrule.num <- 3
    } else {
      stop("Error: C index splitrule applicable to survival data only.")
    }
  } else if (splitrule == "maxstat") {
    if (treetype == 5 || treetype == 3) {
      splitrule.num <- 4
    } else {
      stop("Error: maxstat splitrule applicable to regression or survival data only.")
    }
  } else if (splitrule == "extratrees") {
    splitrule.num <- 5
  } else {
    stop("Error: Unknown splitrule.")
  }
  
  if (splitrule.num == 5 && save.memory && respect.unordered.factors == "partition") {
    stop("Error: save.memory option not possible in extraTrees mode with unordered predictors.")
  }
  
  ## Unordered factors  
  if (respect.unordered.factors == "partition") {
    names.selected <- names(data.selected)
    ordered.idx <- sapply(data.selected, is.ordered)
    factor.idx <- sapply(data.selected, is.factor)
    independent.idx <- names.selected != dependent.variable.name & names.selected != status.variable.name
    unordered.factor.variables <- names.selected[factor.idx & !ordered.idx & independent.idx]
    
    if (length(unordered.factor.variables) > 0) {
      use.unordered.factor.variables <- TRUE
      ## Check level count
      num.levels <- sapply(data.selected[, factor.idx & !ordered.idx & independent.idx, drop = FALSE], nlevels)
      max.level.count <- .Machine$double.digits
      if (max(num.levels) > max.level.count) {
        stop(paste("Too many levels in unordered categorical variable ", unordered.factor.variables[which.max(num.levels)], 
                   ". Only ", max.level.count, " levels allowed on this system. Consider using the 'order' option.", sep = ""))
      } 
    } else {
      unordered.factor.variables <- c("0", "0")
      use.unordered.factor.variables <- FALSE
    } 
  } else if (respect.unordered.factors == "ignore" || respect.unordered.factors == "order") {
    ## Ordering for "order" is handled above
    unordered.factor.variables <- c("0", "0")
    use.unordered.factor.variables <- FALSE
  } else {
    stop("Error: Invalid value for respect.unordered.factors, please use 'order', 'partition' or 'ignore'.")
  }
  
  ## Unordered maxstat splitting not possible
  if (use.unordered.factor.variables && !is.null(splitrule)) {
    if (splitrule == "maxstat") {
      stop("Error: Unordered factor splitting not implemented for 'maxstat' splitting rule.")
    } else if (splitrule %in% c("C", "auc", "C_ignore_ties", "auc_ignore_ties")) {
      stop("Error: Unordered factor splitting not implemented for 'C' splitting rule.")
    }
  }
  
  ## Warning for experimental 'order' splitting 
  if (respect.unordered.factors == "order") {
    if (treetype == 3 && splitrule == "maxstat") {
      warning("Warning: The 'order' mode for unordered factor handling with the 'maxstat' splitrule is experimental.")
    }
    if (gwa.mode & ((treetype %in% c(1,9) & nlevels(response) > 2) | treetype == 5)) {
      stop("Error: Ordering of SNPs currently only implemented for regression and binary outcomes.")
    }
  }
  
  
  ## Prediction mode always false. Use predict.divfor() method.
  prediction.mode <- FALSE
  predict.all <- FALSE
  prediction.type <- 1
  
  ## No loaded forest object
  loaded.forest <- list()
  
  ## Use sparse matrix
  if ("dgCMatrix" %in% class(data.final)) {
    sparse.data <- data.final
    data.final <- matrix(c(0, 0))
    use.sparse.data <- TRUE
  } else {
    sparse.data <- Matrix(matrix(c(0, 0)))
    use.sparse.data <- FALSE
  }
  
  if (respect.unordered.factors == "order"){
    order.snps <- TRUE
  } else {
    order.snps <- FALSE
  }
  
  
  ## Clean up
  rm("data.selected")
  
  ## Call divfor
  
  if (treetype == 5) {
    X <- data.final[data.final[,status.variable.name]==1, !(colnames(data.final) %in% c(dependent.variable.name, status.variable.name))]
    y <- data.final[data.final[,status.variable.name]==1, colnames(data.final) == dependent.variable.name]
  } else {
    X <- data.final[, colnames(data.final) != dependent.variable.name]
    y <- data.final[, colnames(data.final) == dependent.variable.name]
    if (treetype %in% c(1,9) & length(unique(y)) > 2) {
      largestclasses <- as.numeric(names(sort(table(y), decreasing=TRUE))[1:2])
      inclind <- y %in% largestclasses
      X <- X[inclind,]
      y <- y[inclind]
      inds1 <- y==largestclasses[1]
      inds2 <- y==largestclasses[2]
      y[inds1] <- 1
      y[inds2] <- 2
    }
  }
  
  ###cat("Selecting promising pairs...", "\n")
  ###start_time <- Sys.time()
  promispairs <- getPromispairs(X, y)
  ###end_time <- Sys.time()
  ###timetaken <- as.numeric(difftime(end_time, start_time, units="secs"))
  ###cat(paste("Finished selecting promising pairs. Time taken: ", timetaken, " seconds.", sep=""), "\n")
  promispairs <- split(t(promispairs), rep(1:nrow(promispairs), each = ncol(promispairs)))
  
  if (importance != "none" & simplify.large.n & maxdepthnull) {
    if (nrow(data) <= 1000)
      max.depth.eim <- max.depth
    if (nrow(data) > 1000 & nrow(data) <= 2000)
      max.depth.eim <- 10
    if (nrow(data) > 2000 & nrow(data) <= 5000)
      max.depth.eim <- 7
    if (nrow(data) > 5000)
      max.depth.eim <- 5
  }
  else
    max.depth.eim <- max.depth
  
  split.select.weights <- list(c(0,0))
  use.split.select.weights <- FALSE
  num.random.splits <- 1
  
  
  ## Determine the right default values for
  ## the number(s) of trees:
  if (simplify.large.n & nrow(data) > 1000) {
    if (is.null(num.trees))
      num.trees <- 2000
    else {
      if (is.null(num.trees.eim.large.n))
        num.trees.eim.large.n <- num.trees
    }
    if (is.null(num.trees.eim.large.n)) {
      if(importance != "none")
        num.trees.eim.large.n <- 20000
      else
        num.trees.eim.large.n <- 2000
    }
  }
  else {
    # If no restricted tree depth are used (i.e., if simplify.large.n=FALSE or
    # nrow(data) <= 1000) we construct only one forest and the number of trees
    # for that forest is called num.trees.eim.large.n:
    if (is.null(num.trees)) {
      if(importance != "none")
        num.trees.eim.large.n <- 20000
      else
        num.trees.eim.large.n <- 2000
    }
    else
      num.trees.eim.large.n <- num.trees
  }
  
  result <- divforCpp(treetype, dependent.variable.name, data.final, variable.names, mtry=0,
                      num.trees.eim.large.n, verbose, seed, num.threads, write.forest, importance.mode,
                      min.node.size, split.select.weights, use.split.select.weights,
                      always.split.variables, use.always.split.variables,
                      status.variable.name, prediction.mode, loaded.forest, snp.data,
                      replace, probability, unordered.factor.variables, use.unordered.factor.variables, 
                      save.memory, splitrule.num, case.weights, use.case.weights, class.weights, 
                      predict.all, keep.inbag, sample.fraction, alpha=0.5, minprop=0.1, holdout, prediction.type, 
                      num.random.splits, sparse.data, use.sparse.data, order.snps, oob.error, max.depth.eim, 
                      inbag, use.inbag, nsplits=30, npairs, proptry=1, divfortype=2, promispairs, eim.mode)
  
  if (importance != "none" & simplify.large.n & maxdepthnull & nrow(data) > 1000) {
    importance.mode.forest <- 0
    resultforest <- divforCpp(treetype, dependent.variable.name, data.final, variable.names, mtry=0,
                              num.trees, verbose, seed, num.threads, write.forest, importance.mode.forest,
                        min.node.size, split.select.weights, use.split.select.weights,
                        always.split.variables, use.always.split.variables,
                        status.variable.name, prediction.mode, loaded.forest, snp.data,
                        replace, probability, unordered.factor.variables, use.unordered.factor.variables, 
                        save.memory, splitrule.num, case.weights, use.case.weights, class.weights, 
                        predict.all, keep.inbag, sample.fraction, alpha=0.5, minprop=0.1, holdout, prediction.type, 
                        num.random.splits, sparse.data, use.sparse.data, order.snps, oob.error, max.depth, 
                        inbag, use.inbag, nsplits=30, npairs, proptry=1, divfortype=2, promispairs, eim.mode)
    result$forest <- resultforest$forest
    result$num.trees <- resultforest$num.trees
  }
    
  if (length(result) == 0) {
    stop("User interrupt or internal error.")
  }
  
  plotres <- list()
  count <- 1
  
  ## Prepare results
  if (importance.mode != 0) {
    names(result$eim.univ) <- all.independent.variable.names
    
    eim.univ.order <- order(result$eim.univ, decreasing=TRUE)
    plotres[[count]] <- eim.univ.order
    names(plotres)[count] <- "eim.univ.order"
    count <- count + 1
    
    result$eim.univ.sorted <- result$eim.univ[eim.univ.order]
    if (importance == "pooled") {
      namesbiv <- sapply(promispairs, function(x) paste(all.independent.variable.names[x+1], collapse=" AND "))
      result$eim.pooled <- result$eim.bivpooled
      names(result$eim.pooled) <- namesbiv
      result$eim.bivpooled <- NULL
      
      eim.pooled.order <- order(result$eim.pooled, decreasing=TRUE)
      plotres[[count]] <- eim.pooled.order
      names(plotres)[count] <- "eim.pooled.order"
      count <- count + 1
      
      result$eim.pooled.sorted <- result$eim.pooled[eim.pooled.order]
    }
    if (importance == "both" || importance == "qualitative") {
      namesbiv <- sapply(promispairs, function(x) paste(all.independent.variable.names[x+1], collapse=" AND "))
      result$eim.qual <- result$eim.bivqual
      result$eim.bivqual <- NULL
      
      names(result$eim.qual) <- namesbiv
      
      vars1 <- all.independent.variable.names[sapply(promispairs, function(x) x[1]+1)]
      vars2 <- all.independent.variable.names[sapply(promispairs, function(x) x[2]+1)]
      
      covcat <- which(sapply(covariate.levels, function(x) length(x) > 0))
      
      if(length(covcat) > 0) {
        
        factorvars <- all.independent.variable.names[covcat]
        
        for(i in seq(along=factorvars)) {
          names(result$eim.qual)[vars1==factorvars[i] & !(vars2 %in% factorvars)] <- paste(names(result$eim.qual)[vars1==factorvars[i] & !(vars2 %in% factorvars)],
                                                                                           " (", factorvars[i], ": ", paste(covariate.levels[covcat][[i]], collapse=" < "), ")", sep="")
          names(result$eim.qual)[vars2==factorvars[i] & !(vars1 %in% factorvars)] <- paste(names(result$eim.qual)[vars2==factorvars[i] & !(vars1 %in% factorvars)],
                                                                                           " (", factorvars[i], ": ", paste(covariate.levels[covcat][[i]], collapse=" < "), ")", sep="")
        }
        if(length(covcat) > 1) {
          
          for(i in 1:(length(covcat)-1)) {
            for(j in (i+1):length(covcat)) {
              names(result$eim.qual)[vars1==factorvars[i] & vars2==factorvars[j]] <- paste(names(result$eim.qual)[vars1==factorvars[i] & vars2==factorvars[j]],
                                                                                           " (", factorvars[i], ": ", paste(covariate.levels[covcat][[i]], collapse=" < "), ", ", 
                                                                                           factorvars[j], ": ", paste(covariate.levels[covcat][[j]], collapse=" < "), ")", sep="")
            }
          }
          
        }
        
      }
      
      eim.qual.order <- order(result$eim.qual, decreasing=TRUE)
      plotres[[count]] <- eim.qual.order
      names(plotres)[count] <- "eim.qual.order"
      count <- count + 1
      
      result$eim.qual.sorted <- result$eim.qual[eim.qual.order]
    }
    if (importance == "both" || importance == "quantitative") {
      
      namesbiv <- sapply(promispairs, function(x) paste(all.independent.variable.names[x+1], collapse=" AND "))
      
      result$eim.quant.small.small <- result$eim.bivquant.ll - ifelse(result$eim.bivquant.hh > 0, result$eim.bivquant.hh, 0)
      result$eim.quant.large.large <- result$eim.bivquant.hh - ifelse(result$eim.bivquant.ll > 0, result$eim.bivquant.ll, 0)
      result$eim.quant.small.large <- result$eim.bivquant.lh - ifelse(result$eim.bivquant.hl > 0, result$eim.bivquant.hl, 0)
      result$eim.quant.large.small <- result$eim.bivquant.hl - ifelse(result$eim.bivquant.lh > 0, result$eim.bivquant.lh, 0)
      
      result$eim.quant.small.small.raw <- result$eim.bivquant.ll
      result$eim.quant.large.large.raw <- result$eim.bivquant.hh
      result$eim.quant.small.large.raw <- result$eim.bivquant.lh
      result$eim.quant.large.small.raw <- result$eim.bivquant.hl
      result$eim.bivquant.ll <- NULL
      result$eim.bivquant.hh <- NULL
      result$eim.bivquant.lh <- NULL
      result$eim.bivquant.hl <- NULL
      
      names(result$eim.quant.small.small) <- namesbiv
      names(result$eim.quant.large.large) <- namesbiv
      names(result$eim.quant.small.large) <- namesbiv
      names(result$eim.quant.large.small) <- namesbiv
      
      names(result$eim.quant.small.small.raw) <- namesbiv
      names(result$eim.quant.large.large.raw) <- namesbiv
      names(result$eim.quant.small.large.raw) <- namesbiv
      names(result$eim.quant.large.small.raw) <- namesbiv
      
      bivquants <- cbind(result$eim.quant.small.small, result$eim.quant.large.large, result$eim.quant.small.large, result$eim.quant.large.small)
      bivquanttype <- apply(bivquants, 1, nnet::which.is.max)
      result$eim.quant <- mapply(function(x, y) x[y], x = data.frame(t(bivquants)), y = bivquanttype)
      
      vars1 <- all.independent.variable.names[sapply(promispairs, function(x) x[1]+1)]
      vars2 <- all.independent.variable.names[sapply(promispairs, function(x) x[2]+1)]
      
      names(result$eim.quant)[bivquanttype==1] <- paste(vars1[bivquanttype==1], " small AND ", 
                                                        vars2[bivquanttype==1], " small", sep="")
      names(result$eim.quant)[bivquanttype==2] <- paste(vars1[bivquanttype==2], " large AND ", 
                                                        vars2[bivquanttype==2], " large", sep="")
      names(result$eim.quant)[bivquanttype==3] <- paste(vars1[bivquanttype==3], " small AND ", 
                                                        vars2[bivquanttype==3], " large", sep="")
      names(result$eim.quant)[bivquanttype==4] <- paste(vars1[bivquanttype==4], " large AND ", 
                                                        vars2[bivquanttype==4], " small", sep="")
      
      covcat <- which(sapply(covariate.levels, function(x) length(x) > 0))
      
      if(length(covcat) > 0) {
        
        factorvars <- all.independent.variable.names[covcat]
        
        for(i in seq(along=factorvars)) {
          names(result$eim.quant)[vars1==factorvars[i] & !(vars2 %in% factorvars)] <- paste(names(result$eim.quant)[vars1==factorvars[i] & !(vars2 %in% factorvars)],
                                                                                            " (", factorvars[i], ": ", paste(covariate.levels[covcat][[i]], collapse=" < "), ")", sep="")
          names(result$eim.quant)[vars2==factorvars[i] & !(vars1 %in% factorvars)] <- paste(names(result$eim.quant)[vars2==factorvars[i] & !(vars1 %in% factorvars)],
                                                                                            " (", factorvars[i], ": ", paste(covariate.levels[covcat][[i]], collapse=" < "), ")", sep="")
        }
        if(length(covcat) > 1) {
          
          for(i in 1:(length(covcat)-1)) {
            for(j in (i+1):length(covcat)) {
              names(result$eim.quant)[vars1==factorvars[i] & vars2==factorvars[j]] <- paste(names(result$eim.quant)[vars1==factorvars[i] & vars2==factorvars[j]],
                                                                                            " (", factorvars[i], ": ", paste(covariate.levels[covcat][[i]], collapse=" < "), ", ", 
                                                                                            factorvars[j], ": ", paste(covariate.levels[covcat][[j]], collapse=" < "), ")", sep="")
            }
          }
          
        }
        
      }
      
      eim.quant.order <- order(result$eim.quant, decreasing=TRUE)
      plotres[[count]] <- eim.quant.order
      names(plotres)[count] <- "eim.quant.order"
      count <- count + 1
      
      result$eim.quant.sorted <- result$eim.quant[eim.quant.order]
    }
  }
  
  ## Set predictions
  if (treetype == 1 && is.factor(response) && oob.error) {
    result$predictions <- integer.to.factor(result$predictions,
                                            levels(response))
    true.values <- integer.to.factor(unlist(data.final[, dependent.variable.name]),
                                     levels(response))
    result$confusion.matrix <- table(true.values, result$predictions, 
                                     dnn = c("true", "predicted"), useNA = "ifany")
  } else if (treetype == 5 && oob.error) {
    if (is.list(result$predictions)) {
      result$predictions <- do.call(rbind, result$predictions)
    } 
    if (is.vector(result$predictions)) {
      result$predictions <- matrix(result$predictions, nrow = 1)
    }
    result$chf <- result$predictions
    result$predictions <- NULL
    result$survival <- exp(-result$chf)
  } else if (treetype == 9 && !is.matrix(data) && oob.error) {
    if (is.list(result$predictions)) {
      result$predictions <- do.call(rbind, result$predictions)
    } 
    if (is.vector(result$predictions)) {
      result$predictions <- matrix(result$predictions, nrow = 1)
    }
    
    ## Set colnames and sort by levels
    colnames(result$predictions) <- unique(response)
    if (is.factor(response)) {
      result$predictions <- result$predictions[, levels(droplevels(response)), drop = FALSE]
    }
  }
  
  ## Splitrule
  result$splitrule <- splitrule
  
  ## If the number of covariate variables is larger than 100, we pre-select
  ## the 5000 variable pairs with the strongest indications of interaction effects.
  ## However, this cannot be taken into account in the out-of-bag error estimation, which
  ## is why the out-of-bag error estimate is too optimistic if the number of
  ## covariate variables is larger than 100. Therefore, we set the
  ## out-of-bag error 'prediction.error' to NA, if the number of variables
  ## is larger than 1000:
  if (result$num.independent.variables > 100) {
    result$prediction.error <- NA
  }
  
  ## Set treetype
  if (treetype == 1) {
    result$treetype <- "Classification"
  } else if (treetype == 3) {
    result$treetype <- "Regression"
  } else if (treetype == 5) {
    result$treetype <- "Survival"
  } else if (treetype == 9) {
    result$treetype <- "Probability estimation"
  }
  if (treetype == 3) {
    result$r.squared <- 1 - result$prediction.error / var(response)
  }
  result$call <- sys.call()
  result$importance.mode <- importance
  result$num.samples <- nrow(data.final)
  result$replace <- replace
  
  ## Write forest object
  if (write.forest) {
    if (is.factor(response)) {
      result$forest$levels <- levels(response)
    }
    result$forest$independent.variable.names <- independent.variable.names
    result$forest$treetype <- result$treetype
    class(result$forest) <- "interactionfor.forest"
    
    ## In 'ordered' mode, save covariate levels
    if (respect.unordered.factors == "order" && !is.matrix(data)) {
      result$forest$covariate.levels <- covariate.levels
    }
  }
  
  # ## Prepare quantile prediction
  # if (quantreg) {
  #   terminal.nodes <- predict(result, data, type = "terminalNodes")$predictions + 1
  #   n <- result$num.samples
  #   result$random.node.values <- matrix(nrow = max(terminal.nodes), ncol = num.trees)
  #   
  #   ## Select one random obs per node and tree
  #   for (tree in 1:num.trees){
  #     idx <- sample(1:n, n)
  #     result$random.node.values[terminal.nodes[idx, tree], tree] <- response[idx]
  #   }
  #   
  #   ## Prepare out-of-bag quantile regression
  #   if(!is.null(result$inbag.counts)) {
  #     inbag.counts <- simplify2array(result$inbag.counts)
  #     random.node.values.oob <- 0 * terminal.nodes
  #     random.node.values.oob[inbag.counts > 0] <- NA
  #     
  #     ## For each tree and observation select one random obs in the same node (not the same obs)
  #     for (tree in 1:num.trees){
  #       is.oob <- inbag.counts[, tree] == 0
  #       num.oob <- sum(is.oob)
  #       
  #       if (num.oob != 0) {
  #         oob.obs <- which(is.oob)
  #         oob.nodes <- terminal.nodes[oob.obs, tree]
  #         for (j in 1:num.oob) {
  #           idx <- terminal.nodes[, tree] == oob.nodes[j]
  #           idx[oob.obs[j]] <- FALSE
  #           random.node.values.oob[oob.obs[j], tree] <- save.sample(response[idx], size = 1)
  #         }
  #       }
  #     }
  #     
  #     ## Check num.trees
  #     minoob <- min(rowSums(inbag.counts == 0))
  #     if (minoob < 10) {
  #       stop("Error: Too few trees for out-of-bag quantile regression.")
  #     }
  #     
  #     ## Use the same number of values for all obs, select randomly
  #     result$random.node.values.oob <- t(apply(random.node.values.oob, 1, function(x) {
  #       sample(x[!is.na(x)], minoob)
  #     }))
  #   }
  # }
  
  result$mtry <- result$nsplits <- NULL
  
  if (importance %in% c("both", "quantitative")) {
    
    result$eim.quant.rawlists <- list(result$eim.quant.small.small, result$eim.quant.small.large, result$eim.quant.large.small, 
                                      result$eim.quant.large.large, result$eim.quant.small.small.raw, result$eim.quant.small.large.raw,
                                      result$eim.quant.large.small.raw, result$eim.quant.large.large.raw)
    names(result$eim.quant.rawlists) <- c("eim.quant.small.small", "eim.quant.small.large", 
                                          "eim.quant.large.small", "eim.quant.large.large", "eim.quant.small.small.raw", 
                                          "eim.quant.small.large.raw", "eim.quant.large.small.raw", "eim.quant.large.large.raw")
    
    result$eim.quant.small.small <- result$eim.quant.small.large <- result$eim.quant.large.small <- 
      result$eim.quant.large.large <- result$eim.quant.small.small.raw <- result$eim.quant.small.large.raw <- 
      result$eim.quant.large.small.raw <- result$eim.quant.large.large.raw <- NULL
    
  }
  
  if (importance == "both") {
    indsubset <- c(which(names(result)=="eim.univ.sorted"), which(names(result)=="eim.univ"),
                   which(names(result)=="eim.qual.sorted"), which(names(result)=="eim.qual"), which(names(result)=="eim.quant.sorted"), which(names(result)=="eim.quant"))
    result <- result[c(1:which(names(result)=="proptry"), indsubset, setdiff((which(names(result)=="proptry")+1):length(result), indsubset))]
  }
  if (importance == "qualitative") {
    indsubset <- c(which(names(result)=="eim.univ.sorted"), which(names(result)=="eim.univ"),
                   which(names(result)=="eim.qual.sorted"), which(names(result)=="eim.qual"))
    result <- result[c(1:which(names(result)=="proptry"), indsubset, setdiff((which(names(result)=="proptry")+1):length(result), indsubset))]
  }
  if (importance == "quantitative") {
    indsubset <- c(which(names(result)=="eim.univ.sorted"), which(names(result)=="eim.univ"),
                   which(names(result)=="eim.quant.sorted"), which(names(result)=="eim.quant"))
    result <- result[c(1:which(names(result)=="proptry"), indsubset, setdiff((which(names(result)=="proptry")+1):length(result), indsubset))]
  }
  if (importance == "pooled") {
    indsubset <- c(which(names(result)=="eim.univ.sorted"), which(names(result)=="eim.univ"),
                   which(names(result)=="eim.pooled.sorted"), which(names(result)=="eim.pooled"))
    result <- result[c(1:which(names(result)=="eim.univ"), indsubset, setdiff((which(names(result)=="eim.univ")+1):length(result), indsubset))]
  }
  
  plotres$data <- data
  plotres$yvarname <- dependent.variable.name
  if(result$treetype=="Survival") {
    plotres$yvarname <- timename
    plotres$statusvarname <- statusname
    
    names(plotres$data)[names(plotres$data)=="time"] <- timename
    names(plotres$data)[names(plotres$data)=="status"] <- statusname
    
  } else {
    plotres$yvarname <- dependent.variable.name
  }
  
  result$promispairs <- lapply(promispairs, function(x) x+1)
  result$plotres <- plotres
  
  result$proptry <- NULL
  
  class(result) <- "interactionfor"
  
  return(result)
  
}
